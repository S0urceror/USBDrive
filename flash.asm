BDOS	equ	0005h   ; Set the address 0005h into label BDOS.
		                ; We can call several routines under MSX-DOS at address 0005h.
ENASLT	equ	0024h
RSLREG  equ 0138h
EXPTBL	equ	0FCC1h
map_40_Konami	equ	$5000
map_40          equ $6000
map_60_Konami	equ	$7000

    org 0100h
    di
    ; find & select flash slot in 0x4000
    call FINDSELECTFLASH
    jp z,_NOT_FOUND
    ;ld hl, TXT_SLOTID
    ;call PRINT
    ;call PRINTHEX
    ;ld hl,TXT_CRLF
    ;call PRINT
    jr _CONTINUE
_NOT_FOUND:
    ld hl, TXT_FLASH_NOT
    call PRINT
    ei
    ret
_CONTINUE:
    ;ld hl, TXT_DEVICEID
    ;call PRINT
    ;ld a, (deviceID)
    ;call PRINTHEX
    ;ld hl,TXT_CRLF
    ;call PRINT
    
    ; set page 1-1 and 1-2
    xor		a
	ld		(map_40_Konami),a
    inc     a
    ld		(map_60_Konami),a
    
    ; erase sector 0
    ld a, #aa
    ld (#4555),a
    ld a, #55
    ld (#42aa),a
    ld a, #80
    ld (#4555),a
    ld a, #aa
    ld (#4555),a
    ld a, #55
    ld (#42aa),a
    ;ld a, #10; chip erase
    ld a, #30; sector erase
    ld (#4555),a

    ; check if page 1-1 and 1-2 are erased
	ld	de,#4000
_check_next:    
    ld	a,#ff ; check if all bits are high after erase
	call Check
    jr nc, _next0
    ld hl, TXT_ERROR_ERASE
    call PRINT
    ld a, d
    call PRINTHEX
    ld a, e
    call PRINTHEX
    ld hl, TXT_CRLF
    call PRINT
    ei
    ret
_next0:
    inc de
    ; already at #8000h?
    ld a, #80
    cp a, d
    jr z, _next1
    jr _check_next
_next1:
    ;ld hl, TXT_OK_ERASE
    ;call PRINT
    ;ld hl, #4000
    ;ld bc, #0100
    ;call PRINTHEX_BUFFER

    ; flash page 0
    exx
    ld	a,(actualpage)
    ld	hl,map_40_Konami
    ld	(hl),a			; Selects page
    exx
    ld	hl,08000h		; from
    ld	de,04000h		; to
    ld	bc,02000h
writeflash0:
    ld	a,0AAh
    ld	(04555h),a
    ld	a,055h
pf4:    ld	(042AAh),a
    ld	a,0A0h
    ld	(04555h),a		; 4 cycles command PROGRAM     

    ;ld	a,(hl)
    ld a, e ; we're now writing repeating 0-255 values, no copying
    ld	(de),a

    ld	a,(actualpage)
    exx
    ld	(hl),a			; Restores actual page (needed for SCC-Flash)
    exx

    ;ld	a,(hl)
    ld a, e ; we're now writing repeating 0-255 values, no copying
    call	Check
    jp	c,writeflash2		; ERROR. C = 1

    inc	hl			; Ok. Next byte.
    inc	de
    dec	bc
    ld	a,b
    or	c
    jp	nz,writeflash0		; All bytes programmed? Return for making next block.

writeflash2:
    jr nc, _write_flash_ok
    ld hl, TXT_ERROR_WRITE
    call PRINT
    ld a, d
    call PRINTHEX
    ld a, e
    call PRINTHEX
    ld hl, TXT_CRLF
    call PRINT
    ei 
    ret
_write_flash_ok:
    ;ld hl, TXT_OK_WRITE
    ;call PRINT
    ; read page 0
    ;ld hl, #4000
    ;ld bc, #0100
    ;call PRINTHEX_BUFFER

    ; restore RAM for 0x4000
    call Set_RAM40

    ei
    ret

actualpage: DB 0
TXT_SLOTID DB "SLOT ID: ",0
TXT_DEVICEID DB "FLASH ID: ",0
TXT_CRLF DB "\r\n",0
TXT_FLASH_NOT DB "FLASH NOT FOUND\r\n",0
TXT_ERROR_ERASE DB "ERASE OPERATION FAILED: ",0
TXT_OK_ERASE DB "ERASE COMPLETE\r\n",0
TXT_ERROR_WRITE DB "WRITE OPERATION FAILED: ",0
TXT_OK_WRITE DB "WRITE COMPLETE\r\n",0

Set_Flash40:
	ld	a,(THISSLT)
	ld	h,$40
	jp	ENASLT
Set_Flash80:
	ld	a,(THISSLT)
	ld	h,$80
	jp	ENASLT
Set_RAM40:
	ld	a,($f342)
	ld	h,$40
	jp	ENASLT
Set_RAM80:
	ld	a,($f343)
	ld	h,$80
	jp	ENASLT

Check:
		push	bc
		ld	c,a
Check1:
		ld	a,(de)
		xor	c
		jp	p,Check2 ; expected data read back, if not then we're not ready
		xor	c
		and	$20 ; check bit 5
		jr	z,Check1 ; check again
		ld	a,(de)
		xor	c
		jp	p,Check2 ; now ok?
        ; not ok, operation took too long
        ld	a,0f0h
		ld	(de),a ; reset pending operation
		scf
Check2:
		pop	bc
		ret

; returns the ENASLT variable pointing to the current primary/secundary slot for 0x4000
GET40SLTS:
    in	a,(0A8h)	;Read primary slots register
    and 00001100b
    rra 
    rra 
    ;rra 
    ;rra 
    ld b,a ; moved here 000000PP
	ld a, (0x0ffff) ;Read secondary slots configuration
    cpl 
    and 00001100b
    jr z, _NEXT; no secondary slot for region 8000h-bfffh
    ;rra 
    ;rra
    set 7,a ; moved here 1000EE00
_NEXT:
    or b
    ret

FINDSELECTFLASH:
checkflash0:
		call	sigslot		; Calls the next slot (first one if first time)
		cp	0FFh			; Is it the last slot?
		jp	z,checkflashnot	; Yes. FLASH was not found

		ld	h,040h		; It is not the last slot. Placed it in page 1
		call	ENASLT

        xor	a
		ld	(map_40),a	; Prevents SRAM selection

        call checkdeviceid	; Searching flash by esecuting its ID_CHECK command
		jr	nz,checkflash0	; Not found in this slot, continue with next one

        ld	a,(THISSLT) 	; FLASH WAS FOUND 
        or a
checkflashnot:
        ret

; ------------------------
; CHECKDEVICEID
; Check Flash Device ID 
; Am29F040B = A4
; M29F800AB = 58
; Z = 1 if Flash Rom found
; -------------------------
checkdeviceid:
		ld	hl,04001h
		ld	(hl),0F0h	; Send Reset Command.

		ld	de,04555h
        ld	bc,042AAh
		ld	a,0AAh
		ld	(de),a
		ld	a,055h
		ld	(bc),a
		ld	a,090h
		ld	(de),a		; Send 4 cycles command DEVICE ID

		ld	a,(hl)
		ld	(hl),0F0h	; Reset Command
		ld	(deviceID),a
		cp	#a4
		ret	z
		cp	#58
		ret

deviceID:	db	0

;       Subroutine      Print text with the right DOS/BIOS routine
;       Inputs          HL - pointer to text to print
;       Outputs         -------------------------------
PRINT:
 ;   ld de, hl
 ;   ld c, 09h
 ;   call BDOS
 ;   ret

    push af
    push hl
_PRINT_MORE:
    ld a,(hl)
    and a
    jr z, _PRINT_DONE
    call PRINTCHAR
    inc hl
    jr _PRINT_MORE
_PRINT_DONE:
    pop hl
    pop af
    ret

;       Subroutine      Print a buffer of data in HEX
;       Inputs          HL - buffer to be printed
;                       BC - number of bytes
;       Outputs         ________________________
PRINTHEX_BUFFER:
    ld d,16
    call PRINTHL
_PRINTHEX_LOOP:
    ld a, (hl)
    call PRINTHEX
    ld a, 020h
    call PRINTCHAR
    inc hl
    dec bc
    ; decrement d and check if zero
    dec d
    ld a, d
    and a
    jr nz, _PRINTHEX_NEXT
    push hl
    ld hl, TXT_CRLF
    call PRINT
    pop hl
    ld d,16
    call PRINTHL

_PRINTHEX_NEXT:
    ld a,b
    or c
    jp nz, _PRINTHEX_LOOP
    ret

PRINTHL:
    ld a, h
    call PRINTHEX
    ld a, l
    call PRINTHEX
    ld a, ':'
    call PRINTCHAR
    ret 

;       Subroutine      Print 8-bit hexidecimal number
;       Inputs          A - number to be printed - 0ABh
;       Outputs         ________________________
PRINTHEX:
    push af
    push bc
    push de
    call __NUMTOHEX
    ld a, d
    call PRINTCHAR
    ld a, e
    call PRINTCHAR
    pop de
    pop bc
    pop af
    ret

PRINTCHAR:
    push bc,de,hl
    ld e,a
    ld c,02h
    call BDOS
    pop hl,de,bc
    ret

;       Subroutine      Convert 8-bit hexidecimal number to ASCII reprentation
;       Inputs          A - number to be printed - 0ABh
;       Outputs         DE - two byte ASCII values - D=65 / 'A' and E=66 / 'B'
__NUMTOHEX:
    ld c, a   ; a = number to convert
    call _NTH1
    ld d, a
    ld a, c
    call _NTH2
    ld e, a
    ret  ; return with hex number in de
_NTH1:
    rra
    rra
    rra
    rra
_NTH2:
    or 0F0h
    daa
    add a, 0A0h
    adc a, 040h ; Ascii hex at this point (0 to F)   
    ret

; -------------------------------------------------------
; SIGSLOT
; Returns in A the next slot every time it is called.
; For initializing purposes, THISSLT has to be $FF.
; If no more slots, it returns A=$FF.
; --------------------------------------------------------

; this code is programmed by Nestor Soriano aka Konamiman

sigslot:
		ld	a,(THISSLT)		;Returns the next slot, starting by
		cp	0FFh			;slot 0. Returns $FF when there are not more slots
		jr	nz,SIGSL1		;Modifies AF, BC, HL.  
		ld	a,(EXPTBL)
		and	%10000000
		ld	(THISSLT),a
		ret

SIGSL1:
		ld	a,(THISSLT)
		cp	%10001111
		jr	z,NOMASLT
		cp	%00000011
		jr	z,NOMASLT
		bit	7,a
		jr	nz,SLTEXP

SLTSIMP:
		and	%00000011
		inc	a
		ld	c,a
		ld	b,0
		ld	hl,EXPTBL
		add	hl,bc
		ld	a,(hl)
		and	%10000000
		or	c
		ld	(THISSLT),a
		ret

SLTEXP:
		ld	c,a
		and	%00001100
		cp	%00001100
		ld	a,c
		jr	z,SLTSIMP
		add	a,%00000100
		ld	(THISSLT),a
		ret

NOMASLT:
		ld	a,0FFh
		ret

THISSLT	db	0FFh		; sigslot flag
