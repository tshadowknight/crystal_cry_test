GFX_49c0c: ; 49c0c
INCBIN "gfx/unknown/049c0c.2bpp"
; 49cdc

MainMenu: ; 49cdc
	ld de, MUSIC_NONE
	call PlayMusic
	ld b, SCGB_DIPLOMA
	xor a
	ld [hBGMapMode], a
	ld [wDisableTextAcceleration], a
	call Function49ed0
	ld b, SCGB_DIPLOMA
	call GetSGBLayout
	call SetPalettes
	ld hl, wGameTimerPause
	res GAMETIMERPAUSE_TIMER_PAUSED_F, [hl]
	;	wCryPitch
	;	wCryLength	
	xor a
	ld [wBuffer1], a ; cry pitch length
	ld [wBuffer2], a ; base cry
	ld [wBuffer3], a ; delay counter
	ld de, .titleText
	hlcoord 0, 0
	call PlaceString
	ld de, .base
	hlcoord 2, 2
	call PlaceString
	ld de, .pitch
	hlcoord 2, 4
	call PlaceString
	ld de, .length
	hlcoord 2, 6
	call PlaceString
.CryEditor:	
	call DelayFrame	
	ld a, [wBuffer2]
	hlcoord 10, 2
	call .placeDigits
	
	ld a, [wCryPitch+1]
	hlcoord 10, 4
	call .placeDigits
	ld a, [wCryPitch]
	hlcoord 12, 4
	call .placeDigits
	
	ld a, [wCryLength+1]
	hlcoord 10, 6
	call .placeDigits
	ld a, [wCryLength]
	hlcoord 12, 6
	call .placeDigits
	
	ld a, $BA
	hlcoord 9, 2
	ld [hl], a
	hlcoord 9, 4
	ld [hl], a
	hlcoord 9, 6
	ld [hl], a
	
	ld b, $ED
	ld a, [wBuffer1]
	cp 0
	jr nz, .arrowNotBase
	ld a, b 
	hlcoord 9, 2
	ld [hl], a
	jr .arrowPlaced
.arrowNotBase
	cp 1
	jr nz, .arrowNotPitch
	ld a, b 
	hlcoord 9, 4
	ld [hl], a
	jr .arrowPlaced
.arrowNotPitch
	ld a, b 
	hlcoord 9, 6
	ld [hl], a	
.arrowPlaced	
	
	call GetJoypad
	ld a, [hJoyDown]
	and $FF 
	jp z, .resetInputDelay
	ld a, [wBuffer3]
	inc a 
	ld [wBuffer3], a
	cp 1 
	jr z, .firstPress
	cp 10 
	jr c, .notLEFT
	
.firstPress:	
	ld a, [hJoyDown]
	and A_BUTTON
	jr z, .notA
	ld a, [wBuffer2]
	ld e, a
	ld d, 0
	farcall _PlayCry
	jp .nextFrame
.notA:
	ld a, [hJoyDown]
	and D_RIGHT
	jr z, .notRIGHT
	ld a, [wBuffer1]
	cp 0
	jr nz, .rightNotBase
	ld a, [wBuffer2]
	cp $43
	jr nc, .nextFrame
	inc a
	ld [wBuffer2], a
	jr .nextFrame
.rightNotBase	
	cp 1
	jr nz, .rightNotPitch
	ld hl, wCryPitch
	call .incrementparam
	jr .nextFrame
.rightNotPitch		
	ld hl, wCryLength
	call .incrementparam
	jr .nextFrame
.notRIGHT:	
	ld a, [hJoyDown]
	and D_LEFT
	jr z, .notLEFT
	ld a, [wBuffer1]
	cp 0
	jr nz, .leftNotBase
	ld a, [wBuffer2]
	cp 0
	jr z, .nextFrame
	dec a
	ld [wBuffer2], a
	jr .nextFrame
.leftNotBase	
	cp 1
	jr nz, .leftNotPitch
	ld hl, wCryPitch
	call .decrementParam
	jr .nextFrame
.leftNotPitch		
	ld hl, wCryLength
	call .decrementParam
	jr .nextFrame
	
.notLEFT	
	ld a, [hJoyPressed]
	and D_DOWN
	jr z, .notDOWN
	ld a, [wBuffer1]
	inc a 
	cp 3
	jr c, .noOverflow
	xor a
.noOverflow	
	ld [wBuffer1], a
	jr .nextFrame
.notDOWN	
	ld a, [hJoyPressed]
	and D_UP
	jr z, .notUP
	ld a, [wBuffer1]
	dec a
	cp $FF	
	jr nz, .noUnderflow
	ld a, 2
.noUnderflow	
	ld [wBuffer1], a
	jr .nextFrame
.notUP
.nextFrame:	
	
	jp .CryEditor
	
.resetInputDelay:
	xor a
	ld [wBuffer3], a	
	jp .CryEditor
	
.incrementparam:
	push hl
	call GetJoypad
	ld a, [hJoyDown]
	and B_BUTTON
	ld a, [hl]
	jr nz, .incrementHigh	
	ld b, 1
	add b
	ld [hl], a	
	jr nc, .incrementparamNoOverflow
.incrementHigh:	
	inc hl
	ld a, [hl]
	inc a
	ld [hl], a
.incrementparamNoOverflow:
	pop hl
	ret	
	
.decrementParam:
	push hl
	call GetJoypad
	ld a, [hJoyDown]
	and B_BUTTON
	ld a, [hl]
	jr nz, .decrementHigh
	ld b, 1
	sub b
	ld [hl], a	
	jr nc, .decrementParamNoOverflow
.decrementHigh:	
	inc hl
	ld a, [hl]
	dec a
	ld [hl], a
.decrementParamNoOverflow:
	pop hl
	ret		

.titleText:
	db "Cry Editor v0.1@"

.base:
	db "Base:@"	
	
.pitch:
	db "Pitch:@"	
	
.length:
	db "Length:@"		

.digits:
	db "0"
	db "1"
	db "2"
	db "3"
	db "4"
	db "5"
	db "6"
	db "7"
	db "8"
	db "9"
	db "A"	
	db "B"
	db "C"
	db "D"
	db "E"
	db "F"
	
.placeDigits
	push hl
	ld b, a
	ld hl, .digits
	and $F0
	sra a
	sra a
	sra a
	sra a
	and $0F
	ld d, 0 
	ld e, a 
	add hl, de 
	ld a, [hl]
	ld de, wStringBuffer1
	ld [de], a	
	ld hl, .digits	
	ld a, b
	and $F 
	ld d, 0 
	ld e, a 
	add hl, de 
	ld a, [hl]
	ld de, wStringBuffer1
	inc de
	ld [de], a	
	ld a, "@"
	inc de
	ld [de], a 
	dec de
	dec de
	pop hl
	call PlaceString	
	ret
	
.quit
	ret
; 49d14

.MenuHeader: ; 49d14
	db MENU_BACKUP_TILES ; flags
	menu_coords 0, 0, 16, 7
	dw .MenuData
	db 1 ; default option
; 49d1c

.MenuData: ; 49d1c
	db STATICMENU_CURSOR ; flags
	db 0 ; items
	dw MainMenuItems
	dw PlaceMenuStrings
	dw .Strings
; 49d20

.Strings: ; 49d24
	db "CONTINUE@"
	db "NEW GAME@"
	db "OPTION@"
	db "MYSTERY GIFT@"
	db "MOBILE@"
	db "MOBILE STUDIUM@"

.Jumptable: ; 0x49d60
	dw MainMenu_Continue
	dw MainMenu_NewGame
	dw MainMenu_Options
	dw MainMenu_MysteryGift
	dw MainMenu_Mobile
	dw MainMenu_MobileStudium
; 0x49d6c

CONTINUE       EQU 0
NEW_GAME       EQU 1
OPTION         EQU 2
MYSTERY_GIFT   EQU 3
MOBILE         EQU 4
MOBILE_STUDIUM EQU 5

MainMenuItems:

NewGameMenu: ; 0x49d6c
	db 2
	db NEW_GAME
	db OPTION
	db -1

ContinueMenu: ; 0x49d70
	db 3
	db CONTINUE
	db NEW_GAME
	db OPTION
	db -1

MobileMysteryMenu: ; 0x49d75
	db 5
	db CONTINUE
	db NEW_GAME
	db OPTION
	db MYSTERY_GIFT
	db MOBILE
	db -1

MobileMenu: ; 0x49d7c
	db 4
	db CONTINUE
	db NEW_GAME
	db OPTION
	db MOBILE
	db -1

MobileStudiumMenu: ; 0x49d82
	db 5
	db CONTINUE
	db NEW_GAME
	db OPTION
	db MOBILE
	db MOBILE_STUDIUM
	db -1

MysteryMobileStudiumMenu: ; 0x49d89
	db 6
	db CONTINUE
	db NEW_GAME
	db OPTION
	db MYSTERY_GIFT
	db MOBILE
	db MOBILE_STUDIUM
	db -1

MysteryMenu: ; 0x49d91
	db 4
	db CONTINUE
	db NEW_GAME
	db OPTION
	db MYSTERY_GIFT
	db -1

MysteryStudiumMenu: ; 0x49d97
	db 5
	db CONTINUE
	db NEW_GAME
	db OPTION
	db MYSTERY_GIFT
	db MOBILE_STUDIUM
	db -1

StudiumMenu: ; 0x49d9e
	db 4
	db CONTINUE
	db NEW_GAME
	db OPTION
	db MOBILE_STUDIUM
	db -1


MainMenu_GetWhichMenu: ; 49da4
	nop
	nop
	nop
	ld a, [wSaveFileExists]
	and a
	jr nz, .next
	ld a, $0 ; New Game
	ret

.next
	ld a, [hCGB]
	cp $1
	ld a, $1
	ret nz
	ld a, BANK(sNumDailyMysteryGiftPartnerIDs)
	call GetSRAMBank
	ld a, [sNumDailyMysteryGiftPartnerIDs]
	cp -1
	call CloseSRAM
	jr nz, .mystery_gift
	; This check makes no difference.
	ld a, [wStatusFlags]
	bit STATUSFLAGS_MAIN_MENU_MOBILE_CHOICES_F, a
	ld a, $1 ; Continue
	jr z, .ok
	jr .ok

.ok
	jr .ok2

.ok2
	ld a, $1 ; Continue
	ret

.mystery_gift
	; This check makes no difference.
	ld a, [wStatusFlags]
	bit STATUSFLAGS_MAIN_MENU_MOBILE_CHOICES_F, a
	jr z, .ok3
	jr .ok3

.ok3
	jr .ok4

.ok4
	ld a, $6 ; Mystery Gift
	ret
; 49de4

MainMenuJoypadLoop: ; 49de4
	call SetUpMenu
.loop
	call MainMenu_PrintCurrentTimeAndDay
	ld a, [w2DMenuFlags1]
	set 5, a
	ld [w2DMenuFlags1], a
	call GetScrollingMenuJoypad
	ld a, [wMenuJoypad]
	cp B_BUTTON
	jr z, .b_button
	cp A_BUTTON
	jr z, .a_button
	jr .loop

.a_button
	call PlayClickSFX
	and a
	ret

.b_button
	scf
	ret
; 49e09

MainMenu_PrintCurrentTimeAndDay: ; 49e09
	ld a, [wSaveFileExists]
	and a
	ret z
	xor a
	ld [hBGMapMode], a
	call .PlaceBox
	ld hl, wOptions
	ld a, [hl]
	push af
	set NO_TEXT_SCROLL, [hl]
	call .PlaceTime
	pop af
	ld [wOptions], a
	ld a, $1
	ld [hBGMapMode], a
	ret
; 49e27


.PlaceBox: ; 49e27
	call CheckRTCStatus
	and $80
	jr nz, .TimeFail
	hlcoord 0, 14
	ld b, 2
	ld c, 18
	call TextBox
	ret

.TimeFail:
	call SpeechTextBox
	ret
; 49e3d


.PlaceTime: ; 49e3d
	ld a, [wSaveFileExists]
	and a
	ret z
	call CheckRTCStatus
	and $80
	jp nz, .PrintTimeNotSet
	call UpdateTime
	call GetWeekday
	ld b, a
	decoord 1, 15
	call .PlaceCurrentDay
	decoord 4, 16
	ld a, [hHours]
	ld c, a
	farcall PrintHour
	ld [hl], ":"
	inc hl
	ld de, hMinutes
	lb bc, PRINTNUM_LEADINGZEROS | 1, 2
	call PrintNum
	ret

.min
; unused
	db "min.@"
; 49e75

.PrintTimeNotSet: ; 49e75
	hlcoord 1, 14
	ld de, .TimeNotSet
	call PlaceString
	ret
; 49e7f

.TimeNotSet: ; 49e7f
	db "TIME NOT SET@"
; 49e8c

.UnusedText: ; 49e8c
	; Clock time unknown
	text_jump UnknownText_0x1c5182
	db "@"
; 49e91

.PlaceCurrentDay: ; 49e91
	push de
	ld hl, .Days
	ld a, b
	call GetNthString
	ld d, h
	ld e, l
	pop hl
	call PlaceString
	ld h, b
	ld l, c
	ld de, .Day
	call PlaceString
	ret
; 49ea8

.Days:
	db "SUN@"
	db "MON@"
	db "TUES@"
	db "WEDNES@"
	db "THURS@"
	db "FRI@"
	db "SATUR@"
.Day:
	db "DAY@"
; 49ed0

Function49ed0: ; 49ed0
	xor a
	ld [hMapAnims], a
	call ClearTileMap
	call LoadFontsExtra
	call LoadStandardFont
	call ClearWindowData
	ret
; 49ee0


MainMenu_NewGame: ; 49ee0
	farcall NewGame
	ret
; 49ee7

MainMenu_Options: ; 49ee7
	farcall OptionsMenu
	ret
; 49eee

MainMenu_Continue: ; 49eee
	farcall Continue
	ret
; 49ef5

MainMenu_MysteryGift: ; 49ef5
	farcall MysteryGift
	ret
; 49efc
