;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                                ;;;;
;;;;    FINAL                       ;;;;
;;;;        FANTASY                 ;;;;
;;;;            MASTER SYSTEM       ;;;;
;;;;                PROJECT         ;;;;
;;;;    BY                          ;;;;
;;;;        CEREZA64                ;;;;
;;;;                                ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;This project is dedicated to all of my friends who have had to put up with my obsession with this game and much more,
;;    to the original Square A-Team who created an absolutely legendary game, and to NASIR, who programmed the original
;;    game and its two successors. His accomplishment is what inspired me to try my hand at assembly to begin with.
;;    I would also like to thank SEGA for making some of the coolest video game consoles ever made. Hoping that some-
;;    day I'll be able to make a game for the Genesis or Saturn, the latter especially since it's one of my all-time
;;    favorite systems, but for now I'm starting (relatively) simple with the Master System.
;;
;;To preface everything, this is a project I started because I was extremely saddened by the lack of games in the
;;    Master System's NA library. Nintendo's third party policy basically killed all of its competitors in cold
;;    blood, and I believe that was totally unfair. I would like to "right a wrong" here by porting a few NES
;;    classics to the Master System as accurately and fairly as possible, pretending I'm someone like Nasir
;;    (or perhaps some unpaid intern) whose job it is to put the game on other systems. I'm only allowing myself
;;    to make mostly necessary changes; things like color definition and music adjustments so that the game will
;;    run properly on a NA Master System. I know already a few times that I've "upgraded" a few sprites, meaning
;;    I gave them more colors than on the NES :P
;;Other than that, I'm doing a close approximation of how Sega would "reprogram" third party titles to the Master System
;;    like Ghostbusters, Miracle Warriors, and Ys. I prioritize staying close to the original when possible, but a few
;;    changes are absolutely necessary to fit the UI elements in the screen. A lot of menus have been cleaned up and made
;;    compact to fit messy, scattered textboxes from a 256x240 screen into a 256x192 screen.
;;
;;I really don't like the word "homebrew", so don't call this a homebrew. Let me live my fantasy!!!
;;
;;Oh yeah, assuming Square Enix doesn't nuke the crap out of me for this when it's released, I'll drop this source code
;;    to those who contact me so that ROM hacking this port will be extremely easy. Since I'm not making any significant
;;    changes to the game itself, I know people are going to want to hack it and fix the various archaic or difficult
;;    aspects of FF1. To that, I say "go ahead!" All I ask is that you don't take credit for everything. I'm not putting 
;;    my name in the game itself, because I always hated that about translation patches or hack authors. It breaks my immersion,
;;    and I know a lot of people feel the same way (Looking at the one group who translated Dragon Quest I*II for the SNES).
;;    Otherwise, I will do my best to comment the absolute hell out of this code so that everyone (including myself) can
;;    find exactly which line of code does what and easily modify it.
;;
;;TLDR I love Final Fantasy, I wanted to put it on a Sega, you guys have free reign on hacking it once i'm done :D
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;SECTION 0 - SETTING ALL THIS STUFF UP
;Assembler specific memory/bank setup
.memorymap     ;;;
defaultslot 0    ;
slotsize $4000   ;
slot 0 $0000     ;
slot 1 $4000     ;
slot 2 $8000     ;
slot 3 $C000     ;RAM
.endme         ;;;

.rombankmap    ;;;
bankstotal 10    ;
banksize $4000   ;
banks 10         ;
.endro         ;;;

;Defining stuff
.define VDPControl $BF
.define VDPData $BE
.define VRAMWrite $4000
.define CRAMWrite $c000
.define Port_MemoryControl $3E
.define Port_IOPortControl $3F
.define Port_PSG $7F
.define Port_FMAddress $F0
.define Port_FMData $F1
.define Port_AudioControl $F2

; Input Ports
.define Port_VDPStatus $BF
.define Port_IOPort1 $DC
.define Port_IOPort2 $DD

;More assembler stuff to make the file pretty :)
.sdsctag 0.1, "FINAL FANTASY", "The first Final Fantasy game, now on Sega Master System", "Cereza64" ;Remember to edit version number
.bank 0 slot 0
.org $0000

Entry_Reset:
	di      ;disabling interrupts
	im 1    ;set to Interrupt Mode 1
	jp reset
	
;PAUSE BUTTON HANDLING - DO NOT MESS WITH ME!
.org $0066
	retn
;End pause handling

reset:
	ld sp, $dff0
	
	;set up VDP registers
	ld hl,VDPInitData
    ld b,VDPInitDataEnd-VDPInitData
    ld c,VDPControl
    otir
	
	; Set VRAM write address to $0000
    ld hl,$0000 | VRAMWrite
    call SetVDPAddress
    ; Output 16KB of zeroes
    ld bc,$4000     ; Counter for 16KB of VRAM
-:  xor a
    out (VDPData),a ; Output to VRAM address, which is auto-incremented after each write
    dec bc
    ld a,b
    or c
    jr nz,-
	
	call PrepareLoadPalette
	call LoadPalette_Intro
	call PrepareLoadFont
	call LoadFont
	
	; 1. Set VRAM write address to tilemap index 0
    ld hl,$3800 | VRAMWrite
    call SetVDPAddress
    ; 2. Output tilemap data
    ld hl,IntroTilemap
	ld bc,IntroTilemapSize-IntroTilemap
-:  ld a,(hl)
    out (VDPData),a
    inc hl
	dec bc
	ld a,b
	or c
    jp nz,-

    ; Turn screen on
    ld a,%01000000
;          ||||||`- Zoomed sprites -> 16x16 pixels
;          |||||`-- Doubled sprites -> 2 tiles per sprite, 8x16
;          ||||`--- Mega Drive mode 5 enable
;          |||`---- 30 row/240 line mode
;          ||`----- 28 row/224 line mode
;          |`------ VBlank interrupts
;          `------- Enable display
    out (VDPControl),a
    ld a,$81
    out (VDPControl),a

    ; Infinite loop to stop program
-:  jr -
	
SetVDPAddress:
; Sets the VDP address
; Parameters: hl = address
    push af
        ld a,l
        out (VDPControl),a
        ld a,h
        out (VDPControl),a
    pop af
    ret

CopyToVDP:
; Copies data to the VDP
; Parameters: hl = data address, bc = data length
; Affects: a, hl, bc
-:  ld a,(hl)    ; Get data byte
    out (VDPData),a
    inc hl       ; Point to next letter
    dec bc
    ld a,b
    or c
    jr nz,-
    ret
	
PrepareLoadPalette:
	; Set VRAM write address to CRAM (palette) address 0
    ld hl,$0000 | CRAMWrite
    jp SetVDPAddress
	
LoadPalette_Intro:
	; Output tile data
	ld hl,BKG_IntroPalette
	ld bc,BKG_IntroPaletteEnd-BKG_IntroPalette
	jp CopyToVDP
	
PrepareLoadFont:
	; Set VRAM write address to tile index 0
    ld hl,$0000 | VRAMWrite
	jp SetVDPAddress
	
LoadFont:
	ld hl,FontIconsData
	ld bc,FontIconsDataSize
	jp CopyToVDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
;FMDetection:
;    ld a,($C000)
;    or $04             ; Disable I/O chip
;    out ($3E),a
;    ld bc $0700        ; Counter (7 -> b), plus 0 -> c
;-:  ld a,b
;    and %00000001      ; Mask to bit 0 only
;    out ($F2),a        ; Output to the audio control port
;    ld e,a
;    in a,($F2)         ; Read back
;    and %00000111      ; Mask to bits 0-2 only
;    cp e               ; Check low 3 bits are the same as what was written
;    jr nz,+
;    inc c              ; c = # of times the test was passed
;+:  djnz -
;    ld a,c
;    cp 7               ; Check test was passed 7 times out of 7
;    jr z,+
;    xor a              ; If not, result is 0
;+:  and 1              ; If so, result is 1
;    out ($F2),a        ; Output result to audio control port
;    ld (HasFM),a       ; Store result in HasFM
;    ld a,($C000)
;    out ($3E),a        ; Turn I/O chip back on
;    ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;PrepareEnemyFormation_FiendChaos:
;	cp $04
;	jr nz,PrepareEnemyFormation_Fiend
;	jp PrepareEnemyFormation_Chaos
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;BattleFadeOutAndRestartGame:
;	call BattleFadeOut
;	jp GameStart
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;BattleOver_Run:
;	call Battle_FlipAllChars
;	jp ExitBattle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;RespondDelay:
;	ld c,btl_responddelay
;-:	call WaitForVBlank
;	call MusicPlay
;	xor a
;	dec c
;	cp c
;	jr nz,-
;	ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                             DATA SECTION                                  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;VDP
VDPInitData:
.db $04,$80,$00,$81,$ff,$82,$ff,$85,$ff,$86,$ff,$87,$00,$88,$00,$89,$ff,$8a
VDPInitDataEnd:

;BKG_Palette
BKG_IntroPalette:
.db $20,$15,$2A,$3F ;dark blue, dark gray, gray, white
BKG_IntroPaletteEnd:

;BKG_MenuPalette:
;.db $00,$20,$15,$2A,$3F,$2F,$0C,$08 ;black, dark blue, dark gray, gray, white, bone yellow, neon green, dark green
;BKG_MenuPaletteEnd:

;SPR_Palette
;SPR_CharBattlePalette:
;	.db $00,$03,$06,$0B,$0F,$15,$2A,$2B,$30,$38,$3F ;black (transparent), red, brown, light orange, yellow, dark gray, gray, pink, blue, light blue, white
;SPR_CharBattlePaletteEnd:

;LUTS
;lut_ExpToAdvance:
;	.dl $000028,$0000C4,$000223,$000493,$000862,$000DDE,$001555,$001F15,$002B6C,$003AA8
;	.dl $004D17,$006307,$007CC6,$009AA2,$00BCE9,$00E3E9,$010FF1,$01414D,$01784C,$01B53C
;	.dl $01F86B,$024228,$0292BF,$02EA7F,$0349B7,$03B0B3,$041FC3,$049733,$051753,$059772
;	.dl $061792,$0697B1,$0717D1,$0797F0,$081810,$08982F,$09184F,$09986E,$0A188E,$0A98AD
;	.dl $0B18CD,$0B98EC,$0C190C,$0C992B,$0D194B,$0D996A,$0E198A,$0E99A9,$0F19C9

;lut_RespondDelay:
;	.db $78,$50,$3C,$2D,$1E,$0F,$05,$01

;lut_AilmentObToIb:
;	.db $00,$01,$02,$04

;lut_AilmentIbToOb:
;	.db $00,$01,$02,$01,$03,$01,$02,$01

;lut_LvlUpHitRateBonus:
;	.db $03,$02,$03,$02,$01,$01,$03,$02,$03,$02,$01,$01
	;    FT  TH  BB  RM  WM  BM  KN  NJ  MA  RW  WW  BW

;lut_LvlUpMagDefBonus:
;	.db $02,$03,$01,$03,$03,$03,$02,$03,$04,$03,$03,$03
	;    FT  TH  BB  RM  WM  BM  KN  NJ  MA  RW  WW  BW
	; original values are 3, 2, 4, 2, 2, 2, and 3, 2, 1, 2, 2, 2

;lut_LevelUpDataPtrs:
;	.dw data_LevelUpData_Raw + (98 * 0)
;	.dw data_LevelUpData_Raw + (98 * 1)
;	.dw data_LevelUpData_Raw + (98 * 2)
;	.dw data_LevelUpData_Raw + (98 * 3)
;	.dw data_LevelUpData_Raw + (98 * 4)
;	.dw data_LevelUpData_Raw + (98 * 5)
;	.dw data_LevelUpData_Raw + (98 * 0)
;	.dw data_LevelUpData_Raw + (98 * 1)
;	.dw data_LevelUpData_Raw + (98 * 2)
;	.dw data_LevelUpData_Raw + (98 * 3)
;	.dw data_LevelUpData_Raw + (98 * 4)
;	.dw data_LevelUpData_Raw + (98 * 5)

;lut_CharStatsPtrTable:
;	.dw ch_stats
;	.dw ch_stats + $40
;	.dw ch_stats + $80
;	.dw ch_stats + $C0

;lut_CharMagicPtrTable:
;	.dw ch_magicdata
;	.dw ch_magicdata + $40
;	.dw ch_magicdata + $80
;	.dw ch_magicdata + $C0

;lut_FiendTSAPtrs:
;	.dw data_FiendTSA
;	.dw data_FiendTSA + $50
;	.dw data_FiendTSA + $A0
;	.dw data_FiendTSA + $F0



;data
;data_MaxRewardPlusOne:
;	.dl $0F4240 ;This is the GP cap of 999999 + 1

;data_MaxHPPlusOne:
;	.dw $03E8 ;This is the HP cap of 999 + 1
	
;tiles
FontIconsData:
.incbin "fonticons.bin" fsize FontIconsDataSize

IntroTilemap:
.incbin "introtilemap.bin" fsize IntroTilemapSize