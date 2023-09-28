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
slotsize $2000   ;
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
.define CRAMWrite $C000
.define Port_MemoryControl $3E
.define Port_IOPortControl $3F
.define Port_PSG $7F
.define Port_FMAddress $F0
.define Port_FMData $F1
.define Port_AudioControl $F2
.define str_term $FF
.define str_crlf $FE
.define str_tab_start $F0
;RAM section variables - NOTE: C0B0-C0D5 is occupied by PSGlib RAM variables. Don't define anything in that window!
.define vblank_flag $C000
.define intro_color $C064
.define framecounter $C0F0
.define startintrocheck $C0F9
.define respondrate $C0FA
.define cur_pal $C3C0

; Input Ports
.define Port_VDPStatus $BF
.define Port_IOPort1 $DC
.define Port_IOPort2 $DD

;include PSGlib
.include "inc\PSGlib.inc"

;More assembler stuff to make the file pretty :)
.smstag
.sdsctag 0.1, "FINAL FANTASY", "The first Final Fantasy game, now on Sega Master System", "Cereza64" ;Remember to edit version number
.bank 0 slot 0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;EntryReset                                                                 ;
;	This is the point that the code will jump to at console boot or reset.  ;
;	Don't move this!                                                        ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.org $0000
.section "EntryReset" force
EntryReset:
	di      ;disabling interrupts
	im 1    ;set to Interrupt Mode 1
	ld sp,$DFF0 ;reset stack pointer
	;set up VDP registers
	ld hl,VDPInitData
    ld b,VDPInitDataEnd-VDPInitData
    ld c,VDPControl
    otir
	call PSGStop
	call PSGSFXStop
	call ClearBGPalette
	jp GameStart
.ends

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;InterruptHandler                                                           ;
;	This code CANNOT be moved elsewhere, as VBlank interrupts automatically ;
;	jump to this address.                                                   ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.org $0038
.section "InterruptHandler" force
InterruptHandler:
	ex af,af'
	exx
	in a,(Port_VDPStatus)
	ld a,$01
	ld (vblank_flag),a
	exx
	ex af,af'
	ei
	reti
.ends
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;PauseHandler                                                               ;
;	Another chunk of code I strongly recommend not touching. Pausing        ;
;	automatically jumps to this point.                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.org $0066
.section "PauseHandler" force
PauseHandler:
	retn
.ends

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;GameStart                                                                  ;
;	The game jumps to this point shortly after EntryReset to initialize     ;
;	various sections of RAM and either run the intro or the title screen    ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.section "GameStart" free
GameStart:
	;initialize 4KB RAM to 00
	ld hl,$C000
	ld bc,$1000
	xor a
	call MemoryFill
	
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
	
	;Initialize PSGLib - much thanks to sverx :)
	call PSGInit
	
	;Look at a spot in RAM to check if coming in from cold boot or reset
	;ld a,(startintrocheck)
	;cp $4D
	;jr z,+
	;ld (startintrocheck),$4D
	call EnterIntroStory
.ends

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;EnterIntroStory                                                            ;
;	If the console is coming in from a cold boot, code is directed here     ;
;	to show the intro text.                                                 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.section "EnterIntroStory" free
EnterIntroStory:
	call IntroTitlePrepare
	
	ld de,$3800
	ld hl,strings.intro
	call DrawComplexString

    ; Turn screen on
	ld a,%11100000
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
	ei
	
	ld a,$20 ;blue out cur_pal
	ld (cur_pal+$00),a
	ld (cur_pal+$01),a
	ld (cur_pal+$02),a
	ld (cur_pal+$03),a
	ld (cur_pal+$04),a
	jp IntroStory_MainLoop
;-:	call WaitForVBlank
;	call PSGFrame
;	jr -
.ends

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;IntroTitlePrepare                                                          ;
;	Loads Palettes, Tiles, and Music for the intro                          ;
;                                                                           ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.section "IntroTitlePrepare" free
IntroTitlePrepare:
	;Chunk of palette/font loads for intro
	call PrepareBKGLoadPalette
	call LoadBKGPalette_Intro
	call PrepareSPRLoadPalette
	call LoadSPRPalette_Intro
	call PrepareLoadFont
	call LoadFont
	;Load and play the prelude for the Intro Story
	ld hl,music_prelude
	call PSGPlay
	ret
.ends

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;DrawComplexString                                                          ;
;	Called to draw all text, including the intro                            ;
;                                                                           ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.section "DrawComplexString" free
DrawComplexString:
	ld a,d
	or $40
	ld d,a
	call SetVDPAddressALT ;de is keeping track of the vram pointer within this routine
	ld bc,$0040 ;used for line feeds
-:	ld a,(hl) ;load the next byte
	inc hl ;make sure the pointer is moved on regardless
	cp str_term ;test for string terminator
	ret z
	cp str_crlf ;test for newlines
	jr nz, +
	ex de,hl
	add hl,bc ;moves to next line (lf)
	ld a,l
	and $C0 ;moves to start of line (cr)
	ld l,a
	ex de,hl
	call SetVDPAddressALT
	jr -
+:	cp str_tab_start ;test for tabs
	jr c,+
	jr z,+
	sub str_tab_start
	sla a
	add a,e
	ld e,a
	call SetVDPAddressALT
	jr -
+:	out (VDPData),a ;write the tile index
	xor a
	out (VDPData),a ;keeping it simple with first screenmap byte being $00 for now
	jr -
.ends

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;WaitForVBlank                                                              ;
;	Does exactly what you think it does. Uses a flag in RAM, so if you      ;
;	modify the flag, modify this code to match as well.                     ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.section "WaitForVBlank" free
WaitForVBlank:
-:	halt
	ld a,(vblank_flag)
	or a
	jp z,-
	xor a
	ld (vblank_flag),a
	ret
.ends

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;IntroStory_MainLoop                                                        ;
;	Animates the intro text from invisible to white, then loops until a     ;
;	button has been pressed                                                 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.section "IntroStory_MainLoop" free
IntroStory_MainLoop:
	call IntroStory_AnimateRow
-:	call IntroStory_Frame
	jr -
.ends

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;IntroStory_AnimateRow                                                      ;
;	This code animates a row of text in the intro by repeatedly changing    ;
;	the color of the text from blue to dark gray to light gray to white.    ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.section "IntroStory_AnimateRow" free
IntroStory_AnimateRow:
	ld a,$15 ;load dark gray as first intro color
	ld (intro_color),a ;store in RAM
MainLoop:	
	ld a,(intro_color)
	ld (cur_pal+$04),a ;load intro color into text color index
-:	call IntroStory_Frame ;do a frame
	ld hl,framecounter
	inc (hl) ;and count another frame
	ld a,(framecounter) ;check to see if we're on the 16th frame
	and $0F
	jr nz,+ ;if not, alternate between main and sub color
	ld a,(intro_color) ;if we are on even 16th frame, brighten the main color
	adc $15 ;by adding $15
	ld (intro_color),a ;and storing this new color
	cp $54 ;then check to see if we're done.
	jr nz,MainLoop ;If we aren't done, then continue the loop, otherwise
	ret ;exit
+:	ld a,(framecounter)
	rra ;move the low bit of the frame counter into carry to check if even or odd
	jr c,MainLoop ;if even frame, use main color next frame
	ld a,(cur_pal+$04)
	sub $15
	jr nz,++
	ld a,$20
++:	ld (cur_pal+$04),a
	jr -
.ends

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;IntroStory_Frame                                                           ;
;	Handles updating the color palette and music frames during              ;
;	IntroStory_MainLoop and IntroStory_AnimateRow                           ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.section "IntroStory_Frame" free
IntroStory_Frame:
	call WaitForVBlank
	call DrawPalette
	call PSGFrame
	ret
.ends

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;DrawPalette                                                                ;
;	
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.section "DrawPalette" free
DrawPalette:
	call PrepareBKGLoadPalette
	ld b,$0F
	jp DrawPalette_Norm

DrawPalette_Norm:
	ld hl,cur_pal
	ld c,VDPData
	otir
	ret
.ends

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;SetVDPAddress                                                              ;
;	A general purpose subroutine which preserves A and flag registers       ;
;	before sending contents of HL to VDP control port.                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.section "SetVDPAddress" free
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
.ends

;SetVDPAddressALT uses DE instead of HL, called for certain routines where DE
;	is more preferable to HL

.section "SetVDPAddressALT" free
SetVDPAddressALT:
	push af
		ld a,e
		out (VDPControl),a
		ld a,d
		out (VDPControl),a
	pop af
	ret
.ends

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;CopyToVDP                                                                  ;
;	A general purpose subroutine used to copy large chunks of data to       ;
;	the VDP data ports. Typically for palette, tile, and tilemap loads.     ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.section "CopyToVDP" free
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
.ends

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;PrepareBKGLoadPalette/PrepareSPRLoadPalette                                ;
;	General purpose subroutines used to write to the color palette,         ;
;	either the background ($00-0F) or sprites ($10-1F).                     ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.section "PrepareBKGLoadPalette" free
PrepareBKGLoadPalette:
	; Set VRAM write address to CRAM (palette) address 0
    ld hl,$0000 | CRAMWrite
    jp SetVDPAddress
.ends

.section "PrepareSPRLoadPalette" free
PrepareSPRLoadPalette:
	; Set VRAM write address to CRAM (palette) address 0
    ld hl,$0010 | CRAMWrite
    jp SetVDPAddress
.ends

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;LoadBKGPalette_Intro/LoadSPRPalette_Intro                                  ;
;	Palette load subroutines used for the intro text                        ;
;                                                                           ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.section "LoadBKGPalette_Intro" free
LoadBKGPalette_Intro:
	; Output tile data
	ld hl,BKGSPR_IntroPalette
	ld bc,BKGSPR_IntroPaletteEnd-BKGSPR_IntroPalette
	jp CopyToVDP
.ends

.section "LoadSPRPalette_Intro" free
LoadSPRPalette_Intro:
	; Output tile data
	ld hl,BKGSPR_IntroPalette
	ld bc,BKGSPR_IntroPaletteEnd-BKGSPR_IntroPalette
	jp CopyToVDP
.ends

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;PrepareLoadFont/LoadFont                                                   ;
;	Preps and writes font tile data to VDP                                  ;
;                                                                           ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.section "PrepareLoadFont" free
PrepareLoadFont:
	; Set VRAM write address to tile index 0
    ld hl,$0000 | VRAMWrite
	jp SetVDPAddress
.ends

.section "LoadFont" free	
LoadFont:
	ld hl,FontIconsData
	ld bc,FontIconsDataSize
	jp CopyToVDP
.ends

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;MemoryFill                                                                 ;
;	A small subroutine that fills a BC-sized block of memory starting at    ;
;	address HL with the contents of A.                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.section "MemoryFill" free
MemoryFill:
	ld (hl),a
	ld d,h
	ld e,l
	inc de
	dec bc
	ld a,b
	or c
	ret z
	ldir
	ret
.ends

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;ClearBGPalette                                                             ;
;	Fills the entire palette with black. This ensures that any unused       ;
;	palette space will be black instead of random colors.                   ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.section "ClearBGPalette" free
ClearBGPalette:
	; Set VRAM write address to CRAM (palette) address 0
    ld hl,$0000 | CRAMWrite
    call SetVDPAddress
	ld bc,$20        ;Fill all 32 ($20) palette entries with black
-:	xor a
	out (VDPData),a
	dec bc
	ld a,b
	or c
	jr nz,-
	ret
.ends
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
;-:	halt
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

;;;;;
;VDP;
;;;;;

;VDPInitData - Ensures that the VDP is properly set up at console boot or reset
.section "VDPInitData" free
VDPInitData:
	.db $06,$80,$80,$81,$FF,$82,$FF,$85,$FF,$86,$00,$87,$00,$88,$00,$89,$FF,$8A
VDPInitDataEnd:
.ends

;;;;;;;;;;;;;
;BKG_Palette;
;;;;;;;;;;;;;

;BKGSPR_IntroPalette - Palette data for the intro (both background and sprite). Note that text tiles use palette index $04
.section "BKGSPR_IntroPalette" free
BKGSPR_IntroPalette:
	.db $20,$20,$20,$20,$20 ;all dark blue
BKGSPR_IntroPaletteEnd:
.ends

;BKG_MenuPalette:
;	.db $00,$20,$15,$2A,$3F,$2F,$0C,$08 ;black, dark blue, dark gray, gray, white, bone yellow, neon green, dark green
;BKG_MenuPaletteEnd:

;;;;;;;;;;;;;
;SPR_Palette;
;;;;;;;;;;;;;

;SPR_CharBattlePalette:
;	.db $00,$03,$06,$0B,$0F,$15,$2A,$2B,$30,$38,$3F ;black (transparent), red, brown, light orange, yellow, dark gray, gray, pink, blue, light blue, white
;SPR_CharBattlePaletteEnd:

;SPR_WoodWeaponsPalette:
;	.db $06,$1B,$00
;SPR_WoodWeaponsPaletteEnd:

;SPR_IronWeaponsPalette:
;	.db $15,$2A,$3F
;SPR_IronWeaponsPaletteEnd:

;SPR_GreenWeaponsPalette:
;	.db $04,$19,$2D
;SPR_GreenWeaponsPaletteEnd:

;SPR_PinkWeaponsPalette:
;	.db $02,$13,$27
;SPR_PinkWeaponsPaletteEnd:

;SPR_SilverWeaponsPalette:
;	.db $14,$28,$3C
;SPR_SilverWeaponsPaletteEnd:

;SPR_FireSwordPalette:
;	.db $02,$03,$17
;SPR_FireSwordPaletteEnd:

;SPR_BlueWeaponsPalette:
;	.db $20,$30,$38
;SPR_BlueWeaponsPaletteEnd:

;SPR_GiantBanePalette:
;	.db $21,$32,$3A
;SPR_GiantBanePaletteEnd:

;SPR_OrangeWeaponsPalette:
;	.db $01,$07,$0B
;SPR_OrangeWeaponsPaletteEnd:

;SPR_WereThorPalette:
;	.db $22,$33,$3B
;SPR_WereThorPaletteEnd:

;SPR_RuneLightPalette:
;	.db $21,$32,$36
;SPR_RuneLightPaletteEnd:

;SPR_XcalburPalette:
;	.db $06,$0A,$0F
;SPR_XcalburPaletteEnd:

;SPR_WhiteSpellsPalette:
; .db $3F,$2A,$3E
;SPR_WhiteSpellsPaletteEnd:

;SPR_LBlueSpellsPalette:
; .db $3F,$34,$38
;SPR_LBlueSpellsPaletteEnd:

;SPR_BlueSpellsPalette:
; .db $3F,$34,$39
;SPR_BlueSpellsPaletteEnd:

;SPR_PurpleSpellsPalette:
; .db $3F,$35,$36
;SPR_PurpleSpellsPaletteEnd:

;SPR_PinkSpellsPalette:
; .db $3F,$22,$37
;SPR_PinkSpellsPaletteEnd:

;SPR_RedSpellsPalette:
; .db $3F,$13,$27
;SPR_RedSpellsPaletteEnd:

;SPR_DOrangeSpellsPalette:
; .db $3F,$03,$17
;SPR_DOrangeSpellsPaletteEnd:

;SPR_OrangeSpellsPalette:
; .db $3F,$07,$1B
;SPR_OrangeSpellsPaletteEnd:

;SPR_LOrangeSpellsPalette:
; .db $3F,$06,$0B
;SPR_LOrangeSpellsPaletteEnd:

;SPR_LGreenSpellsPalette:
; .db $3F,$08,$0E
;SPR_LGreenSpellsPaletteEnd:

;SPR_GreenSpellsPalette:
; .db $3F,$08,$1D
;SPR_GreenSpellsPaletteEnd:

;SPR_SGreenSpellsPalette:
; .db $3F,$18,$2D
;SPR_SGreenSpellsPaletteEnd:

;SPR_CyanSpellsPalette:
; .db $3F,$28,$3C
;SPR_CyanSpellsPaletteEnd:

;;;;;
;lut;
;;;;;

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

;;;;;;
;data;
;;;;;;

;data_MaxRewardPlusOne:
;	.dl $0F4240 ;This is the GP cap of 999999 + 1

;data_MaxHPPlusOne:
;	.dw $03E8 ;This is the HP cap of 999 + 1
	
;;;;;;;
;tiles;
;;;;;;;

;FontIconsData - bin file containing text tiles, icons for weapons/armor, and textbox borders
.section "FontIconsData" free
FontIconsData:
.incbin "bin\fonticons.bin" fsize FontIconsDataSize
.ends

;IntroText
.section "IntroText" free
.stringmaptable FontIconsStringmap "inc\FontIconsStringmap.tbl"

strings.intro:
	.stringmap FontIconsStringmap, "<br>"
	.stringmap FontIconsStringmap, "<5>The world is veiled in<br><br>"
	.stringmap FontIconsStringmap, "<4>darkness. The wind stops,<br><br>"
	.stringmap FontIconsStringmap, "<9>the sea is wild,<br><br>"
	.stringmap FontIconsStringmap, "<2>and the earth begins to rot.<br><br>"
	.stringmap FontIconsStringmap, "<9>The people wait,<br><br>"
	.stringmap FontIconsStringmap, "<2>their only hope, a prophecy....<br><br><br>"
	.stringmap FontIconsStringmap, "<1>'When the world is in darkness<br><br>"
	.stringmap FontIconsStringmap, "<3>Four Warriors will come....'<br><br>"
	.stringmap FontIconsStringmap, "<3>After a long journey, four<br><br>"
	.stringmap FontIconsStringmap, "<5>young warriors arrive,<br><br>"
	.stringmap FontIconsStringmap, "<6>each holding an ORB.</>"
.ends

;;;;;;;
;music;
;;;;;;;

;music_prelude - The Final Fantasy Prelude played during Intro, Title Screen, and in Party Order Menu
.section "music_prelude" free
music_prelude:
.incbin "bin\prelude.psg"
.ends