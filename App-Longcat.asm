;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@                                                                            @
;@                               L o n g c a t                                @
;@                                                                            @
;@                            (c) by Martin Magni                             @
;@           SymbOS port 2025 by Prodatron / SymbiosiS (Jörn Mika)            @
;@                                                                            @
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

;Bugs

;Todo


;==============================================================================
;### CODE AREA ################################################################
;==============================================================================


;### PRGPRZ -> Application process
prgwin  db 0    ;main window ID

prgprz  call SySystem_HLPINI
        call sndini
        call mapget

        ld de,wingam
        ld a,(App_BnkNum)
        call SyDesktop_WINOPN       ;open startmenu edit window
        jp c,prgend                 ;memory full -> quit process
        ld (prgwin),a               ;window has been opened -> store ID

prgprz0 ld ix,(App_PrcID)           ;check for messages
        db #dd:ld h,-1
        ld iy,App_MsgBuf
        rst #08         ;no timer running -> sleep until message
        db #dd:dec l
        jr nz,prgprz0
        ld a,(App_MsgBuf+0)
        or a
        jr z,prgend
        cp MSR_DSK_WCLICK
        jr nz,prgprz0
        ld a,(App_MsgBuf+2)
        cp DSK_ACT_KEY
        jr z,prgkey
        cp DSK_ACT_CLOSE
        jr z,prgend
        cp DSK_ACT_MENU
        jr z,prgprz1
        cp DSK_ACT_CONTENT
        jr nz,prgprz0
prgprz1 ld hl,(App_MsgBuf+8)
        ld a,h
        or h
        jr z,prgprz0
        jp (hl)

;### PRGEND -> End program
prgend  ld hl,(App_BegCode+prgpstnum)
        call SySystem_PRGEND
prgend0 rst #30
        jr prgend0

;### PRGINF -> open info window
prginf  ld hl,prgmsginf
        call prginf0
        jp prgprz0
prginf0 ld a,(App_BnkNum)       ;** PUZZLE SOLVED
        ld b,8*2+1+64+128
        ld de,wingam
        jp SySystem_SYSWRN

;### PRGHLP -> shows help
prghlp  call SySystem_HLPOPN
        jp prgprz0

;### PRGKEY -> key has been clicked
prgkey  ld a,(App_MsgBuf+4)
        call clcucs
        cp 136:jp z,movup
        cp 137:jp z,movdwn
        cp 138:jp z,movlft
        cp 139:jp z,movrgt
        ;...
        jr prgprz0


;==============================================================================
;### GAME CONTROL #############################################################
;==============================================================================

;### BUTGO -> (Re)Start selected Map
butgo   ld a,(mapsel)
        ld (mapnum),a
        jp mapslv1

;### BUTNXT -> Next Map
butnxt  ld a,(mapsel)
        inc a
        cp mapcnt
        jr butpre1

;### BUTPRE -> Previous Map
butpre  ld a,(mapsel)
        dec a
        cp -1
butpre1 jp z,prgprz0
        ld (mapsel),a
        call mapdsp
        jp prgprz0


;==============================================================================
;### MOVE ROUTINES ############################################################
;==============================================================================

;newadd  oldadd  newdir
;   0       0       0   bl
;   1       0       0   ul
;   0       2       0   br
;   1       2       0   ur
;   0       0       4   ur
;   1       0       4   ul
;   0       2       4   br
;   1       2       4   bl
movtab  dw cat_bl,cat_ul,cat_br,cat_ur
        dw cat_ur,cat_ul,cat_br,cat_bl

movold  db -1   ;old direction add
movnew  dw 0    ;new direction type/add
movdif  dw 0    ;new direction dif

movbmp1 dw 0    ;first tale bitmap
movbmp2 dw 0    ;second tale bitmap

movctr  db 0    ;control ID of current position

movup   ld de,256*0+1:ld bc,256*255+000:jr movdir
movdwn  ld de,256*1+1:ld bc,256*001+000:jr movdir
movlft  ld de,256*0+5:ld bc,256*000+255:jr movdir
movrgt  ld de,256*1+5:ld bc,256*000+001:jr movdir

;### MOVDIR -> tries to move into a direction
;### Input      E=direction (1=vertical, 5=horizontal), D=neg(0)/pos(1), C=xdif, B=ydif
movdir  ld (movnew),de
        ld (movdif),bc
        bit 2,e
        ld hl,cat_ver       ;bitmap2 depends on direction type
        jr z,movdir1
        ld hl,cat_hor
movdir1 ld (movbmp1),hl
        ld (movbmp2),hl
        ld a,(movold)
        cp -1
        jr z,movdir2
        add e               ;calculate curve type for bitmap1
        add d
        add a
        ld c,a
        ld b,0
        ld hl,movtab-2
        add hl,bc
        ld de,movbmp1
        ldi:ldi
movdir2 ld a,(movnew+1)
        add a
        push af
        call movdir3
        pop bc
        jr c,movdir6       
        ld l,SND_SYS_SHOOT      ;*** wall, no movement, go back
        call sndefx
        jp prgprz0
movdir6 ld a,b
        ld (movold),a
        push hl
        ld l,SND_SYS_SLIDR2
        call sndefx
        pop hl
movdir4 push hl             ;cat is moving
        ld hl,(catxps)
        push hl
        call mapadr
        ld (hl),7           ;block old position
        pop hl
        call mapctr
        ex de,hl
        ld hl,movbmp1       ;show tale at old position
        push hl
        ldi:ldi
        pop de
        ldi:ldi             ;copy bitmap2 to bitmap1
        call movshw
        pop hl
        ld (catxps),hl      ;update new cat position
        call mapcat0        ;show cat face at new position
        call movshw
        rst #30

        ld hl,mapfre
        dec (hl)
        jr z,mapslv         ;all fields filled -> finished, next level
        call movdir3
        jr c,movdir4        ;continue until wall reached
        ld l,SND_SYS_STEP       ;*** wall reached
        call sndefx
        ld bc,256*001+000:call movdir5:jp c,prgprz0
        ld bc,256*255+000:call movdir5:jp c,prgprz0
        ld bc,256*000+001:call movdir5:jp c,prgprz0
        ld bc,256*000+255:call movdir5:jp c,prgprz0
        jr mapded

movdir3 ld bc,(movdif)
movdir5 ld hl,(catxps)
        ld a,c
        add l
        ld l,a
        ld a,b
        add h
        ld h,a
        push hl
        call mapadr
        ld a,(hl)
        pop hl
        cp 7
        ret

movshw  ld hl,(catxps)
        call mapctr0
        ld a,wingam_map_num
        add l
        ld e,a
        jp dskdrw


;==============================================================================
;### MAP ROUTINES #############################################################
;==============================================================================

mapnum  dw 0
mapsel  db 0    ;select mapnumber

mapfre  db 0    ;number of free fields left
catxps  db 0
catyps  db 0

;### MAPSLV -> map solved
mapslv  ld l,SND_SYS_WIN
        call mapded0
        ld a,(mapnum)
        inc a
        cp mapcnt
        jr nz,mapslv2
        ld hl,prgmsgwin
        call prginf0
        xor a
mapslv2 ld (mapnum),a
        ld (mapsel),a
        call mapdsp
mapslv1 call mapget
        ld e,-1
        ld hl,16
        ld bc,16
        ld ix,16*10
        ld iy,16*6
        ld a,(prgwin)
        call SyDesktop_WINPIN
        jp prgprz0

;### MAPDED -> dead end, replay map
mapded  ld l,SND_SYS_LOSE
        call mapded0
        jr mapslv1
mapded0 call sndefx
        ld b,50
mapded1 rst #30
        djnz mapded1
        ret

;### MAPDSP -> display selected map number
;### Input      A=(mapsel)
mapdsp  inc a
        call clcdez
        ld (txtlevval),hl
        ld e,wingam_dsp_num
        jp dskdrw


;### MAPGET -> loads map into buffer and gui controls
mapgett dw map_0,map_1,map_2,map_3,map_4,map_5,map_6,map_7
        ; 111 110 101 100 011 010 001 000
mapgets db  6,  2,  6,  3,  4,  1,  5,  0

mapget  ld hl,(mapnum)
        ld e,l:ld d,h
        add hl,hl
        add hl,hl
        add hl,hl
        add hl,de
        ld de,mapdat
        add hl,de
        ld a,(hl)
        rrca:rrca:rrca:rrca
        and #0f
        ld (catxps),a
        ld a,(hl)
        and #0f
        ld (catyps),a
        inc hl
        ld de,mapmem+16+1   ;copy map data
        ld ix,256*255+8
        ld iyh,mapylen
mapget1 ld iyl,mapxlen
mapget2 ld a,(hl)
        ld b,ixl
mapget3 rra
        djnz mapget3
        ld a,7
        jr c,mapget5
        ld a,0
        inc ixh
mapget5 ld (de),a
        inc de
        dec ixl
        jr nz,mapget6
        inc hl
        ld ixl,8
mapget6 dec iyl
        jr nz,mapget2
        ld bc,16-mapxlen
        ex de,hl
        add hl,bc
        ex de,hl
        dec iyh
        jr nz,mapget1
        ld a,ixh
        ld (mapfre),a

        ld ix,mapmem+16     ;generate shadows
        ld b,16*6
        ld d,0
mapget7 ld a,(ix+0)
        or a
        jr nz,mapget8
        ld e,a
        ld a,(ix-16+0):cp 7:rl e
        ld a,(ix-16+1):cp 7:rl e
        ld a,(ix+00+1):cp 7:rl e
        ld hl,mapgets
        add hl,de
        ld a,(hl)
        ld (ix+0),a
mapget8 inc ix
        djnz mapget7

        ld hl,256*1+1
mapget4 push hl
        push hl
        call mapadr
        ld a,(hl)
        add a
        ld c,a
        ld b,0
        ld hl,mapgett
        add hl,bc
        ex (sp),hl
        call mapctr
        ex de,hl
        pop hl
        ldi:ldi
        pop hl
        inc l
        ld a,mapxlen+1
        cp l
        jr nz,mapget4
        ld l,1
        inc h
        ld a,mapylen+1
        cp h
        jr nz,mapget4
        call mapcat
        ld a,-1
        ld (movold),a
        ret

;### MAPCAT -> sets cat head control at current position
mapcat  ld hl,(catxps)
mapcat0 call mapctr
        ld de,cat_f1
        ld (hl),e
        inc hl
        ld (hl),d
        ret

;### MAPCTR -> calculate map gui control address
;### Input      L=xpos, H=ypos
;### Output     HL=address of bitmap pointer
mapctr  call mapctr0
        add hl,hl
        add hl,hl
        add hl,hl
        add hl,hl
        ld de,wingam_map+4
        add hl,de
        ret
mapctr0 dec l:dec h
        ld c,l
        ld l,mapxlen+2
        call clcmu8
        ld b,0
        add hl,bc
        ret

;### MAPADR -> calculate mapmem address
;### Input      L=xpos, H=ypos
;### Output     HL=address
mapadr  ld c,l
        ld b,0
        ld l,h
        ld h,b
        add hl,hl
        add hl,hl
        add hl,hl
        add hl,hl
        add hl,bc
        ld bc,mapmem
        add hl,bc
        ret

;==============================================================================
;### SOUND-ROUTINES ###########################################################
;==============================================================================

snddvcflg   db 0    ;0=no device, 1=psg, 2=wavetable

;### SNDINI -> inits sound support
sndini  call SySound_SNDINI         ;search and set Sound Daemon
        ret c
        ld a,l
        ld (snddvcflg),a
        ret

;### SNDEFX -> play sound effect
;### Input      L=system effect ID
sndefx  dec l
        ld a,(snddvcflg)
        cp 1
        ret c
        ld a,0
        ld h,255            ;volume=255
        ld b,3              ;psg -> play on rottating channel
        jr z,sndefx1
        ld bc,256*1+128     ;opl4 -> play, panning=center
        ld de,0             ;        pitch=default
sndefx1 jp SySound_EFXPLY


;==============================================================================
;### SUB-ROUTINES #############################################################
;==============================================================================

;### CLCUCS -> upper case
;### Input      A=char
;### Output     A=ucase(A)
;### Destroyed  F
clcucs  cp "a"
        ret c
        cp "z"+1
        ret nc
        add "A"-"a"
        ret

;### CLCMU8 -> 8bit unsigned multiplication
;### Input      L,H=values
;### Output     HL=L*H
;### Destroyed  F,B,DE
clcmu8  ld e,l
        ld l,0
        ld d,0
        ld b,8
clcmu81 add hl,hl
        jr nc,clcmu82
        add hl,de
clcmu82 djnz clcmu81
        ret

;### CLCDIV -> division
;### Output     L=HL/C, H=HL mod C
;### Destroyed  AF,BC
clcdiv  ld b,8
clcdiv1 add hl,hl       ;3
        ld a,h          ;1
        sub c           ;1
        jr c,clcdiv2    ;2/3
        ld h,a          ;1
        inc l           ;1 9/8
clcdiv2 djnz clcdiv1
        ret

;### CLCDEZ -> Calculates 2digit decimal string
;### Input      A=value
;### Output     L=10er digit, H=1er digit
;### Veraendert AF
clcdez  ld l,"0"
clcdez1 sub 10
        jr c,clcdez2
        inc l
        jr clcdez1
clcdez2 add "0"+10
        ld h,a
        ret

;### DSKDRW -> redraws one or more controls
;### Input      E,D=controls
dskdrw  ld a,(prgwin)
        jp SyDesktop_WINDIN


;==============================================================================
;### MAP DATA #################################################################
;==============================================================================

maptot  equ 3
mapxlen equ 10
mapylen equ 6

mapdat

READ "App-Longcat-Levels.asm"

mapmem      ;current map
db 7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7
db 7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7
db 7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7
db 7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7
db 7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7
db 7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7
db 7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7
db 7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7


;==============================================================================
;### DATA AREA ################################################################
;==============================================================================

App_BegData

prgicn16c db 12,24,24:dw $+7:dw $+4,12*24:db 5
db #88,#88,#88,#81,#88,#88,#88,#88,#18,#88,#88,#88,#88,#88,#88,#13,#18,#88,#88,#81,#31,#88,#88,#88,#88,#88,#81,#e3,#31,#11,#11,#13,#3e,#18,#88,#88,#88,#88,#13,#e3,#33,#33,#33,#33,#3e,#31,#88,#88
db #88,#88,#13,#33,#33,#33,#33,#33,#33,#31,#88,#88,#88,#81,#33,#33,#33,#33,#33,#33,#33,#33,#18,#88,#88,#81,#33,#33,#33,#33,#33,#33,#33,#33,#18,#88,#88,#81,#33,#31,#13,#33,#33,#31,#13,#33,#18,#88
db #81,#11,#33,#31,#83,#33,#33,#31,#83,#33,#11,#18,#88,#81,#11,#31,#13,#33,#33,#31,#13,#11,#18,#88,#81,#11,#33,#33,#88,#8e,#e8,#88,#33,#33,#11,#18,#88,#81,#11,#38,#88,#88,#88,#88,#83,#11,#18,#88
db #88,#81,#33,#88,#18,#81,#18,#81,#88,#33,#18,#88,#88,#88,#18,#88,#81,#18,#81,#18,#88,#81,#88,#88,#88,#88,#81,#18,#88,#88,#88,#88,#81,#18,#88,#88,#88,#88,#88,#f1,#11,#11,#11,#11,#17,#88,#88,#88
db #88,#88,#88,#ff,#22,#cc,#aa,#66,#77,#88,#88,#88,#88,#88,#88,#ff,#22,#cc,#aa,#66,#77,#88,#88,#88,#88,#88,#8f,#f2,#2c,#ca,#a6,#67,#78,#88,#88,#88,#88,#88,#8f,#f2,#2c,#ca,#a6,#67,#78,#88,#88,#88
db #88,#88,#8f,#f2,#2c,#ca,#a6,#67,#78,#88,#88,#88,#88,#88,#88,#ff,#22,#cc,#aa,#66,#77,#88,#88,#88,#88,#88,#88,#ff,#22,#cc,#aa,#66,#77,#88,#88,#88,#88,#88,#88,#ff,#22,#cc,#aa,#66,#77,#88,#88,#88


;### MAP ELEMENTS #############################################################

map_0   db 8,16,16:dw $+7,$+4,8*16:db 5
db #88,#88,#88,#88,#88,#88,#88,#88
db #88,#88,#88,#88,#88,#88,#88,#88
db #88,#88,#88,#88,#88,#88,#88,#88
db #88,#88,#88,#88,#88,#88,#88,#88
db #88,#88,#88,#88,#88,#88,#88,#88
db #88,#88,#88,#88,#88,#88,#88,#88
db #88,#88,#88,#88,#88,#88,#88,#88
db #88,#88,#88,#88,#88,#88,#88,#88
db #88,#88,#88,#88,#88,#88,#88,#88
db #88,#88,#88,#88,#88,#88,#88,#88
db #88,#88,#88,#88,#88,#88,#88,#88
db #88,#88,#88,#88,#88,#88,#88,#88
db #88,#88,#88,#88,#88,#88,#88,#88
db #88,#88,#88,#88,#88,#88,#88,#88
db #88,#88,#88,#88,#88,#88,#88,#88
db #88,#88,#88,#88,#88,#88,#88,#88

map_1   db 8,16,16:dw $+7,$+4,8*16:db 5
db #88,#88,#88,#88,#81,#81,#81,#81
db #88,#88,#88,#88,#18,#18,#18,#18
db #88,#88,#88,#88,#81,#81,#81,#81
db #88,#88,#88,#88,#18,#18,#18,#18
db #88,#88,#88,#88,#81,#81,#81,#81
db #88,#88,#88,#88,#18,#18,#18,#18
db #88,#88,#88,#88,#81,#81,#81,#81
db #88,#88,#88,#88,#18,#18,#18,#18
db #88,#88,#88,#88,#88,#88,#88,#88
db #88,#88,#88,#88,#88,#88,#88,#88
db #88,#88,#88,#88,#88,#88,#88,#88
db #88,#88,#88,#88,#88,#88,#88,#88
db #88,#88,#88,#88,#88,#88,#88,#88
db #88,#88,#88,#88,#88,#88,#88,#88
db #88,#88,#88,#88,#88,#88,#88,#88
db #88,#88,#88,#88,#88,#88,#88,#88

map_2   db 8,16,16:dw $+7,$+4,8*16:db 5
db #81,#81,#81,#81,#81,#81,#81,#81
db #18,#18,#18,#18,#18,#18,#18,#18
db #81,#81,#81,#81,#81,#81,#81,#81
db #18,#18,#18,#18,#18,#18,#18,#18
db #81,#81,#81,#81,#81,#81,#81,#81
db #18,#18,#18,#18,#18,#18,#18,#18
db #81,#81,#81,#81,#81,#81,#81,#81
db #18,#18,#18,#18,#18,#18,#18,#18
db #88,#88,#88,#88,#88,#88,#88,#88
db #88,#88,#88,#88,#88,#88,#88,#88
db #88,#88,#88,#88,#88,#88,#88,#88
db #88,#88,#88,#88,#88,#88,#88,#88
db #88,#88,#88,#88,#88,#88,#88,#88
db #88,#88,#88,#88,#88,#88,#88,#88
db #88,#88,#88,#88,#88,#88,#88,#88
db #88,#88,#88,#88,#88,#88,#88,#88

map_3   db 8,16,16:dw $+7,$+4,8*16:db 5
db #81,#81,#81,#81,#81,#81,#81,#81
db #18,#18,#18,#18,#18,#18,#18,#18
db #81,#81,#81,#81,#81,#81,#81,#88
db #18,#18,#18,#18,#18,#18,#18,#88
db #81,#81,#81,#81,#81,#81,#88,#88
db #18,#18,#18,#18,#18,#18,#88,#88
db #81,#81,#81,#81,#81,#88,#88,#88
db #18,#18,#18,#18,#18,#88,#88,#88
db #88,#88,#88,#88,#88,#88,#88,#88
db #88,#88,#88,#88,#88,#88,#88,#88
db #88,#88,#88,#88,#88,#88,#88,#88
db #88,#88,#88,#88,#88,#88,#88,#88
db #88,#88,#88,#88,#88,#88,#88,#88
db #88,#88,#88,#88,#88,#88,#88,#88
db #88,#88,#88,#88,#88,#88,#88,#88
db #88,#88,#88,#88,#88,#88,#88,#88

map_4   db 8,16,16:dw $+7,$+4,8*16:db 5
db #88,#88,#88,#88,#81,#81,#81,#81
db #88,#88,#88,#88,#18,#18,#18,#18
db #88,#88,#88,#88,#81,#81,#81,#81
db #88,#88,#88,#88,#18,#18,#18,#18
db #88,#88,#88,#88,#81,#81,#81,#81
db #88,#88,#88,#88,#18,#18,#18,#18
db #88,#88,#88,#88,#81,#81,#81,#81
db #88,#88,#88,#88,#18,#18,#18,#18
db #88,#88,#88,#88,#81,#81,#81,#81
db #88,#88,#88,#88,#18,#18,#18,#18
db #88,#88,#88,#88,#81,#81,#81,#81
db #88,#88,#88,#88,#18,#18,#18,#18
db #88,#88,#88,#88,#81,#81,#81,#81
db #88,#88,#88,#88,#18,#18,#18,#18
db #88,#88,#88,#88,#81,#81,#81,#81
db #88,#88,#88,#88,#18,#18,#18,#18

map_5   db 8,16,16:dw $+7,$+4,8*16:db 5
db #88,#88,#88,#88,#88,#88,#88,#81
db #88,#88,#88,#88,#88,#88,#88,#18
db #88,#88,#88,#88,#88,#88,#81,#81
db #88,#88,#88,#88,#88,#88,#18,#18
db #88,#88,#88,#88,#88,#81,#81,#81
db #88,#88,#88,#88,#88,#18,#18,#18
db #88,#88,#88,#88,#81,#81,#81,#81
db #88,#88,#88,#88,#18,#18,#18,#18
db #88,#88,#88,#88,#81,#81,#81,#81
db #88,#88,#88,#88,#18,#18,#18,#18
db #88,#88,#88,#88,#81,#81,#81,#81
db #88,#88,#88,#88,#18,#18,#18,#18
db #88,#88,#88,#88,#81,#81,#81,#81
db #88,#88,#88,#88,#18,#18,#18,#18
db #88,#88,#88,#88,#81,#81,#81,#81
db #88,#88,#88,#88,#18,#18,#18,#18

map_6   db 8,16,16:dw $+7,$+4,8*16:db 5
db #81,#81,#81,#81,#81,#81,#81,#81
db #18,#18,#18,#18,#18,#18,#18,#18
db #81,#81,#81,#81,#81,#81,#81,#81
db #18,#18,#18,#18,#18,#18,#18,#18
db #81,#81,#81,#81,#81,#81,#81,#81
db #18,#18,#18,#18,#18,#18,#18,#18
db #81,#81,#81,#81,#81,#81,#81,#81
db #18,#18,#18,#18,#18,#18,#18,#18
db #88,#88,#88,#88,#81,#81,#81,#81
db #88,#88,#88,#88,#18,#18,#18,#18
db #88,#88,#88,#88,#81,#81,#81,#81
db #88,#88,#88,#88,#18,#18,#18,#18
db #88,#88,#88,#88,#81,#81,#81,#81
db #88,#88,#88,#88,#18,#18,#18,#18
db #88,#88,#88,#88,#81,#81,#81,#81
db #88,#88,#88,#88,#18,#18,#18,#18

map_7   db 8,16,16:dw $+7,$+4,8*16:db 5
db #77,#77,#77,#17,#77,#77,#77,#17
db #77,#77,#77,#71,#77,#77,#77,#71
db #17,#77,#77,#77,#17,#77,#67,#77
db #71,#77,#77,#77,#71,#77,#77,#77
db #77,#17,#77,#77,#77,#17,#77,#77
db #77,#71,#77,#77,#77,#71,#77,#77
db #77,#77,#17,#77,#67,#77,#17,#77
db #77,#77,#71,#77,#77,#77,#71,#77
db #77,#77,#77,#17,#77,#77,#77,#17
db #77,#77,#77,#71,#77,#77,#77,#71
db #17,#77,#67,#77,#17,#77,#77,#77
db #71,#77,#77,#77,#71,#77,#77,#77
db #77,#17,#77,#77,#77,#17,#77,#77
db #77,#71,#77,#77,#77,#71,#77,#77
db #77,#77,#17,#77,#77,#77,#17,#77
db #77,#77,#71,#77,#77,#77,#71,#77

;### CAT ELEMENTS #############################################################

cat_hor db 8,16,16:dw $+7,$+4,8*16:db 5
db #88,#88,#88,#88,#88,#88,#88,#88
db #ff,#f8,#88,#88,#88,#8f,#ff,#88
db #ff,#ff,#ff,#88,#ff,#ff,#ff,#ff
db #22,#2f,#ff,#ff,#ff,#f2,#22,#ff
db #22,#22,#22,#ff,#22,#22,#22,#22
db #cc,#c2,#22,#22,#22,#2c,#cc,#22
db #cc,#cc,#cc,#22,#cc,#cc,#cc,#cc
db #aa,#ac,#cc,#cc,#cc,#ca,#aa,#cc
db #aa,#aa,#aa,#cc,#aa,#aa,#aa,#aa
db #66,#6a,#aa,#aa,#aa,#a6,#66,#aa
db #66,#66,#66,#aa,#66,#66,#66,#66
db #77,#76,#66,#66,#66,#67,#77,#66
db #77,#77,#77,#66,#77,#77,#77,#77
db #88,#87,#77,#77,#77,#78,#88,#77
db #88,#88,#88,#77,#88,#88,#88,#88
db #88,#88,#88,#88,#88,#88,#88,#88

cat_ver db 8,16,16:dw $+7,$+4,8*16:db 5
db #88,#ff,#22,#cc,#aa,#66,#77,#88
db #88,#ff,#22,#cc,#aa,#66,#77,#88
db #8f,#f2,#2c,#ca,#a6,#67,#78,#88
db #8f,#f2,#2c,#ca,#a6,#67,#78,#88
db #8f,#f2,#2c,#ca,#a6,#67,#78,#88
db #88,#ff,#22,#cc,#aa,#66,#77,#88
db #88,#ff,#22,#cc,#aa,#66,#77,#88
db #88,#ff,#22,#cc,#aa,#66,#77,#88
db #88,#8f,#f2,#2c,#ca,#a6,#67,#78
db #88,#8f,#f2,#2c,#ca,#a6,#67,#78
db #88,#ff,#22,#cc,#aa,#66,#77,#88
db #88,#ff,#22,#cc,#aa,#66,#77,#88
db #88,#ff,#22,#cc,#aa,#66,#77,#88
db #8f,#f2,#2c,#ca,#a6,#67,#78,#88
db #8f,#f2,#2c,#ca,#a6,#67,#78,#88
db #8f,#f2,#2c,#ca,#a6,#67,#78,#88

cat_ul  db 8,16,16:dw $+7,$+4,8*16:db 5
db #88,#88,#88,#88,#88,#88,#88,#88
db #88,#88,#88,#88,#88,#88,#88,#88
db #88,#ff,#ff,#ff,#ff,#ff,#ff,#ff
db #88,#ff,#ff,#ff,#ff,#ff,#ff,#ff
db #88,#ff,#22,#22,#22,#22,#22,#22
db #88,#ff,#22,#22,#22,#22,#22,#22
db #88,#ff,#22,#cc,#cc,#cc,#cc,#cc
db #88,#ff,#22,#cc,#cc,#cc,#cc,#cc
db #88,#ff,#22,#cc,#aa,#aa,#aa,#aa
db #88,#ff,#22,#cc,#aa,#aa,#aa,#aa
db #88,#ff,#22,#cc,#aa,#66,#66,#66
db #88,#ff,#22,#cc,#aa,#66,#66,#66
db #88,#ff,#22,#cc,#aa,#66,#77,#77
db #88,#ff,#22,#cc,#aa,#66,#77,#77
db #88,#ff,#22,#cc,#aa,#66,#77,#88
db #88,#ff,#22,#cc,#aa,#66,#77,#88

cat_bl  db 8,16,16:dw $+7,$+4,8*16:db 5
db #88,#ff,#22,#cc,#aa,#66,#77,#88
db #88,#ff,#22,#cc,#aa,#66,#77,#88
db #88,#ff,#22,#cc,#aa,#66,#77,#ff
db #88,#ff,#22,#cc,#aa,#66,#7f,#ff
db #88,#ff,#22,#cc,#aa,#66,#22,#22
db #88,#ff,#22,#cc,#aa,#62,#22,#22
db #88,#ff,#22,#cc,#aa,#cc,#cc,#cc
db #88,#ff,#22,#cc,#ac,#cc,#cc,#cc
db #88,#ff,#22,#cc,#aa,#aa,#aa,#aa
db #88,#ff,#22,#ca,#aa,#aa,#aa,#aa
db #88,#ff,#22,#66,#66,#66,#66,#66
db #88,#ff,#26,#66,#66,#66,#66,#66
db #88,#ff,#77,#77,#77,#77,#77,#77
db #88,#f7,#77,#77,#77,#77,#77,#77
db #88,#88,#88,#88,#88,#88,#88,#88
db #88,#88,#88,#88,#88,#88,#88,#88

cat_ur  db 8,16,16:dw $+7,$+4,8*16:db 5
db #88,#88,#88,#88,#88,#88,#88,#88
db #88,#88,#88,#88,#88,#88,#88,#88
db #ff,#ff,#ff,#ff,#ff,#ff,#ff,#88
db #ff,#ff,#ff,#ff,#ff,#ff,#f7,#88
db #22,#22,#22,#22,#22,#22,#77,#88
db #22,#22,#22,#22,#22,#26,#77,#88
db #cc,#cc,#cc,#cc,#cc,#66,#77,#88
db #cc,#cc,#cc,#cc,#ca,#66,#77,#88
db #aa,#aa,#aa,#aa,#aa,#66,#77,#88
db #aa,#aa,#aa,#ac,#aa,#66,#77,#88
db #66,#66,#66,#cc,#aa,#66,#77,#88
db #66,#66,#62,#cc,#aa,#66,#77,#88
db #77,#77,#22,#cc,#aa,#66,#77,#88
db #77,#7f,#22,#cc,#aa,#66,#77,#88
db #88,#ff,#22,#cc,#aa,#66,#77,#88
db #88,#ff,#22,#cc,#aa,#66,#77,#88

cat_br  db 8,16,16:dw $+7,$+4,8*16:db 5
db #88,#ff,#22,#cc,#aa,#66,#77,#88
db #88,#ff,#22,#cc,#aa,#66,#77,#88
db #ff,#ff,#22,#cc,#aa,#66,#77,#88
db #ff,#ff,#22,#cc,#aa,#66,#77,#88
db #22,#22,#22,#cc,#aa,#66,#77,#88
db #22,#22,#22,#cc,#aa,#66,#77,#88
db #cc,#cc,#cc,#cc,#aa,#66,#77,#88
db #cc,#cc,#cc,#cc,#aa,#66,#77,#88
db #aa,#aa,#aa,#aa,#aa,#66,#77,#88
db #aa,#aa,#aa,#aa,#aa,#66,#77,#88
db #66,#66,#66,#66,#66,#66,#77,#88
db #66,#66,#66,#66,#66,#66,#77,#88
db #77,#77,#77,#77,#77,#77,#77,#88
db #77,#77,#77,#77,#77,#77,#77,#88
db #88,#88,#88,#88,#88,#88,#88,#88
db #88,#88,#88,#88,#88,#88,#88,#88

cat_f1 db 8,16,16:dw $+7,$+4,8*16:db 5
db #88,#88,#18,#88,#88,#81,#88,#88
db #88,#81,#31,#88,#88,#13,#18,#88
db #88,#1e,#33,#11,#11,#33,#e1,#88
db #81,#3e,#33,#33,#33,#33,#e3,#18
db #81,#33,#33,#33,#33,#33,#33,#18
db #13,#33,#33,#33,#33,#33,#33,#31
db #13,#33,#33,#33,#33,#33,#33,#31
db #13,#33,#11,#33,#33,#11,#33,#31
db #13,#33,#18,#33,#33,#18,#33,#31
db #11,#13,#11,#33,#33,#11,#31,#11
db #13,#33,#38,#8e,#e8,#83,#33,#31
db #11,#13,#88,#88,#88,#88,#31,#11
db #13,#38,#81,#81,#18,#18,#83,#31
db #81,#88,#88,#18,#81,#88,#88,#18
db #88,#11,#88,#88,#88,#88,#11,#88
db #88,#88,#11,#11,#11,#11,#88,#88

;### STRINGS ##################################################################

wintittxt   db "Longcat",0

prgwinmentx0 db "File",0
prgwinmentx1 db "?",0

prgwinmen0tx0 db "Quit",0

prgwinmen1tx0 db "Index",0
prgwinmen1tx1 db "About",0

;### info
prgmsginf1 db "Longcat for SymbOS",0
prgmsginf2 db " Original (c) by Martin Magni",0
prgmsginf3 db " Port by Prodatron/Retroparla",0

;### win
prgmsgwin1 db "Congratulation!",0
prgmsgwin2 db " You solved level 99,",0
prgmsgwin3 db " how smart you are!",0


;### BITMAPS ##################################################################



;==============================================================================
;### TRANSFER AREA ############################################################
;==============================================================================

App_BegTrns

;### PRGPRZS -> Stack for application process
        ds 64
prgstk  ds 6*2
        dw prgprz
App_PrcID  db 0
App_MsgBuf ds 14

;### INFO-FENSTER #############################################################

prgmsginf  dw prgmsginf1,4*1+2,prgmsginf2,4*1+2,prgmsginf3,4*1+2,prgicnbig
prgmsgwin  dw prgmsgwin1,4*1+2,prgmsgwin2,4*1+2,prgmsgwin3,4*1+2,prgicnbig


;### MAIN WINDOW ##############################################################

wingam       dw #3501,0,63,3,241,128,0,0,241,128,241,128,241,128,prgicnsml,wintittxt,0,prgwinmen,wingamgrp,0,0:ds 136+14

prgwinmen  dw 2, 1+4,prgwinmentx0,prgwinmen0,0, 1+4,prgwinmentx1,prgwinmen1,0
prgwinmen0 dw 1, 1,prgwinmen0tx0,prgend
prgwinmen1 dw 3, 1,prgwinmen1tx0,prghlp,0, 1+8,0,0,0, 1,prgwinmen1tx1,prginf,0

wingamgrp    db 106,0: dw wingamdat,0,0,0,0,0,0
wingamdat
dw      0,255*256+ 0,128+7,                0,  0,192,128,0
dw      0,255*256+ 0,128+1,              192,  0,  1,128,0
dw      0,255*256+ 0,128+12,             193,  0, 48,128,0

dw 0,255*256+10,map_7,000,000,16,16,0, 0,255*256+10,map_7,016,000,16,16,0, 0,255*256+10,map_7,032,000,16,16,0, 0,255*256+10,map_7,048,000,16,16,0, 0,255*256+10,map_7,064,000,16,16,0
dw 0,255*256+10,map_7,080,000,16,16,0, 0,255*256+10,map_7,096,000,16,16,0, 0,255*256+10,map_7,112,000,16,16,0, 0,255*256+10,map_7,128,000,16,16,0, 0,255*256+10,map_7,144,000,16,16,0
dw 0,255*256+10,map_7,160,000,16,16,0, 0,255*256+10,map_7,176,000,16,16,0

dw 0,255*256+10,map_7,000,016,16,16,0

wingam_map_num  equ 16
wingam_map
dw                                     0,255*256+10,map_7,016,016,16,16,0, 0,255*256+10,map_7,032,016,16,16,0, 0,255*256+10,map_7,048,016,16,16,0, 0,255*256+10,map_7,064,016,16,16,0
dw 0,255*256+10,map_7,080,016,16,16,0, 0,255*256+10,map_7,096,016,16,16,0, 0,255*256+10,map_7,112,016,16,16,0, 0,255*256+10,map_7,128,016,16,16,0, 0,255*256+10,map_7,144,016,16,16,0
dw 0,255*256+10,map_7,160,016,16,16,0, 0,255*256+10,map_7,176,016,16,16,0

dw 0,255*256+10,map_7,000,032,16,16,0, 0,255*256+10,map_7,016,032,16,16,0, 0,255*256+10,map_7,032,032,16,16,0, 0,255*256+10,map_7,048,032,16,16,0, 0,255*256+10,map_7,064,032,16,16,0
dw 0,255*256+10,map_7,080,032,16,16,0, 0,255*256+10,map_7,096,032,16,16,0, 0,255*256+10,map_7,112,032,16,16,0, 0,255*256+10,map_7,128,032,16,16,0, 0,255*256+10,map_7,144,032,16,16,0
dw 0,255*256+10,map_7,160,032,16,16,0, 0,255*256+10,map_7,176,032,16,16,0

dw 0,255*256+10,map_7,000,048,16,16,0, 0,255*256+10,map_7,016,048,16,16,0, 0,255*256+10,map_7,032,048,16,16,0, 0,255*256+10,map_7,048,048,16,16,0, 0,255*256+10,map_7,064,048,16,16,0
dw 0,255*256+10,map_7,080,048,16,16,0, 0,255*256+10,map_7,096,048,16,16,0, 0,255*256+10,map_7,112,048,16,16,0, 0,255*256+10,map_7,128,048,16,16,0, 0,255*256+10,map_7,144,048,16,16,0
dw 0,255*256+10,map_7,160,048,16,16,0, 0,255*256+10,map_7,176,048,16,16,0

dw 0,255*256+10,map_7,000,064,16,16,0, 0,255*256+10,map_7,016,064,16,16,0, 0,255*256+10,map_7,032,064,16,16,0, 0,255*256+10,map_7,048,064,16,16,0, 0,255*256+10,map_7,064,064,16,16,0
dw 0,255*256+10,map_7,080,064,16,16,0, 0,255*256+10,map_7,096,064,16,16,0, 0,255*256+10,map_7,112,064,16,16,0, 0,255*256+10,map_7,128,064,16,16,0, 0,255*256+10,map_7,144,064,16,16,0
dw 0,255*256+10,map_7,160,064,16,16,0, 0,255*256+10,map_7,176,064,16,16,0

dw 0,255*256+10,map_7,000,080,16,16,0, 0,255*256+10,map_7,016,080,16,16,0, 0,255*256+10,map_7,032,080,16,16,0, 0,255*256+10,map_7,048,080,16,16,0, 0,255*256+10,map_7,064,080,16,16,0
dw 0,255*256+10,map_7,080,080,16,16,0, 0,255*256+10,map_7,096,080,16,16,0, 0,255*256+10,map_7,112,080,16,16,0, 0,255*256+10,map_7,128,080,16,16,0, 0,255*256+10,map_7,144,080,16,16,0
dw 0,255*256+10,map_7,160,080,16,16,0, 0,255*256+10,map_7,176,080,16,16,0

dw 0,255*256+10,map_7,000,096,16,16,0, 0,255*256+10,map_7,016,096,16,16,0, 0,255*256+10,map_7,032,096,16,16,0, 0,255*256+10,map_7,048,096,16,16,0, 0,255*256+10,map_7,064,096,16,16,0
dw 0,255*256+10,map_7,080,096,16,16,0, 0,255*256+10,map_7,096,096,16,16,0, 0,255*256+10,map_7,112,096,16,16,0, 0,255*256+10,map_7,128,096,16,16,0, 0,255*256+10,map_7,144,096,16,16,0
dw 0,255*256+10,map_7,160,096,16,16,0, 0,255*256+10,map_7,176,096,16,16,0

dw 0,255*256+10,map_7,000,112,16,16,0, 0,255*256+10,map_7,016,112,16,16,0, 0,255*256+10,map_7,032,112,16,16,0, 0,255*256+10,map_7,048,112,16,16,0, 0,255*256+10,map_7,064,112,16,16,0
dw 0,255*256+10,map_7,080,112,16,16,0, 0,255*256+10,map_7,096,112,16,16,0, 0,255*256+10,map_7,112,112,16,16,0, 0,255*256+10,map_7,128,112,16,16,0, 0,255*256+10,map_7,144,112,16,16,0
dw 0,255*256+10,map_7,160,112,16,16,0, 0,255*256+10,map_7,176,112,16,16,0

dw butpre,255*256+16, txtbutpre,     193+ 4, 14, 18,12,0    ;button "<<"
dw butnxt,255*256+16, txtbutnxt,     193+26, 14, 18,12,0    ;button ">>"

wingam_dsp_num  equ 101
dw      0,255*256+01, ctlobjdat1,    193+ 5, 30, 38, 8,0    ;level number

dw butgo ,255*256+16, txtbutres,     193+ 4, 42, 40,12,0    ;button "Restart"

dw 00,255*256+10,prgicn16c, 193+12,072,24,24,0
dw 00,255*256+10,cat_ver,   193+16,096,16,16,0
dw 00,255*256+10,cat_ver,   193+16,112,16,16,0


ctlobjdat1  dw txtlevval:db 3+0+128,2

txtbutres   db "Go!",0
txtbutpre   db "<<",0
txtbutnxt   db ">>",0
txtlevval   db "01",0

prgtrnend
