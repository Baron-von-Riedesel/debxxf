
# make deb16f.exe and deb16f.ovl
# tools used:
# - jwasm or Masm
# - jwlink or MS OMF linker (link16.exe)
# - jwlib

# HX directory - for stub dpmist16.bin ( and tool patchne if MS link is used ).
HXDIR=\hx

OUTDIR = DEB16F
NAME = deb16f
APP  = deb16f
SRCA = deb16f

!ifndef DEBUG
DEBUG=0
!endif

!if $(DEBUG)
AOPTD= -D_DEBUG
!else
AOPTD=
!endif

SRC0 = Debxxfd
SRC1 = Parser
SRC2 = Breakpnt
SRC3 = VidProcs
SRC4 = Procs3
SRC5 = Procs2
SRC6 = Keybrd
SRC7 = Procs1
SRC8 = ComProcs
SRC9 = Printf
SRC10= PrivProf
SRC11= FPtoStr

!ifndef MASM
MASM=0
!endif
!ifndef MSLINK
MSLINK=0
!endif

LIBS  = LibOmf\lib3216.lib

AOPT=/nologo /c /Sg /Fl$* /Fo$* /D?WINDOWS=0 /D?32BIT=0 /D?FLAT=0 /Isrc $(AOPTD)

!if $(MASM)
ASM   = ml.exe $(AOPT)
!else
ASM   = jwasm.exe $(AOPT)
!endif

DEBSX  = src/debxxfd.inc src/extern32.inc src/extern16.inc src/const.inc src/rmdbghlp.inc src/errors.inc

!if $(MSLINK)
LOPTS2 = /MAP:FULL/NOE/NON/ONE:NOE/A:16
!endif

DEBO1 = + $(OUTDIR)\$(SRC1).obj + $(OUTDIR)\$(SRC2).obj \
        + $(OUTDIR)\$(SRC3).obj + $(OUTDIR)\$(SRC4).obj + $(OUTDIR)\$(SRC5).obj \
        + $(OUTDIR)\$(SRC6).obj + $(OUTDIR)\$(SRC7).obj + $(OUTDIR)\$(SRC8).obj \
        + $(OUTDIR)\$(SRC9).obj + $(OUTDIR)\$(SRC10).obj + $(OUTDIR)\$(SRC11).obj
        

{src}.asm{$(OUTDIR)}.obj:
	@$(ASM) $<

ALL: $(OUTDIR) $(OUTDIR)\$(APP).EXE $(OUTDIR)\$(NAME).OVL 

$(OUTDIR):
	@mkdir $(OUTDIR)

$(OUTDIR)\$(NAME).ovl: $(OUTDIR)\$(SRC0).obj $(NAME).mak $*.lib
!if $(MSLINK)
	@link16.exe $(OUTDIR)\$(SRC0) LibOmf\disasmd.obj, $*.ovl, $*, $*.lib $(LIBS);
!else
	@jwlink.exe format dos f $(OUTDIR)\$(SRC0), LibOmf\disasmd.obj name $*.ovl lib { $*.lib $(LIBS) } op q, m=$*
!endif

$(OUTDIR)\$(NAME).lib: $(DEBO1:+=) $(NAME).mak
	@jwlib.exe -q -n -b $*.lib $(DEBO1)

$(OUTDIR)\$(SRC0).obj: src/$(SRC0).asm $(DEBSX)
#	@$(ASM) src/$(SRC0).asm

$(OUTDIR)\$(SRC1).obj: src/$(SRC1).asm $(DEBSX)
#	@$(ASM) src/$(SRC1).asm

$(OUTDIR)\$(SRC2).obj: src/$(SRC2).asm $(DEBSX)
#	@$(ASM) src/$(SRC2).asm

$(OUTDIR)\$(SRC3).obj: src/$(SRC3).asm $(DEBSX)
#	@$(ASM) src/$(SRC3).asm

$(OUTDIR)\$(SRC4).obj: src/$(SRC4).asm $(DEBSX)
#	@$(ASM) src/$(SRC4).asm

$(OUTDIR)\$(SRC5).obj: src/$(SRC5).asm $(DEBSX)
#	@$(ASM) src/$(SRC5).asm

$(OUTDIR)\$(SRC6).obj: src/$(SRC6).asm $(DEBSX)
#	@$(ASM) src/$(SRC6).asm

$(OUTDIR)\$(SRC7).obj: src/$(SRC7).asm $(DEBSX)
#	@$(ASM) src/$(SRC7).asm

$(OUTDIR)\$(SRC8).obj: src/$(SRC8).asm $(DEBSX)
#	@$(ASM) src/$(SRC8).asm

$(OUTDIR)\$(SRC9).obj: src/$(SRC9).asm $(DEBSX)
#	@$(ASM) src/$(SRC9).asm

$(OUTDIR)\$(SRC10).obj: src/$(SRC10).asm $(DEBSX)
#	@$(ASM) src/$(SRC10).asm

$(OUTDIR)\$(SRC11).obj: src/$(SRC11).asm $(DEBSX)
#	@$(ASM) src/$(SRC11).asm

$(OUTDIR)\$(APP).exe: $(OUTDIR)\$(SRCA).obj src/$(APP).def $(NAME).mak
!if $(MSLINK)
	@link16.exe $(OUTDIR)\$(SRCA).obj $(LOPTS2), $*.exe, $*a.map, , src/deb16f.def
	@$(HXDIR)\bin\patchNE $*.exe
!else
	@jwlink.exe format win dpmi f $(OUTDIR)\$(SRCA).obj name $*.exe op m=$*a.map op q, stub=$(HXDIR)\bin\dpmist16.bin, stack=2048
!endif

$(OUTDIR)\$(SRCA).obj: src/$(SRCA).asm src/debxxf.inc $(NAME).mak
#	@$(ASM) src/$(SRCA).asm

clean:
	@erase $(OUTDIR)\*.obj
	@erase $(OUTDIR)\*.lib
	@erase $(OUTDIR)\*.lst
	@erase $(OUTDIR)\*.map
	@erase $(OUTDIR)\*.ovl
	@erase $(OUTDIR)\*.exe
