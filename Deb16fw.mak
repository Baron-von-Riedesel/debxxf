
# creates deb16fw.exe and deb16fw.ovl
# tools used:
# - jwasm or Masm (ml.exe)
# - jwlink or MS OMF linker (link16.exe)
# - jwlib
# - 16-bit resource compiler (used: Open Watcom's wrc.exe)

OUTDIR = DEB16FW
NAME   = deb16fw
APP    = deb16fw

# directory to find windows.h ( for the OW resource compiler )
#WINH=\MSsdk\Include
WINH=\watcom\H\Win

!ifndef DEBUG
DEBUG=0
!endif

!if $(DEBUG)
AOPTD=-D_DEBUG
!else
AOPTD=
!endif

!ifndef MSLINK
MSLINK=0
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
SRC11= FPtoStr

LIBS  = LibOmf\lib3216.lib

!ifndef MASM
MASM=0
!endif

AOPT1=/c /nologo /Sg /Fl$* /Fo$* /D?FLAT=0 /D?WINDOWS=1 /D?32BIT=0 /Isrc $(AOPTD)
AOPT2=-c -nologo -Sg -Fl$* -Fo$* -Cp -D?FLAT=0

!if $(MASM)
ASM   = ml.exe $(AOPT1)
ASMA  = ml.exe $(AOPT2)
!else
ASM   = jwasm.exe $(AOPT1)
ASMA  = jwasm.exe $(AOPT2)
!endif

DEBSX = src/debxxfd.inc src/extern32.inc src/extern16.inc src/const.inc src/rmdbghlp.inc src/errors.inc

DEBO1 = $(OUTDIR)\$(SRC1).obj $(OUTDIR)\$(SRC2).obj\
        $(OUTDIR)\$(SRC3).obj $(OUTDIR)\$(SRC4).obj $(OUTDIR)\$(SRC5).obj\
        $(OUTDIR)\$(SRC6).obj $(OUTDIR)\$(SRC7).obj $(OUTDIR)\$(SRC8).obj\
        $(OUTDIR)\$(SRC9).obj $(OUTDIR)\$(SRC11).obj

{src}.asm{$(OUTDIR)}.obj:
	@$(ASM) $<

ALL: $(OUTDIR) $(OUTDIR)\$(NAME).ovl $(OUTDIR)\$(NAME).exe

$(OUTDIR):
	@mkdir $(OUTDIR)

$(OUTDIR)\$(NAME).ovl: $(OUTDIR)\$(SRC0).obj $(NAME).mak $*.lib
!if $(MSLINK)
	@link16.exe /ONE:NOE/NOD/MAP:FULL/NOE/NON $(OUTDIR)\$(SRC0) LibOmf\disasmw.obj, $*.ovl, $*, $*.lib $(LIBS);
!else
	@jwlink.exe format dos f $(OUTDIR)\$(SRC0).obj,LibOmf\disasmw.obj name $*.ovl lib { $*.lib $(LIBS) } op q, m=$*.map
!endif


DEBO1 = + $(OUTDIR)\$(SRC1).obj + $(OUTDIR)\$(SRC2).obj + $(OUTDIR)\$(SRC3).obj \
        + $(OUTDIR)\$(SRC4).obj + $(OUTDIR)\$(SRC5).obj + $(OUTDIR)\$(SRC6).obj \
        + $(OUTDIR)\$(SRC7).obj + $(OUTDIR)\$(SRC8).obj + $(OUTDIR)\$(SRC9).obj + $(OUTDIR)\$(SRC11).obj

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

$(OUTDIR)\$(SRC11).obj: src/$(SRC11).asm $(DEBSX)
#	@$(ASM) src/$(SRC11).asm

# mark for windows 3.1! - else int 01 and int 03 won't be called as exceptions.

$(OUTDIR)\$(APP).exe: $*.obj src/$(APP).def src/$(APP).lbc $(NAME).mak src/$(APP).rc res/$(APP).ico
!if $(MSLINK)
	@link16.exe $* /FAR/MAP:FULL/NOE/NON/A:16/NOD, $*.exe, $*a.map, LibOmf\libw.lib, src/$(APP).def
!else
	@jwlink.exe format win f $*.obj name $*.exe lib LibOmf\libw.lib op q, m=$*a.map @src/$(APP).lbc
!endif
	@wrc.exe -q -bt=windows -31 -fo$*.res -i$(WINH) src/$(APP).rc $*.exe

$(OUTDIR)\$(APP).obj: src/$(APP).asm src/debxxf.inc $(NAME).mak
	@$(ASMA) src/$(APP).asm

clean:
	@erase $(OUTDIR)\*.obj
	@erase $(OUTDIR)\*.lib
	@erase $(OUTDIR)\*.res
	@erase $(OUTDIR)\*.lst
	@erase $(OUTDIR)\*.map
	@erase $(OUTDIR)\*.ovl
	@erase $(OUTDIR)\*.exe
