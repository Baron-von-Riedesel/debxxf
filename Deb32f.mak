
# make deb32f.exe and deb32f.dll
# tools used:
# - jwasm or Masm (ml.exe)
# - jwlink or MS link (link.exe)
# - jwlib

# HX directory - to find stub dpmist32.bin (and tool patchPE if MS link is used)
HXDIR=\hx


OUTDIR = DEB32F
NAME  = deb32f
APP   = deb32f
SRCA  = deb32f

!ifndef DEBUG
DEBUG=0
!endif

!ifndef MASM
MASM=0
!endif

!ifndef MSLINK
MSLINK=0
!endif

!if $(DEBUG)
AOPTD=-D_DEBUG
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
SRC10 = PrivProf
SRC11 = FPtoStr

AOPT1=/nologo /c /coff /Sg /Fl$* /Fo$* /D?WINDOWS=0 /D?32BIT=1 /D?FLAT=1 /Isrc $(AOPTD)
AOPT2=/nologo /c /coff /Sg /Fl$* /Fo$* /D?WINDOWS=0 /D?FLAT=1

!if $(MASM)
ASM   = ml.exe $(AOPT1)
ASMA  = ml.exe $(AOPT2)
!else
ASM   = jwasm.exe $(AOPT1)
ASMA  = jwasm.exe $(AOPT2)
!endif

LIBS  = LibCoff\lib32n.lib
MSLOPTS = /DLL /NODEFAULTLIB /MERGE:Strings=.text /MERGE:CConst=.text /MERGE:CCONST=.text /MERGE:.CRT=.data /MERGE:.stream=.data /MERGE:.BASE=.data /MERGE:.rdata=.text

MODS2 =

DEBSX = src/debxxfd.inc src/extern32.inc src/extern16.inc src/const.inc src/rmdbghlp.inc src/errors.inc

DEBO1 = + $(OUTDIR)\$(SRC0).obj + $(OUTDIR)\$(SRC1).obj + $(OUTDIR)\$(SRC2).obj \
        + $(OUTDIR)\$(SRC3).obj + $(OUTDIR)\$(SRC4).obj + $(OUTDIR)\$(SRC5).obj \
        + $(OUTDIR)\$(SRC6).obj + $(OUTDIR)\$(SRC7).obj + $(OUTDIR)\$(SRC8).obj \
        + $(OUTDIR)\$(SRC9).obj + $(OUTDIR)\$(SRC10).obj + $(OUTDIR)\$(SRC11).obj

{src}.asm{$(OUTDIR)}.obj:
	@$(ASM) $<
        
ALL: $(OUTDIR) $(OUTDIR)\$(APP).exe $(OUTDIR)\$(NAME).dll 

$(OUTDIR):
	@mkdir $(OUTDIR)

$(OUTDIR)\$(NAME).dll: $(OUTDIR)\$(NAME).LIB $(NAME).mak
!if $(MSLINK)
	@link.exe $(OUTDIR)\$(SRC0).OBJ $*.LIB $(LIBS) $(MSLOPTS) /OUT:$*.dll /MAP:$*.map /BASE:0x320000
!else
	@jwlink.exe format win pe dll f $(OUTDIR)\$(SRC0).OBJ, LibCoff\disasms.obj name $*.dll lib { $*.LIB $(LIBS) } op q, m=$*.map, offset=0x320000
!endif

$(OUTDIR)\$(NAME).LIB: $(DEBO1:+=) $(NAME).mak
	@jwlib.exe -q -n -b $*.LIB $(DEBO1)

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

# dont make the stack of deb32f too small
# if the debugger loads a 32bit dll without an app,
# this stack will be used!

# also, the debugger needs support for NE binaries
# which makes the other stubs (DPMILD32,...) not suitable

$(OUTDIR)\$(APP).exe: $(OUTDIR)\$(SRCA).obj $(NAME).mak
!if $(MSLINK)
	@link.exe $*.obj /OUT:$*.exe /STUB:$(HXDIR)\bin\DPMIST32.BIN /FIXED:NO /MAP:$*a.map /HEAP:0x4000 /STACK:0x10000 /SUBSYSTEM:CONSOLE /BASE:0x300000 
	@$(HXDIR)\BIN\patchPE.exe $*.exe
!else
	@jwlink.exe format win pe hx f $*.obj name $*.exe lib \hx\lib\dkrnl32s.lib op q, stub=$(HXDIR)\bin\DPMIST32.BIN, m=$*a.map, heapsize=0x4000, stack=0x10000, offset=0x300000
!endif

$(OUTDIR)\$(SRCA).obj: src/$(SRCA).asm src/debxxf.inc $(NAME).mak
	@$(ASMA) src/$(SRCA).asm

clean:
	@erase $(OUTDIR)\*.obj
	@erase $(OUTDIR)\*.lib
	@erase $(OUTDIR)\*.lst
	@erase $(OUTDIR)\*.map
	@erase $(OUTDIR)\*.dll
	@erase $(OUTDIR)\*.exe
