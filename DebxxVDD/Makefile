
# NMAKE makefile, creates DEBXXVDD.DLL
# tools used:
# - JWasm or Masm
# - jwlink or MS link
#
# the Win32 import libraries are copied from WinInc.

# /BASE:0x73f00000 isn't accepted anymore by MS link v7

NAME=debxxvdd

!ifndef DEBUG
DEBUG=0
!endif

!if $(DEBUG)
OUTDIR=Debug
AOPTD=-Sg -D_DEBUG
XLIB=user32.lib
!else
OUTDIR=Release
AOPTD=
!endif

AOPT= -c -nologo -Fl$* -Fo$* -coff $(AOPTD) -IInclude
#LOPTSD=/NOLOGO /MAP /SUBSYSTEM:CONSOLE /DLL /LIBPATH:Lib /MERGE:.rdata=.text /OUT:$*.DLL 
LIBS=kernel32.lib ntvdm.lib $(XLIB)

ALL: $(OUTDIR) $(OUTDIR)\$(NAME).dll

$(OUTDIR):
	@mkdir $(OUTDIR)

$(OUTDIR)\$(NAME).dll: $*.obj $(NAME).def Makefile $(NAME).lbc
#	@link.exe $*.OBJ $(LOPTSD) $(LIBS) /DEF:$(NAME).def
	@jwlink format win pe dll f $*.obj n $*.dll libpath Lib lib {$(LIBS)} op q,m=$* @$(NAME).lbc

$(OUTDIR)\$(NAME).obj: $(NAME).asm Makefile
	@jwasm $(AOPT) $(NAME).asm

