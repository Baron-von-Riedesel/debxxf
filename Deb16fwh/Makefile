
# uses MS VC 7.1 ( Visual C++ Toolkit 2003 )
# No C runtime is used.
# Win32 import libs from WinInc.

!ifndef DEBUG
DEBUG=0
!endif

!if $(DEBUG)
OUTDIR=Debug
COPTD=-D_DEBUG
!else
OUTDIR=Release
COPTD=
!endif

NAME=deb16fwh

BIN32DIR=\msvc71\Bin
INC32DIR=\msvc71\Include
WININC=\mssdk\Include
WINLIB=\wininc\Lib

#CC=$(BIN32DIR)\cl -c -Ox -G3s -D_ML -I$(INC32DIR) -I$(WININC) -Fo$* -FAsc $(COPTD)
CC=$(BIN32DIR)\cl -c -Ox -G3s -D_ML -I$(INC32DIR) -I$(WININC) -Fo$* $(COPTD)
LINK=$(BIN32DIR)\link
LIBS=kernel32.lib user32.lib

ALL: $(OUTDIR) $(OUTDIR)\$(NAME).dll

$(OUTDIR):
	@mkdir $(OUTDIR)

$(OUTDIR)\$(NAME).dll: $*.obj $(NAME).def Makefile
#	$(LINK) $*.obj /LIBPATH:$(WINLIB) /MAP /DLL /NOD /DEF:$(NAME).def /OUT:$*.dll /MERGE:.rdata=.text /SECTION:.text,res /SECTION:.bss,rws /SECTION:.data,rws $(LIBS) /STACK:0,0 /HEAP:0,0
	@$(LINK) $*.obj /LIBPATH:$(WINLIB) /MAP /DLL /NOD /DEF:$(NAME).def /OUT:$*.dll $(LIBS) /STACK:0,0 /HEAP:0,0

$(OUTDIR)\$(NAME).obj: $(NAME).cpp Makefile
	$(CC) $(NAME).cpp

