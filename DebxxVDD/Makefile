
# NMAKE makefile, creates DEBXXVDD.DLL
# tools used: JWasm

NAME=debxxvdd

!ifndef DEBUG
DEBUG=0
!endif

!if $(DEBUG)
OUTDIR=Debug
AOPTD=-D_DEBUG
!else
OUTDIR=Release
AOPTD=
!endif

ALL: $(OUTDIR) $(OUTDIR)\$(NAME).dll

$(OUTDIR):
	@mkdir $(OUTDIR)

$(OUTDIR)\$(NAME).dll: $(NAME).asm Makefile
	@jwasm -c -nologo -pe -zze -Sg -Fl$* -Fo$*.DLL -IInclude $(AOPTD) $(NAME).asm
