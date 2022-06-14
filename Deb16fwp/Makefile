
# simple console terminal for deb16fw
# tools: jwasm 

NAME=deb16fwp

OUTDIR=Release

ALL: $(OUTDIR) $(OUTDIR)\$(NAME).exe

$(OUTDIR):
	@mkdir $(OUTDIR)

$(OUTDIR)\$(NAME).exe: $(NAME).asm Makefile
	@jwasm -pe -nologo -Fl$* -Fo$*.exe -IInclude $(NAME).asm

