
# create debfvga.dll
# jwasm & jwlink

LIBDIR=..\LibOMF

OUTDIR=Release

name = debfvga
aopt = -nologo -Fl$* -Fo$* -Cp -Sg -D?DEBUG=0 -D?FLAT=0
lopt = /NOLOGO/MAP:FULL/A:16/NON/NOD/ONE:NOE/NOE/NOI

ALL: $(OUTDIR) $(OUTDIR)\$(name).dll

$(OUTDIR):
	@mkdir $(OUTDIR)

$(OUTDIR)\$(name).dll: $*.obj $(name).def Makefile $(name).lbc
#	@link16 $(lopt) $*.obj,$*.dll,$*.map,$(LIBDIR)\libw.lib,$(name).def
	@jwlink format win dll f $*.obj n $*.dll lib $(LIBDIR)\libw.lib op q,m=$*.map @$(name).lbc

$(OUTDIR)\$(name).obj: $(name).asm Makefile
	@jwasm $(aopt) $(name).asm



