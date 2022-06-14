
# create deb16fwv.dll
# uses jwasm & jwlink

!ifndef DEBUG
DEBUG=0
!endif

OUTDIR=Release

!if $(DEBUG)
AOPTD=-D_DEBUG
!else
AOPTD=
!endif

name = deb16fwv
AOPT = -nologo -Fl$* -Fo$* -Cp -Sg $(AOPTD)

ALL: $(OUTDIR) $(OUTDIR)\$(name).dll

$(OUTDIR):
	@mkdir $(OUTDIR)

$(OUTDIR)\$(name).dll: $*.obj $(name).def Makefile $(name).lbc
#	@link16 /NOLOGO/MAP:FULL/A:16/FAR/NON/NOD/ONE:N/NOE $*.obj, $*.dll, $*.map, ..\LibOMF\libw.lib, $(name).def 
	@jwlink format win dll f $*.obj n $*.dll lib ..\LibOMF\libw.lib op q,m=$*.map,protmode,align=16,version=3.10 @$(name).lbc

$(OUTDIR)\$(name).obj: $(name).asm Makefile
	@jwasm.exe $(AOPT) $(name).asm



