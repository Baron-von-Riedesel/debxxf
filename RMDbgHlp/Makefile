
# small helper code that is loaded into conventional memory
# to support real-mode interrupt interception.
# tools: jwasm & jwlink

name = rmdbghlp
OUTDIR=Release

incdir  = ..\src
debsinc = ..\src\rmdbghlp.inc ..\src\const.inc

ALL: $(OUTDIR) $(OUTDIR)\$(name).bin

$(OUTDIR):
	@mkdir $(OUTDIR)

$(OUTDIR)\$(name).bin: $(OUTDIR)\text16.obj $(OUTDIR)\vcpi.obj Makefile
	@jwlink format dos com f $(OUTDIR)\text16.obj, $(OUTDIR)\vcpi.obj n $*.bin op q,m=$*.map

$(OUTDIR)\text16.obj: text16.asm $(depsinc) Makefile
	@jwasm -nologo -Sg -Fl$* -Fo$* -D?WINDOWS=0 -D?FLAT=0 -I$(incdir) text16.asm

$(OUTDIR)\vcpi.obj: vcpi.asm $(depsinc) Makefile
	@jwasm -c -nologo -Zg -Sg -Fl$* -Fo$* -D?WINDOWS=0 -D?FLAT=0 -I$(incdir) vcpi.asm
