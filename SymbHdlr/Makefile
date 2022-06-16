
# create SymbhdlW.dll and SymbhdlD.dll
# tools: jwasm & jwlink

!ifndef DEBUG
DEBUG=0
!endif

OUTD = Release
name = SymbHdlr
dllW = SymbhdlW
dllD = SymbhdlD
LIBS = ..\LibOMF\libw
mods  = $(OUTD)\GetSym.obj $(OUTD)\GetSymNm.obj $(OUTD)\GetSegm.obj $(OUTD)\GetPrAdr.obj $(OUTD)\NresName.obj $(OUTD)\GetPrcNm.obj
modsW = $(OUTD)\Entries2.obj
modsD = $(OUTD)\GetEntry.obj

!if $(DEBUG)
AOPTD=-D_DEBUG
!else
AOPTD=
!endif

aopt = -c -nologo -Fl$* -Fo$* -Cp -Sg $(AOPTD)

.asm.{$(OUTD)}.obj:
	@jwasm $(aopt) $<

ALL: $(OUTD) $(OUTD)\$(dllW).dll $(OUTD)\$(dllD).dll

$(OUTD):
	@mkdir $(OUTD)

$(OUTD)\$(dllW).dll: $(OUTD)\$(name).obj $(name).lbc $(modsW) $(mods)
	@jwlink format win dll f {$(OUTD)\$(name).obj $(modsW) $(mods)} n $*.dll lib $(LIBS) op q,m=$*,align=16 @$(name).lbc
#	@link16 /A:16 $(OUTD)\$(name).obj $(modsW) $(mods),$*.dll,$*.map,$(LIBS),$(name).def

$(OUTD)\$(dllD).dll: $(OUTD)\$(name).obj $(name).lbc $(modsD) $(mods)
	@jwlink format win dll f {$(OUTD)\$(name).obj $(modsD) $(mods)} n $*.dll lib $(LIBS) op q,m=$*,align=16 @$(name).lbc

