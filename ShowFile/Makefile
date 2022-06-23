
# creates both showfile.dll and s.exe
# HX directory needed for s.exe only (stub dpmist16.bin)
# uses jwasm # jwlink

HXDIR=\hx

name = ShowFile
name1= S
name2= Pager
name3= Edit
name4= hread

ODIR1=Release
ODIR2=Exe

deps = ShowFile.inc Makefile

aopt1 = -nologo -Fl$* -Sg -Fo$* -D?DLL=1
mods1 = $(ODIR1)\$(name1).obj $(ODIR1)\$(name2).obj $(ODIR1)\$(name3).obj $(ODIR1)\$(name4).obj

aopt2  = -nologo -Fl$* -Fo$* -D?DLL=0 -D?USEPREDEFSEGS=1
mods2 = $(ODIR2)\$(name2).obj $(ODIR2)\$(name3).obj $(ODIR2)\$(name4).obj $(ODIR2)\sexe.obj

ALL: $(ODIR1) $(ODIR2) $(ODIR1)\$(name).dll $(ODIR2)\$(name1).exe

$(ODIR1):
	@mkdir $(ODIR1)

$(ODIR2):
	@mkdir $(ODIR2)

$(ODIR1)\$(name).dll: $*.obj $(mods1) $(name).lbc
	@jwlink format win dll f {$* $(mods1)} n $*.dll lib ..\LibOMF\libw op q,m=$*,align=16 @$(name).lbc

$(ODIR1)\$(name1).obj: $(name1).asm $(deps)
	@jwasm $(aopt1) $(name1).asm

$(ODIR1)\$(name2).obj: $(name2).asm $(deps)
	@jwasm $(aopt1) $(name2).asm

$(ODIR1)\$(name3).obj: $(name3).asm $(deps)
	@jwasm $(aopt1) $(name3).asm

$(ODIR1)\$(name4).obj: $(name4).asm $(deps)
	@jwasm $(aopt1) $(name4).asm

$(ODIR1)\$(name).obj: $(name).asm $(deps)
	@jwasm $(aopt1) $(name).asm


$(ODIR2)\$(name1).exe: $*.obj $(mods2) $(name1).lbc
	@jwlink format win dpmi f {$* $(mods2)} n $* lib ..\LibOMF\libw op q,m=$*,align=16,stub=$(HXDIR)\bin\dpmist16.bin @$(name1).lbc

$(ODIR2)\sexe.obj: sexe.asm $(deps)
	@jwasm $(aopt2) sexe.asm

$(ODIR2)\$(name1).obj: $(name1).asm $(deps)
	@jwasm $(aopt2) $(name1).asm

$(ODIR2)\$(name2).obj: $(name2).asm $(deps)
	@jwasm $(aopt2) $(name2).asm

$(ODIR2)\$(name3).obj: $(name3).asm $(deps)
	@jwasm $(aopt2) $(name3).asm

$(ODIR2)\$(name4).obj: $(name4).asm $(deps)
	@jwasm $(aopt2) $(name4).asm

