
!ifndef DEBUG
DEBUG = 0
!endif

ALL: 
	@nmake /nologo /s /f Deb16f.mak  DEBUG=$(DEBUG) 
	@nmake /nologo /s /f Deb16fw.mak DEBUG=$(DEBUG) 
	@nmake /nologo /s /f Deb32f.mak  DEBUG=$(DEBUG) 

clean: 
	@nmake /nologo /s /f Deb16f.mak  clean
	@nmake /nologo /s /f Deb16fw.mak clean
	@nmake /nologo /s /f Deb32f.mak  clean

