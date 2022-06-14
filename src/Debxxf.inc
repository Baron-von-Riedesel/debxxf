
;*** used by debxxfd, deb16f, deb16fw, deb32f and deb32fo ***

if ?FLAT
PN32 typedef near32 ptr
else
PF32 typedef far32 ptr
 if ?WINDOWS
PF16 typedef far16 ptr
 endif
endif

DEBUGPARM struct
if ?FLAT
Enable		PN32 ?			;debugger enable function
DebugEntry	PN32 ?			;debugger entry function
Disable		PN32 ?			;debugger disable function
pCmdLine	dd ?			;command line for debugger
pHostStack	dd ?			;host stack to use by debugger
else
Enable		PF32 ?
DebugEntry	PF32 ?
Disable		PF32 ?
pCmdLine	df ?
pHostStack	df ?
endif

if ?WINDOWS
pszDebuggerName dd ?		;full path of debugger overlay
CheckUnregister PF32 ?		;test if debugger can be disabled
_LoadLibrary	PF16 ?
_GetProcAddress PF16 ?
_FreeLibrary	PF16 ?
_OutputDebugString PF16 ?
_ToolhelpHook	PF16 ?
_ReadAccProc	PF16 ?
;_WriteAccProc	PF16 ?
_PeekMessageProc PF16 ?
endif
DEBUGPARM ends

