
if ?WINDOWS

;--- kernel procs

externdef _GetCurrentTask      :PF16
externdef _GetCurrentPDB       :PF16
externdef _GetDOSEnvironment   :PF16
externdef _GlobalDosAlloc      :PF16
externdef _GlobalDosFree       :PF16
externdef _NoHookDosCall       :PF16
externdef _WaitEvent           :PF16
externdef _PostEvent           :PF16
externdef _SetErrorMode        :PF16
externdef _ToolhelpHook        :PF16
externdef _LoadLibrary         :PF16
externdef _FreeLibrary			:PF16
externdef _GetProcAddress		:PF16
externdef _OutputDebugString	:PF16
externdef _GetTaskDS			:PF16
externdef _LockCurrentTask		:PF16
externdef _GetPrivateProfileString	:PF16
externdef _WritePrivateProfileString:PF16
externdef _GetModuleHandle		:PF16
externdef _GetExePtr			:PF16

externdef _ReadAccProc			:PF16
externdef _PeekMessageProc		:PF16

;--- user procs

externdef _PostAppMessage		:PF16
;externdef _MessageBox			:PF16

;--- mouse procs

externdef _MouseGetIntVect		:PF16

;--- toolhelp functions

externdef _TaskGetCSIP         :PF16
externdef _GlobalEntryHandle   :PF16
externdef _GlobalFirst         :PF16
externdef _GlobalNext          :PF16
externdef _NotifyRegister      :PF16
externdef _NotifyUnregister    :PF16
externdef _InterruptRegister   :PF16
externdef _InterruptUnregister :PF16
externdef _TaskSwitch          :PF16

;--- deb16fwv procs

externdef _SwitchToDebuggerScreen	:PF16
externdef _SwitchToWindowsScreen	:PF16
externdef _VideoInit				:PF16
externdef _VideoDone				:PF16

endif

externdef _GetSymbolName	:PF16
externdef _GetSymbolAddr	:PF16

