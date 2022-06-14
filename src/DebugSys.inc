

; D386_Load_Segment type equates:

ST_code_sel     equ 0       ; code selector
ST_data_sel     equ 1       ; data selector
ST_code_seg     equ 10h     ; code segment
ST_data_seg     equ 11h     ; data segment
ST_dual_code    equ 40h     ; code segment and selector
ST_dual_data    equ 41h     ; data segment and selector
ST_device_code  equ 80h     ; device driver code segment
ST_device_data  equ 81h     ; device driver data segment

; D386_Load_Segment device load parameters structure

D386_Device_Params STRUC
DD_logical_seg  dw  ?   ; logical segment # from map
DD_actual_sel   dw  ?   ; actual selector value
DD_base         dd  ?   ; linear address offset for start of segment
DD_length       dd  ?   ; actual length of segment
DD_name         df  ?   ; 16:32 ptr to null terminated device name
DD_sym_name     df  ?   ; 16:32 ptr to null terminated symbolic
                        ; module name  (i.e. Win386)
DD_alias_sel    dw  ?   ; alias selector value (0 = none)
D386_Device_Params ENDS


DEBUG_FAULT_TYPE_FIRST equ 1000b
;
;  Protected mode Debugger services:
;


Debug_Serv_Int	     equ 41h	; Interrupt that calls Deb386 to perform

DS_Out_Char		equ     0	; function to display the char in DL
DS_In_Char		equ     1	; function to read a char into AL
DS_Out_Str		equ     2	; function to display a NUL terminated string
DS_Is_Char		equ     3	; Non blocking In_Chr
DS_Out_Str16	equ    12h	; function to display a NUL terminated string
DS_ForcedGO16	equ    40h	; enter the debugger and perform the equivalent
DS_LinkMap		equ    45h	; DX:(E)DI = ptr to paragraph in front of map
DS_UnlinkMap	equ    46h	; DX:(E)DI = ptr to paragraph in front of map
DS_CheckMap		equ    47h	; DX:(E)DI = pointer to module name
DS_IsAutoLoadSym equ   48h	; returns AX != 0, auto load symbols
DS_DebLoaded	equ    4Fh	; check to see if the debugger is installed and
DS_DebPresent	equ 0F386h
DS_LoadSeg		equ    50h	; define a segment value for the
DS_LoadSeg_32	equ  0150h	; Define a 32-bit segment for Windows 32
DS_MoveSeg		equ    51h	; notify the debugger that a segment has moved
DS_FreeSeg		equ    52h	; notify the debugger that a segment has been
DS_FreeSeg_32	equ  0152h	; notify the debugger that a segment has been
DS_DGH			equ    56h	; register "dump global heap" handler
DS_DFL			equ    57h	; register "dump free list" handler
DS_DLL			equ    58h	; register "dump LRU list" handler
DS_StartTask	equ    59h	; notify debugger that a new task is starting
DS_Kernel_Vars	equ    5ah	; Used by the Windows kernel to tell the
DS_VCPI_Notify	equ    5bh	; notify debugger that DOS extender is
DS_ReleaseSeg	equ    5ch	; This does the same as a DS_FreeSeg, but
DS_User_Vars    equ    5dh	; DS:SI = pointer to an array of offsets:
DS_POSTLOAD		equ    60h	; Used by the RegisterPTrace interface
DS_EXITCALL		equ    62h	; Somebody will fill these in if we ever
DS_INT2			equ    63h	; figure out what they are supposed to do.
DS_LOADDLL		equ    64h
DS_DELMODULE	equ    65h
DS_CondBP		equ 0F001h	; conditional break pt, if the command line
DS_ForcedBP		equ 0F002h	; break pt, which accomplishes the same thing
DS_ForcedGO		equ 0F003h	; enter the debugger and perform the equivalent
DS_HardINT1		equ 0F004h	; check to see if INT 1 hooked for all rings
DS_Out_Symbol	equ    0Fh	; find the symbol nearest to the address in
DS_Disasm_Ins	equ    10h	; function to disassemble the instruction
DS_JumpTableStart      equ 70h
DS_RegisterDotCommand  equ 70h
DS_RegisterDotCommand16 equ 71h
DS_DeRegisterDotCommand equ 72h
DS_Printf			equ 73h
DS_Printf16 		equ 74h
DS_GetRegisterSet	equ 75h
DS_SetAlternateRegisterSet equ 76h
DS_GetCommandLineChar equ 77h
DS_EvaluateExpression equ 78h
DS_VerifyMemory		equ 79h
DS_PrintRegisters	equ 7ah
DS_PrintStackDump	equ 7bh
DS_SetThreadID		equ 7ch
DS_ExecDebugCommand	equ 7dh
DS_GetDebuggerInf	equ 7eh
DS_CheckFault		equ 7fh
DS_SetBreak			equ 80h
DS_RedirectExec		equ 81h
DS_PassOnDebugCommand equ 82h
DS_TrapFault		equ 83h
DS_SetStackTraceCallBack equ 84h
DS_RemoveSegs		equ 85h
DS_DefineDebugSegs	equ 86h
DS_SetBaudRate		equ 87h
DS_SetComPort		equ 88h
DS_ChangeTaskNum	equ 89h
DS_ExitCleanup		equ 8ah
DS_InstallVGAHandler equ 8bh
DS_GetComBase		equ 8ch
DS_GetSymbol		equ 8dh
DS_CopyMem			equ 8eh
DS_JumpTableEnd 	equ 8eh

