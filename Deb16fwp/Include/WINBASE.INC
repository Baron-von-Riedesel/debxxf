
_PAGESIZE equ 4096

LPVOID typedef ptr
HANDLE typedef ptr

SID_IDENTIFIER_AUTHORITY struct
Value	BYTE 6 dup (?)
SID_IDENTIFIER_AUTHORITY ends

SID	struct
Revision			BYTE ?
SubAuthorityCount	BYTE ?
IdentifierAuthority	SID_IDENTIFIER_AUTHORITY <>
SubAuthority		DWORD 1 dup (?)
SID	ends

SMALL_RECT struct
Left	dw ?
Top 	dw ?
Right	dw ?
Bottom	dw ?
SMALL_RECT ends

COORD struct
X		dw ?
Y		dw ?
COORD ends

LMEM_FIXED		equ 00h
LMEM_MOVEABLE	equ 02h
LMEM_ZEROINIT	equ 40h
LMEM_MODIFY		equ 80h

GMEM_FIXED		equ 00h
GMEM_MOVEABLE	equ 02h
GMEM_ZEROINIT	equ 40h
GMEM_MODIFY		equ 80h

MAXPATH    equ 260

INVALID_HANDLE_VALUE equ -1

STD_INPUT_HANDLE  equ -10
STD_OUTPUT_HANDLE equ -11
STD_ERROR_HANDLE  equ -12

;---- winnls.h

CP_ACP                    equ 0          ; // default to ANSI code page
CP_OEMCP                  equ 1          ; // default to OEM  code page
CP_MACCP                  equ 2          ; // default to MAC  code page
CP_THREAD_ACP             equ 3          ; // current thread's ANSI code page
CP_SYMBOL                 equ 42         ; // SYMBOL translations

NORM_IGNORECASE	equ 1  ;// ignore case for CompareString

LCMAP_LOWERCASE		equ 100h
LCMAP_UPPERCASE		equ 200h
LCMAP_SORTKEY		equ 400h
LCMAP_BYTEREV		equ 800h
LCMAP_HIRAGANA			  equ 00100000h
LCMAP_KATAKANA			  equ 00200000h
LCMAP_HALFWIDTH 		  equ 00400000h
LCMAP_FULLWIDTH 		  equ 00800000h
LCMAP_LINGUISTIC_CASING   equ 01000000h
LCMAP_SIMPLIFIED_CHINESE  equ 02000000h
LCMAP_TRADITIONAL_CHINESE equ 04000000h

CT_CTYPE1	equ 1h
CT_CTYPE2	equ 2h
CT_CTYPE3	equ 4h

C1_UPPER	equ 1h
C1_LOWER	equ 2h
C1_DIGIT	equ 4h
C1_SPACE	equ 8h
C1_PUNCT	equ 10h
C1_CNTRL	equ 20h
C1_BLANK	equ 40h
C1_XDIGIT	equ 80h
C1_ALPHA	equ 100h


MB_PRECOMPOSED            equ 00000001h  ;// use precomposed chars
MB_COMPOSITE              equ 00000002h  ;// use composite chars
MB_USEGLYPHCHARS          equ 00000004h  ;// use glyph chars, not ctrl chars
MB_ERR_INVALID_CHARS      equ 00000008h  ;// error for invalid chars

WC_COMPOSITECHECK         equ 00000200h  ;// convert composite to precomposed
WC_DISCARDNS              equ 00000010h  ;// discard non-spacing chars
WC_SEPCHARS               equ 00000020h  ;// generate separate chars
WC_DEFAULTCHAR            equ 00000040h  ;// replace w/ default char


INFINITE  equ 0FFFFFFFFh

EXCEPTION_ACCESS_VIOLATION		   equ STATUS_ACCESS_VIOLATION
EXCEPTION_DATATYPE_MISALIGNMENT    equ STATUS_DATATYPE_MISALIGNMENT
EXCEPTION_BREAKPOINT			   equ STATUS_BREAKPOINT
EXCEPTION_SINGLE_STEP			   equ STATUS_SINGLE_STEP
EXCEPTION_ARRAY_BOUNDS_EXCEEDED    equ STATUS_ARRAY_BOUNDS_EXCEEDED
EXCEPTION_FLT_DENORMAL_OPERAND	   equ STATUS_FLOAT_DENORMAL_OPERAND
EXCEPTION_FLT_DIVIDE_BY_ZERO	   equ STATUS_FLOAT_DIVIDE_BY_ZERO
EXCEPTION_FLT_INEXACT_RESULT	   equ STATUS_FLOAT_INEXACT_RESULT
EXCEPTION_FLT_INVALID_OPERATION    equ STATUS_FLOAT_INVALID_OPERATION
EXCEPTION_FLT_OVERFLOW			   equ STATUS_FLOAT_OVERFLOW
EXCEPTION_FLT_STACK_CHECK		   equ STATUS_FLOAT_STACK_CHECK
EXCEPTION_FLT_UNDERFLOW 		   equ STATUS_FLOAT_UNDERFLOW
EXCEPTION_INT_DIVIDE_BY_ZERO	   equ STATUS_INTEGER_DIVIDE_BY_ZERO
EXCEPTION_INT_OVERFLOW			   equ STATUS_INTEGER_OVERFLOW
EXCEPTION_PRIV_INSTRUCTION		   equ STATUS_PRIVILEGED_INSTRUCTION
EXCEPTION_IN_PAGE_ERROR 		   equ STATUS_IN_PAGE_ERROR
EXCEPTION_ILLEGAL_INSTRUCTION	   equ STATUS_ILLEGAL_INSTRUCTION
EXCEPTION_NONCONTINUABLE_EXCEPTION equ STATUS_NONCONTINUABLE_EXCEPTION
EXCEPTION_STACK_OVERFLOW		   equ STATUS_STACK_OVERFLOW
EXCEPTION_INVALID_DISPOSITION	   equ STATUS_INVALID_DISPOSITION
EXCEPTION_GUARD_PAGE			   equ STATUS_GUARD_PAGE_VIOLATION
EXCEPTION_INVALID_HANDLE		   equ STATUS_INVALID_HANDLE
CONTROL_C_EXIT					   equ STATUS_CONTROL_C_EXIT

FILE_TYPE_UNKNOWN equ  0000h
FILE_TYPE_DISK	  equ  0001h
FILE_TYPE_CHAR	  equ  0002h
FILE_TYPE_PIPE	  equ  0003h
FILE_TYPE_REMOTE  equ  8000h

FILE_BEGIN	  equ	 0
FILE_CURRENT  equ	 1
FILE_END	  equ	 2

;FILE_MAP_READ equ 0004h

;*** file handling ***

;--- dwDesiredAccess flags (2. parameter of CreateFile)

GENERIC_READ	  equ 80000000h
GENERIC_WRITE	  equ 40000000h

;--- dwShareMode (3. parameter of CreateFile)

FILE_SHARE_READ		EQU	00000001h
FILE_SHARE_WRITE	EQU	00000002h
FILE_SHARE_DELETE	EQU	00000004h

;--- dwCreationDisposition flags (5. parameter of CreateFile)

CREATE_NEW		  equ 1  ; fails if file already exists
CREATE_ALWAYS	  equ 2  ; if file exists, it will be overwritten
OPEN_EXISTING	  equ 3  ; fails if file doesn't exist
OPEN_ALWAYS 	  equ 4  ; creates file if it doesn't exist
TRUNCATE_EXISTING equ 5  ; fails if file doesn't exist. will be truncated

;--- dwFlagsAndAttributes flags (6. parameter of CreateFile)

FILE_ATTRIBUTE_READONLY				EQU	00000001h
FILE_ATTRIBUTE_HIDDEN				EQU	00000002h
FILE_ATTRIBUTE_SYSTEM				EQU	00000004h
FILE_ATTRIBUTE_DIRECTORY			EQU	00000010h
FILE_ATTRIBUTE_ARCHIVE				EQU	00000020h
FILE_ATTRIBUTE_ENCRYPTED			EQU	00000040h
FILE_ATTRIBUTE_NORMAL				EQU	00000080h
FILE_ATTRIBUTE_TEMPORARY			EQU	00000100h
FILE_ATTRIBUTE_SPARSE_FILE			EQU	00000200h
FILE_ATTRIBUTE_REPARSE_POINT		EQU	00000400h
FILE_ATTRIBUTE_COMPRESSED			EQU	00000800h
FILE_ATTRIBUTE_OFFLINE				EQU	00001000h
FILE_ATTRIBUTE_NOT_CONTENT_INDEXED	EQU	00002000h

FILE_FLAG_WRITE_THROUGH			EQU 80000000h
FILE_FLAG_OVERLAPPED			EQU 40000000h
FILE_FLAG_NO_BUFFERING			EQU 20000000h
FILE_FLAG_RANDOM_ACCESS			EQU 10000000h
FILE_FLAG_SEQUENTIAL_SCAN		EQU 08000000h
FILE_FLAG_DELETE_ON_CLOSE		EQU 04000000h
FILE_FLAG_BACKUP_SEMANTICS		EQU 02000000h
FILE_FLAG_POSIX_SEMANTICS		EQU 01000000h
FILE_FLAG_OPEN_REPARSE_POINT	EQU	00200000h
FILE_FLAG_OPEN_NO_RECALL		EQU	00100000h

MOVEFILE_REPLACE_EXISTING		EQU	00000001h
MOVEFILE_COPY_ALLOWED			EQU	00000002h
MOVEFILE_DELAY_UNTIL_REBOOT		EQU	00000004h
MOVEFILE_WRITE_THROUGH			EQU	00000008h
MOVEFILE_CREATE_HARDLINK		EQU	00000010h
MOVEFILE_FAIL_IF_NOT_TRACKABLE	EQU	00000020h

;--- process

NORMAL_PRIORITY_CLASS	equ    00000020h
IDLE_PRIORITY_CLASS 	equ    00000040h
HIGH_PRIORITY_CLASS 	equ    00000080h
REALTIME_PRIORITY_CLASS equ    00000100h

STARTUPINFOA struct
cb				 DWORD	?	;+00
lpReserved		 DWORD	?	;+04
lpDesktop		 DWORD	?	;+08
lpTitle 		 DWORD	?	;+0C
dwX 			 DWORD	?	;+10
dwY 			 DWORD	?	;+14
dwXSize 		 DWORD	?	;+18
dwYSize 		 DWORD	?	;+1C
dwXCountChars	 DWORD	?	;+20
dwYCountChars	 DWORD	?	;+24
dwFillAttribute  DWORD	?	;+28
dwFlags 		 DWORD	?	;+2C
wShowWindow 	 WORD	?	;+30
cbReserved2 	 WORD	?	;+32
lpReserved2 	 DWORD	?	;+34
hStdInput		 DWORD	?	;+38
hStdOutput		 DWORD	?	;+3C
hStdError		 DWORD	?	;+40
STARTUPINFOA ends

STARTUPINFOW struct
	STARTUPINFOA <>	;no "real" difference to the A version
STARTUPINFOW ends

STARTF_RUNFULLSCREEN	equ 020h
STARTF_USESTDHANDLES	equ 100h

PROCESS_INFORMATION struct
hProcess		DWORD	?
hThread			DWORD	?
dwProcessId		DWORD	?
dwThreadId		DWORD	?
PROCESS_INFORMATION ends

OSVERSIONINFO struct
dwOSVersionInfoSize    Dd ?
dwMajorVersion		   Dd ?
dwMinorVersion		   Dd ?
dwBuildNumber		   Dd ?
dwPlatformId		   Dd ?
szCSDVersion		   db 128 dup (?)
OSVERSIONINFO ends

OSVERSIONINFOEX struct
	OSVERSIONINFO <>
wServicePackMajor dw ?
wServicePackMinor dw ?
wSuiteMask dw ?
wProductType db ?
wReserved db ?
OSVERSIONINFOEX ends

OFS_MAXPATHNAME equ 128

OFSTRUCT STRUCT
  cBytes        BYTE      ?
  fFixedDisk    BYTE      ?
  nErrCode      WORD      ?
  Reserved1     WORD      ?
  Reserved2     WORD      ?
  szPathName    BYTE  OFS_MAXPATHNAME dup(?)
OFSTRUCT ENDS

DRIVE_UNKNOWN	  equ 0
DRIVE_NO_ROOT_DIR equ 1
DRIVE_REMOVABLE   equ 2
DRIVE_FIXED 	  equ 3
DRIVE_REMOTE	  equ 4
DRIVE_CDROM 	  equ 5
DRIVE_RAMDISK	  equ 6

MEMORYSTATUS struct
dwLength		 DWORD ?
dwMemoryLoad	 DWORD ?
dwTotalPhys 	 DWORD ?
dwAvailPhys 	 DWORD ?
dwTotalPageFile  DWORD ?
dwAvailPageFile  DWORD ?
dwTotalVirtual	 DWORD ?
dwAvailVirtual	 DWORD ?
MEMORYSTATUS ends


LMEM_FIXED			equ 0000h
LMEM_MOVEABLE		equ 0002h
LMEM_NOCOMPACT		equ 0010h
LMEM_NODISCARD		equ 0020h
LMEM_ZEROINIT		equ 0040h
LMEM_MODIFY 		equ 0080h
LMEM_DISCARDABLE	equ 0F00h
LMEM_VALID_FLAGS	equ 0F72h
LMEM_INVALID_HANDLE equ 8000h

DEBUG_PROCESS			   equ 00000001h
DEBUG_ONLY_THIS_PROCESS    equ 00000002h

CREATE_SUSPENDED		   equ 00000004h

DETACHED_PROCESS		   equ 00000008h

CREATE_NEW_CONSOLE		   equ 00000010h

NORMAL_PRIORITY_CLASS	   equ 00000020h
IDLE_PRIORITY_CLASS 	   equ 00000040h
HIGH_PRIORITY_CLASS 	   equ 00000080h
REALTIME_PRIORITY_CLASS    equ 00000100h

CREATE_NEW_PROCESS_GROUP   equ 00000200h
CREATE_UNICODE_ENVIRONMENT equ 00000400h

CREATE_SEPARATE_WOW_VDM    equ 00000800h
CREATE_SHARED_WOW_VDM	   equ 00001000h

CREATE_DEFAULT_ERROR_MODE  equ 04000000h
CREATE_NO_WINDOW		   equ 08000000h

PROFILE_USER			   equ 10000000h
PROFILE_KERNEL			   equ 20000000h
PROFILE_SERVER			   equ 40000000h

;CRITICAL_SECTION typedef RTL_CRITICAL_SECTION

THREAD_PRIORITY_LOWEST			equ THREAD_BASE_PRIORITY_MIN
THREAD_PRIORITY_BELOW_NORMAL	equ THREAD_PRIORITY_LOWEST + 1
THREAD_PRIORITY_NORMAL			equ 0
THREAD_PRIORITY_HIGHEST 		equ THREAD_BASE_PRIORITY_MAX
THREAD_PRIORITY_ABOVE_NORMAL	equ THREAD_PRIORITY_HIGHEST - 1
THREAD_PRIORITY_ERROR_RETURN	EQU	( MAXLONG )
THREAD_PRIORITY_TIME_CRITICAL	EQU	<THREAD_BASE_PRIORITY_LOWRT>
THREAD_PRIORITY_IDLE			EQU	<THREAD_BASE_PRIORITY_IDLE>

FORMAT_MESSAGE_ALLOCATE_BUFFER	EQU	00000100h
FORMAT_MESSAGE_IGNORE_INSERTS	EQU	00000200h
FORMAT_MESSAGE_FROM_STRING		EQU	00000400h
FORMAT_MESSAGE_FROM_HMODULE		EQU	00000800h
FORMAT_MESSAGE_FROM_SYSTEM		EQU	00001000h
FORMAT_MESSAGE_ARGUMENT_ARRAY	EQU	00002000h
FORMAT_MESSAGE_MAX_WIDTH_MASK	EQU	000000FFh

STILL_ACTIVE	equ STATUS_PENDING

SYSTEM_INFO STRUCT
  wProcessorArchitecture        WORD       ?
  wReserved                     WORD       ?
  dwPageSize                    DWORD      ?
  lpMinimumApplicationAddress   DWORD      ?
  lpMaximumApplicationAddress   DWORD      ?
  dwActiveProcessorMask         DWORD      ?
  dwNumberOfProcessors          DWORD      ?
  dwProcessorType               DWORD      ?
  dwAllocationGranularity       DWORD      ?
  wProcessorLevel               WORD       ?
  wProcessorRevision            WORD       ?
SYSTEM_INFO ENDS

FILETIME struct
dwLowDateTime  dd ?
dwHighDateTime dd ?
FILETIME ends

WIN32_FIND_DATAA struct 4
dwFileAttributes dd ?
ftCreationTime	 FILETIME <?,?>
ftLastAccessTime FILETIME <?,?>
ftLastWriteTime  FILETIME <?,?>
nFileSizeHigh	 dd ?
nFileSizeLow	 dd ?
dwReserved0 	 dd ?
dwReserved1 	 dd ?
cFileName		 db MAXPATH dup (?)
cAlternateFileName db 14 dup (?)
WIN32_FIND_DATAA ends

WIN32_FIND_DATAW struct
dwFileAttributes dd ?
ftCreationTime	 FILETIME <?,?>
ftLastAccessTime FILETIME <?,?>
ftLastWriteTime  FILETIME <?,?>
nFileSizeHigh	 dd ?
nFileSizeLow	 dd ?
dwReserved0 	 dd ?
dwReserved1 	 dd ?
cFileName		 dw MAXPATH dup (?)
cAlternateFileName dw 14 dup (?)
WIN32_FIND_DATAW ends

WIN32_FILE_ATTRIBUTE_DATA STRUCT
  dwFileAttributes DWORD ?
  ftCreationTime  FILETIME <>
  ftLastAccessTime FILETIME <>
  ftLastWriteTime FILETIME <>
  nFileSizeHigh   DWORD ?
  nFileSizeLow    DWORD ?
WIN32_FILE_ATTRIBUTE_DATA ENDS

BY_HANDLE_FILE_INFORMATION struct
dwAttributes     dd ?
ftCreationTime   FILETIME <?,?>
ftLastAccessTime FILETIME <?,?>
ftLastWriteTime  FILETIME <?,?>
dwVolume         dd ?
dwFileSizeHigh   dd ?
dwFileSizeLow    dd ?
nLinks           dd ?
dwFileIndexHigh  dd ?
dwFileIndexLow   dd ?
BY_HANDLE_FILE_INFORMATION ends

LOAD_LIBRARY_AS_DATAFILE	equ 2

PROCESSOR_ARCHITECTURE_INTEL equ 0
PROCESSOR_INTEL_PENTIUM     equ 586

VER_PLATFORM_WIN32s             equ 0
VER_PLATFORM_WIN32_WINDOWS      equ 1
VER_PLATFORM_WIN32_NT           equ 2

WAIT_OBJECT_0       equ ((STATUS_WAIT_0 ) + 0 )
WAIT_ABANDONED      equ ((STATUS_ABANDONED_WAIT_0 ) + 0 )
WAIT_ABANDONED_0    equ ((STATUS_ABANDONED_WAIT_0 ) + 0 )
WAIT_IO_COMPLETION	equ	STATUS_USER_APC
WAIT_TIMEOUT		equ STATUS_TIMEOUT
WAIT_FAILED			equ -1

OF_READ 				equ 0h
OF_WRITE				equ 1h
OF_READWRITE			equ 2h
OF_SHARE_COMPAT 		equ 0h
OF_SHARE_EXCLUSIVE		equ 10h
OF_SHARE_DENY_WRITE 	equ 20h
OF_SHARE_DENY_READ		equ 30h
OF_SHARE_DENY_NONE		equ 40h
OF_PARSE				equ 100h
OF_DELETE				equ 200h
OF_VERIFY				equ 400h
OF_CANCEL				equ 800h
OF_CREATE				equ 1000h
OF_PROMPT				equ 2000h
OF_EXIST				equ 4000h
OF_REOPEN				equ 8000h

FILE_MAP_COPY			equ SECTION_QUERY
FILE_MAP_WRITE			equ	SECTION_MAP_WRITE
FILE_MAP_READ			equ SECTION_MAP_READ
FILE_MAP_ALL_ACCESS		equ SECTION_ALL_ACCESS

SEM_FAILCRITICALERRORS	   equ 0001h
SEM_NOGPFAULTERRORBOX	   equ 0002h
SEM_NOALIGNMENTFAULTEXCEPT equ 0004h
SEM_NOOPENFILEERRORBOX	   equ 8000h

SYSTEMTIME struct
wYear		dw ?
wMonth		dw ?
wDayOfWeek	dw ?
wDay		dw ?
wHour		dw ?
wMinute 	dw ?
wSecond 	dw ?
wMilliseconds	dw ?
SYSTEMTIME ends

TIME_ZONE_ID_UNKNOWN	equ 0
TIME_ZONE_ID_STANDARD	equ 1

TIME_ZONE_INFORMATION STRUCT
  Bias          DWORD       ?
  StandardName  WORD        32 dup(?)
  StandardDate  SYSTEMTIME  <>
  StandardBias  DWORD       ?
  DaylightName  WORD        32 dup(?)
  DaylightDate  SYSTEMTIME  <>
  DaylightBias  DWORD       ?
TIME_ZONE_INFORMATION ENDS

PROCESS_HEAP_ENTRY struct
lpData			DWORD ?
cbData			DWORD ?
cbOverhead		BYTE ?
iRegionIndex    BYTE ?
wFlags			WORD ?
UNION 
STRUCT Block
hMem			HANDLE ?
dwReserved		DWORD 3 dup (?)
ENDS
STRUCT Region
dwCommittedSize		DWORD ?
dwUnCommittedSize	DWORD ?
lpFirstBlock		DWORD ?
lpLastBlock			DWORD ?
ENDS
ENDS
PROCESS_HEAP_ENTRY ends

PROCESS_HEAP_REGION				equ 1
PROCESS_HEAP_UNCOMMITTED_RANGE	equ 2
PROCESS_HEAP_ENTRY_BUSY			equ 4

OVERLAPPED	struct
Internal		DWORD	?
InternalHigh	DWORD	?
Offset_			DWORD	?
OffsetHigh		DWORD	?
hEvent			HANDLE	?
OVERLAPPED	ends

PF_FLOATING_POINT_PRECISION_ERRATA  equ 0
PF_FLOATING_POINT_EMULATED          equ 1
PF_COMPARE_EXCHANGE_DOUBLE          equ 2
PF_MMX_INSTRUCTIONS_AVAILABLE       equ 3
PF_PPC_MOVEMEM_64BIT_OK             equ 4
PF_ALPHA_BYTE_INSTRUCTIONS          equ 5
PF_XMMI_INSTRUCTIONS_AVAILABLE      equ 6
PF_3DNOW_INSTRUCTIONS_AVAILABLE     equ 7
PF_RDTSC_INSTRUCTION_AVAILABLE      equ 8
PF_PAE_ENABLED                      equ 9
PF_XMMI64_INSTRUCTIONS_AVAILABLE    equ 10

;--- COMM API

PST_UNSPECIFIED		EQU	00000000h
PST_RS232			EQU	00000001h
PST_PARALLELPORT	EQU	00000002h
PST_RS422			EQU	00000003h
PST_RS423			EQU	00000004h
PST_RS449			EQU	00000005h
PST_MODEM			EQU	00000006h
PST_FAX				EQU	00000021h
PST_SCANNER			EQU	00000022h
PST_NETWORK_BRIDGE	EQU	00000100h
PST_LAT				EQU	00000101h
PST_TCPIP_TELNET	EQU	00000102h
PST_X25				EQU	00000103h

;--- COMSTAT is used by ClearCommError())

;--- MASM record bits are from high to low!

COMSTAT_R0	RECORD	fReserved:25,fTxim:1,fEof:1,fXoffSent:1,fXoffHold:1,fRlsdHold:1,fDsrHold:1,fCtsHold:1

COMSTAT	struct
r0  		COMSTAT_R0 <>
cbInQue		DWORD	?
cbOutQue	DWORD	?
COMSTAT	ends

;--- communication errors (returned by ClearCommError())

CE_RXOVER	EQU	0001h
CE_OVERRUN	EQU	0002h
CE_RXPARITY	EQU	0004h
CE_FRAME	EQU	0008h
CE_BREAK	EQU	0010h
CE_TXFULL	EQU	0100h
CE_PTO		EQU	0200h
CE_IOE		EQU	0400h
CE_DNS		EQU	0800h
CE_OOP		EQU	1000h
CE_MODE		EQU	8000h

DCB_R0	RECORD fDummy2:17,fAbortOnError:1,fRtsControl:2,fNull:1,fErrorChar:1,fInX:1,fOutX:1,fTXContinueOnXoff:1,fDsrSensitivity:1,fDtrControl:2,fOutxDsrFlow:1,fOutxCtsFlow:1,fParity:1,fBinary:1

DCB struct 
DCBlength	DWORD	?	;+0
BaudRate	DWORD	?	;+4
r0			DCB_R0 <>	;+8
wReserved	WORD	?	;+12
XonLim		WORD	?	;+14
XoffLim		WORD	?	;+16
ByteSize	BYTE	?	;+18
Parity		BYTE	?	;+19
StopBits	BYTE	?	;+20
XonChar		SBYTE	?	;+21
XoffChar	SBYTE	?	;+22
ErrorChar	SBYTE	?	;+23
EofChar		SBYTE	?	;+24
EvtChar		SBYTE	?	;+25
wReserved1	WORD	?	;+26
DCB ends

;--- BaudRate values

CBR_110		EQU	110
CBR_300		EQU	300
CBR_600		EQU	600
CBR_1200	EQU	1200
CBR_2400	EQU	2400
CBR_4800	EQU	4800
CBR_9600	EQU	9600
CBR_14400	EQU	14400
CBR_19200	EQU	19200
CBR_38400	EQU	38400
CBR_56000	EQU	56000
CBR_57600	EQU	57600
CBR_115200	EQU	115200
CBR_128000	EQU	128000
CBR_256000	EQU	256000

;--- values for fRtsControl

RTS_CONTROL_DISABLE		EQU	00h
RTS_CONTROL_ENABLE		EQU	01h
RTS_CONTROL_HANDSHAKE	EQU	02h
RTS_CONTROL_TOGGLE		EQU	03h

;--- values for fDtrControl

DTR_CONTROL_DISABLE		EQU	00h
DTR_CONTROL_ENABLE		EQU	01h
DTR_CONTROL_HANDSHAKE	EQU	02h

;--- values for Parity

NOPARITY	EQU	0
ODDPARITY	EQU	1
EVENPARITY	EQU	2
MARKPARITY	EQU	3
SPACEPARITY	EQU	4

;--- values for StopBits

ONESTOPBIT		EQU	0
ONE5STOPBITS	EQU	1
TWOSTOPBITS		EQU	2

COMMTIMEOUTS	struct 
ReadIntervalTimeout			DWORD	?
ReadTotalTimeoutMultiplier	DWORD	?
ReadTotalTimeoutConstant	DWORD	?
WriteTotalTimeoutMultiplier	DWORD	?
WriteTotalTimeoutConstant	DWORD	?
COMMTIMEOUTS	ends

COMMCONFIG	struct 4
dwSize				DWORD	?
wVersion			WORD	?
wReserved			WORD	?
dcb					DCB	<>
dwProviderSubType	DWORD	?
dwProviderOffset	DWORD	?
dwProviderSize		DWORD	?
wcProviderData		WORD 1 dup (?)
COMMCONFIG	ends

COMMPROP	struct 4
wPacketLength		WORD	?
wPacketVersion		WORD	?
dwServiceMask		DWORD	?
dwReserved1			DWORD	?
dwMaxTxQueue		DWORD	?
dwMaxRxQueue		DWORD	?
dwMaxBaud			DWORD	?
dwProvSubType		DWORD	?
dwProvCapabilities	DWORD	?
dwSettableParams	DWORD	?
dwSettableBaud		DWORD	?
wSettableData		WORD	?
wSettableStopParity	WORD	?
dwCurrentTxQueue	DWORD	?
dwCurrentRxQueue	DWORD	?
dwProvSpec1			DWORD	?
dwProvSpec2			DWORD	?
wcProvChar			WORD 1 dup (?)
COMMPROP	ends

;--- dwServiceMask values

SP_SERIALCOMM	EQU	00000001h

;--- dwMaxBaud values

BAUD_115200	EQU	00020000h

;--- dwProvCapabilities values

PCF_DTRDSR			EQU	0001h
PCF_RTSCTS			EQU	0002h
PCF_RLSD			EQU	0004h
PCF_PARITY_CHECK	EQU	0008h
PCF_XONXOFF			EQU	0010h
PCF_SETXCHAR		EQU	0020h
PCF_TOTALTIMEOUTS	EQU	0040h
PCF_INTTIMEOUTS		EQU	0080h
PCF_SPECIALCHARS	EQU	0100h
PCF_16BITMODE		EQU	0200h

;--- dwSettableParams values

SP_PARITY		EQU	0001h
SP_BAUD			EQU	0002h
SP_DATABITS		EQU	0004h
SP_STOPBITS		EQU	0008h
SP_HANDSHAKING	EQU	0010h
SP_PARITY_CHECK	EQU	0020h
SP_RLSD			EQU	0040h

;--- wSettableStopParity values

STOPBITS_10	EQU	0001h
STOPBITS_15	EQU	0002h
STOPBITS_20	EQU	0004h

PARITY_NONE		EQU	0100h
PARITY_ODD		EQU	0200h
PARITY_EVEN		EQU	0400h
PARITY_MARK		EQU	0800h
PARITY_SPACE	EQU	1000h

;--- values for EscapeCommFunction

SETXOFF		EQU	1
SETXON		EQU	2
SETRTS		EQU	3
CLRRTS		EQU	4
SETDTR		EQU	5
CLRDTR		EQU	6
RESETDEV	EQU	7
SETBREAK	EQU	8
CLRBREAK	EQU	9

;--- values for WaitCommEvent, GetCommMask, SetCommMask

EV_RXCHAR	EQU	0001h
EV_RXFLAG	EQU	0002h
EV_TXEMPTY	EQU	0004h
EV_CTS		EQU	0008h
EV_DSR		EQU	0010h
EV_RLSD		EQU	0020h
EV_BREAK	EQU	0040h
EV_ERR		EQU	0080h
EV_RING		EQU	0100h
EV_PERR		EQU	0200h
EV_RX80FULL	EQU	0400h
EV_EVENT1	EQU	0800h
EV_EVENT2	EQU	1000h

;--- values for PurgeComm flags parameter

PURGE_TXABORT	EQU	0001h
PURGE_RXABORT	EQU	0002h
PURGE_TXCLEAR	EQU	0004h
PURGE_RXCLEAR	EQU	0008h

FINDEX_INFO_LEVELS typedef DWORD
FindExInfoStandard equ 0
FindExInfoBasic    equ 1
FindExInfoMaxInfoLevel equ 2

FINDEX_SEARCH_OPS typedef DWORD
FindExSearchNameMatch          equ 0
FindExSearchLimitToDirectories equ 1
FindExSearchLimitToDevices     equ 2
FindExSearchMaxSearchOp        equ 3

;--- FindFirstFileEx dwAdditionalFlags
FIND_FIRST_EX_CASE_SENSITIVE       equ 1
FIND_FIRST_EX_LARGE_FETCH          equ 2
FIND_FIRST_EX_ON_DISK_ENTRIES_ONLY equ 4

ifdef __JWASM__
    option dllimport:<kernel32.dll>
endif

AddAtomA				proto near stdcall :ptr byte
Beep					proto near stdcall x1:dword,x2:dword
CancelWaitableTimer 	proto near stdcall :dword
CloseHandle 			proto near stdcall x1:dword
CompareStringA 			proto near stdcall :DWORD, :DWORD, :ptr BYTE, :DWORD, :ptr BYTE, :DWORD
CompareStringW 			proto near stdcall :DWORD, :DWORD, :ptr WORD, :DWORD, :ptr WORD, :DWORD
CopyFileA 				proto near stdcall :ptr BYTE, :ptr BYTE, :DWORD
CreateDirectoryA		proto near stdcall :ptr BYTE, :dword
CreateDirectoryExA		proto near stdcall :ptr BYTE, :ptr BYTE, :ptr
CreateDirectoryW		proto near stdcall :ptr WORD, :dword
CreateEventA			proto near stdcall :dword, :dword, :dword, :ptr BYTE
CreateFileA 			proto near stdcall x1:dword,x2:dword,x3:dword,x4:dword,x5:dword,x6:dword,x7:dword
CreateFileMappingA		proto near stdcall a1:dword,a2:dword,a3:dword,a4:dword,a5:dword,a6:ptr BYTE
CreateMutexA			proto near stdcall :dword, :dword, :ptr BYTE
CreateMutexW			proto near stdcall :dword, :dword, :ptr WORD
CreateProcessA			proto near stdcall :ptr BYTE, :ptr BYTE, :dword, :dword, :dword, :dword, :ptr , :ptr BYTE, :ptr STARTUPINFOA, :ptr PROCESS_INFORMATION
CreateSemaphoreA		proto near stdcall a1:dword,a2:dword,a3:dword,a4:dword
CreateThread			proto near stdcall x1:dword,x2:dword,x3:dword,x4:dword,x5:dword,x6:dword
CreateWaitableTimerA	proto near stdcall :dword, :dword, :ptr BYTE
DebugBreak				proto near stdcall
DeleteAtom				proto near stdcall :dword
DeleteCriticalSection   proto near stdcall :ptr CRITICAL_SECTION
DeleteFileA 			proto near stdcall x1:dword
DeviceIoControl 		proto near stdcall :dword, :dword, :ptr, :dword, :ptr, :dword, :ptr dword, :dword
DisableThreadLibraryCalls proto near stdcall :dword
DosDateTimeToFileTime	proto near stdcall :DWORD, :DWORD, :ptr FILETIME
DuplicateHandle 		proto near stdcall :dword, :dword, :dword, :dword, :dword, :dword, :dword
EnterCriticalSection    proto near stdcall :ptr CRITICAL_SECTION
EnumResourceLanguagesA 	proto near stdcall :dword, :ptr BYTE, :ptr BYTE, :dword, :dword
EnumResourceNamesA  	proto near stdcall :dword, :ptr BYTE, :dword, :dword
EnumResourceTypesA  	proto near stdcall :dword, :dword, :dword
ExitProcess 			proto near stdcall :dword
ExitThread				proto near stdcall :dword
ExpandEnvironmentStringsA proto near stdcall :ptr BYTE, :ptr BYTE, :dword
FatalAppExitA			proto near stdcall :dword, :ptr BYTE
FileTimeToDosDateTime	proto near stdcall :ptr FILETIME, :ptr WORD, :ptr WORD
FileTimeToSystemTime    proto near stdcall :ptr FILETIME, :ptr SYSTEMTIME
FindAtomA				proto near stdcall :ptr byte
FindClose				proto near stdcall :dword
FindFirstFileA			proto near stdcall :ptr BYTE, :ptr WIN32_FIND_DATAA
FindFirstFileExA		proto near stdcall :ptr BYTE, :FINDEX_INFO_LEVELS, :LPVOID, :FINDEX_SEARCH_OPS, :LPVOID, :DWORD
FindFirstFileExW		proto near stdcall :ptr WORD, :FINDEX_INFO_LEVELS, :LPVOID, :FINDEX_SEARCH_OPS, :LPVOID, :DWORD
FindFirstFileW			proto near stdcall :ptr WORD, :ptr WIN32_FIND_DATAW
FindNextFileA			proto near stdcall :dword, :ptr WIN32_FIND_DATAA
FindNextFileW			proto near stdcall :dword, :ptr WIN32_FIND_DATAW
FindResourceA			proto near stdcall a1:dword,a2:dword,a3:dword
FindResourceExA			proto near stdcall a1:dword,a2:dword,a3:dword,a4:dword
FindResourceW			proto near stdcall :dword, :ptr WORD, :ptr word
FlushFileBuffers		proto near stdcall x1:dword
FlushViewOfFile			proto near stdcall :ptr, :dword
FormatMessageA			proto near stdcall :dword, :ptr, :DWORD, :DWORD, :ptr BYTE, :DWORD, :ptr
FreeEnvironmentStringsA proto near stdcall x1:dword
FreeEnvironmentStringsW proto near stdcall x1:dword
FreeLibrary 			proto near stdcall :dword
GetACP			 		proto near stdcall
GetAtomNameA 			proto near stdcall :dword, :ptr byte, :dword
GetCommandLineA 		proto near stdcall
GetComputerNameA 		proto near stdcall :ptr BYTE, :ptr DWORD
GetCurrentDirectoryA	proto near stdcall a1:dword,a2:ptr byte
GetCurrentProcess		proto near stdcall
GetCurrentProcessId		proto near stdcall
GetCurrentThread		proto near stdcall
GetCurrentThreadId		proto near stdcall
GetDateFormatA			proto near stdcall lcid:DWORD, dwFlags:DWORD, pDate:ptr SYSTEMTIME,lpFormat:ptr BYTE, lpDateStr:ptr BYTE, cchDate:DWORD
GetDiskFreeSpaceA		proto near stdcall :ptr BYTE, :ptr dword, :ptr dword, :ptr dword, :ptr dword
GetDiskFreeSpaceW		proto near stdcall :ptr WORD, :ptr dword, :ptr dword, :ptr dword, :ptr dword
GetDriveTypeA			proto near stdcall a1:ptr BYTE
GetDriveTypeW			proto near stdcall a1:ptr WORD
GetEnvironmentStrings	proto near stdcall
GetEnvironmentStringsA	proto near stdcall
GetEnvironmentStringsW	proto near stdcall
GetEnvironmentVariableA proto near stdcall :ptr BYTE, :ptr BYTE, :dword
GetExitCodeThread		proto near stdcall :dword, :ptr dword
GetFileAttributesA		proto near stdcall :ptr byte
GetFileAttributesExA	proto near stdcall :ptr BYTE, :DWORD, :ptr WIN32_FILE_ATTRIBUTE_DATA
GetFileSize 			proto near stdcall :dword, :ptr dword
GetFileTime 			proto near stdcall :dword, :ptr FILETIME, :ptr FILETIME, :ptr FILETIME
GetFileType 			proto near stdcall x1:dword
GetFullPathNameA		proto near stdcall :ptr BYTE, :DWORD, :ptr BYTE, :ptr ptr BYTE
GetFullPathNameW		proto near stdcall :ptr WORD, :DWORD, :ptr WORD, :ptr ptr WORD
GetLastError			proto near stdcall
GetLocaleInfoA			proto near stdcall :dword, :dword, :ptr BYTE, :dword
GetLocalTime			proto near stdcall x1:ptr SYSTEMTIME
GetLongPathNameA		proto near stdcall :ptr byte, :ptr byte, :DWORD
GetModuleFileNameA		proto near stdcall :dword, :ptr byte, :dword
GetModuleHandleA		proto near stdcall :ptr BYTE
GetNumberOfConsoleMouseButtons proto near stdcall :ptr DWORD
GetOEMCP			 	proto near stdcall
GetPrivateProfileStringA proto near stdcall :ptr BYTE, :ptr BYTE, :ptr BYTE, :ptr BYTE, :DWORD, :ptr BYTE
GetProcAddress			proto near stdcall :DWORD, :DWORD
GetProcessHeap			proto near stdcall
GetShortPathNameA		proto near stdcall :ptr byte, :ptr byte, :DWORD
GetStartupInfoA 		proto near stdcall :ptr STARTUPINFOA
GetStdHandle			proto near stdcall a1:dword
GetSystemDirectoryA 	proto near stdcall :ptr byte, :DWORD
GetSystemTime			proto near stdcall :ptr SYSTEMTIME
GetSystemTimeAsFileTime proto near stdcall a1:ptr FILETIME
GetTempFileNameA		proto near stdcall :ptr BYTE, :ptr BYTE, :DWORD, :ptr BYTE
GetTempPathA			proto near stdcall :DWORD, :ptr BYTE
GetTickCount			proto near stdcall
GetTimeFormatA			proto near stdcall :dword, :dword, :dword, :dword, :dword, :dword
GetUserDefaultLCID      proto near stdcall
GetVersion				proto near stdcall
GetVersionExA			proto near stdcall :ptr OSVERSIONINFO
GlobalAddAtomA			proto near stdcall :ptr BYTE
GlobalAlloc 			proto near stdcall :dword, :dword
GlobalFree	 			proto near stdcall :dword
GlobalGetAtomNameA		proto near stdcall :dword, :ptr BYTE, :dword
GlobalMemoryStatus		proto near stdcall :ptr MEMORYSTATUS
GlobalSize	 			proto near stdcall :dword
HeapAlloc				proto near stdcall :dword, :dword, :dword
HeapCreate				proto near stdcall :dword, :dword, :dword
HeapDestroy 			proto near stdcall :dword
HeapFree				proto near stdcall :dword, :dword, :dword
HeapReAlloc				proto near stdcall :dword, :dword, :dword, :dword
HeapSize				proto near stdcall :dword, :dword, :dword
HeapValidate			proto near stdcall :dword, :dword, :dword
HeapWalk				proto near stdcall :dword, :ptr PROCESS_HEAP_ENTRY
InitializeCriticalSection proto near stdcall :ptr CRITICAL_SECTION
InterlockedDecrement	proto near stdcall :dword
InterlockedExchange		proto near stdcall :ptr DWORD, :DWORD
InterlockedIncrement	proto near stdcall :dword
IsBadCodePtr			proto near stdcall :DWORD
IsBadReadPtr			proto near stdcall :DWORD, :DWORD
IsBadWritePtr			proto near stdcall :DWORD, :DWORD
IsDebuggerPresent		proto near stdcall
IsProcessorFeaturePresent proto near stdcall :dword
LeaveCriticalSection    proto near stdcall :ptr CRITICAL_SECTION
LoadLibraryA			proto near stdcall :ptr BYTE
LoadLibraryExA			proto near stdcall :ptr BYTE, :DWORD, :DWORD
LoadResource			proto near stdcall :dword, :dword
LocalAlloc				proto near stdcall :dword, :dword
LocalFileTimeToFileTime proto near stdcall :ptr FILETIME, :ptr FILETIME
LocalFree				proto near stdcall :dword
MapViewOfFile	 		proto near stdcall :dword, :dword, :dword, :dword, :dword
MapViewOfFileEx	 		proto near stdcall :dword, :dword, :dword, :dword, :dword, :ptr
MoveFileA 				proto near stdcall :ptr BYTE, :ptr BYTE
MoveFileExA 			proto near stdcall :ptr BYTE, :ptr BYTE, :dword
MultiByteToWideChar		proto near stdcall :DWORD, :DWORD, :ptr BYTE, :DWORD, :ptr WORD, :DWORD
OpenEventA				proto near stdcall :dword, :dword, :ptr BYTE
OpenFile		 		proto near stdcall :ptr byte, :ptr OFSTRUCT, :dword
OpenFileMappingA 		proto near stdcall :dword, :dword, :ptr BYTE
OpenMutexA 				proto near stdcall :dword, :dword, :dword
OpenProcess 			proto near stdcall :dword, :dword, :dword
OpenSemaphoreA  		proto near stdcall :dword, :dword, :dword
OutputDebugStringA		proto near stdcall :ptr BYTE
PulseEvent				proto near stdcall :dword
QueryPerformanceCounter	proto near stdcall :ptr
QueryPerformanceFrequency	proto near stdcall :ptr
QueueUserAPC			proto near stdcall :dword, :dword, :dword
RaiseException			proto near stdcall :DWORD, :DWORD, :DWORD, :ptr
ReadFile				proto near stdcall :dword, :dword, :dword, :ptr dword, :ptr OVERLAPPED
ReleaseMutex			proto near stdcall a1:dword
ReleaseSemaphore		proto near stdcall a1:dword,a2:dword,a3:dword
RemoveDirectoryA		proto near stdcall :ptr BYTE
RemoveDirectoryW		proto near stdcall :ptr WORD
ResetEvent  			proto near stdcall :dword
ResumeThread			proto near stdcall x1:dword
RtlFillMemory			proto near stdcall :ptr BYTE, :DWORD, :dword
RtlMoveMemory			proto near stdcall :ptr BYTE, :ptr BYTE, :dword
RtlUnwind				proto near stdcall :ptr EXCEPTION_REGISTRATION, :dword, :ptr EXCEPTION_RECORD, :dword
RtlZeroMemory			proto near stdcall :ptr BYTE, :dword
SearchPathA				proto near stdcall :ptr BYTE, :ptr BYTE, :ptr BYTE, :DWORD, :ptr BYTE, :ptr ptr BYTE
SetCurrentDirectoryA	proto near stdcall :ptr BYTE
SetCurrentDirectoryW	proto near stdcall :ptr WORD
SetEndOfFile  			proto near stdcall :dword
SetEnvironmentVariableA proto near stdcall x1:ptr BYTE,x2:ptr BYTE
SetErrorMode			proto near stdcall x1:dword
SetEvent				proto near stdcall :dword
SetFileAttributesA		proto near stdcall :ptr byte, :dword
SetFilePointer			proto near stdcall :dword, :dword, :dword, :dword
SetFilePointerEx		proto near stdcall :dword, :qword, :ptr QWORD, :dword
SetFileTime 			proto near stdcall :dword, :ptr FILETIME, :ptr FILETIME, :ptr FILETIME
SetHandleCount			proto near stdcall :dword
SetLastError			proto near stdcall :dword
SetPriorityClass		proto near stdcall :dword, :dword
SetStdHandle			proto near stdcall :dword, :dword
SetThreadPriority		proto near stdcall :dword, :dword
SetUnhandledExceptionFilter proto near stdcall :ptr
SetWaitableTimer		proto near stdcall :dword, :ptr QWORD, :dword, :dword, :dword, :dword
SizeofResource			proto near stdcall :dword, :dword
Sleep					proto near stdcall x1:dword
SuspendThread			proto near stdcall x1:dword
SwitchToThread			proto near stdcall
SystemTimeToFileTime	proto near stdcall :ptr SYSTEMTIME, :ptr FILETIME
TerminateProcess		proto near stdcall x1:dword,x2:dword
TerminateThread 		proto near stdcall x1:dword,x2:dword
TlsAlloc				proto near stdcall
TlsFree 				proto near stdcall :dword
TlsGetValue 			proto near stdcall x1:dword
TlsSetValue 			proto near stdcall x1:dword,x2:dword
UnmapViewOfFile			proto near stdcall x1:ptr
VirtualAlloc			proto near stdcall x1:dword,x2:dword,x3:dword,x4:dword
VirtualFree 			proto near stdcall x1:dword,x2:dword,x3:dword
VirtualLock 			proto near stdcall x1:dword,x2:dword
VirtualProtect			proto near stdcall x1:dword,x2:dword,x3:dword,x4:dword
VirtualQuery			proto near stdcall :dword, :ptr MEMORY_BASIC_INFORMATION, :dword
WaitForSingleObject 	proto near stdcall x1:dword,x2:dword
WaitForMultipleObjects 	proto near stdcall :dword, :ptr dword, :dword, :dword
WideCharToMultiByte		proto near stdcall :DWORD, :DWORD, :ptr WORD, :DWORD, :ptr BYTE, :DWORD, :DWORD, :DWORD
WinExec 				proto near stdcall :ptr BYTE, :DWORD
WriteFile				proto near stdcall :dword, :dword, :dword, :ptr dword, :ptr OVERLAPPED
WritePrivateProfileStringA	proto near stdcall lpAppName:ptr BYTE, lpKeyName:ptr BYTE, lpString:ptr BYTE, lpFileName:ptr BYTE
_lcreat 				proto near stdcall :dword, :dword
_llseek 				proto near stdcall :dword, :dword, :dword
_lopen					proto near stdcall :dword, :dword
_lclose 				proto near stdcall :dword
_lread					proto near stdcall :dword, :dword, :dword
_lwrite 				proto near stdcall :dword, :dword, :dword
lstrcat 				proto near stdcall :ptr BYTE, :ptr BYTE
lstrcatA				proto near stdcall :ptr BYTE, :ptr BYTE
lstrcatW				proto near stdcall :ptr WORD, :ptr WORD
lstrcmp 				proto near stdcall :ptr BYTE, :ptr BYTE
lstrcmpA				proto near stdcall :ptr BYTE, :ptr BYTE
lstrcmpi				proto near stdcall :ptr BYTE, :ptr BYTE
lstrcmpiA				proto near stdcall :ptr BYTE, :ptr BYTE
lstrcpy 				proto near stdcall :ptr BYTE, :ptr BYTE
lstrcpyA				proto near stdcall :ptr BYTE, :ptr BYTE
lstrcpyn				proto near stdcall :ptr BYTE, :ptr BYTE, :DWORD
lstrcpynA				proto near stdcall :ptr BYTE, :ptr BYTE, :DWORD
lstrcpynW				proto near stdcall :ptr WORD, :ptr WORD, :DWORD
lstrlen 				proto near stdcall :ptr BYTE
lstrlenA				proto near stdcall :ptr BYTE
lstrlenW				proto near stdcall :ptr WORD

ifdef __JWASM__
    option dllimport:none
endif

