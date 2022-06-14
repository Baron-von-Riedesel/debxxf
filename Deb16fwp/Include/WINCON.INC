
CTRL_C_EVENT        equ 0
CTRL_BREAK_EVENT    equ 1
CTRL_CLOSE_EVENT    equ 2
CTRL_LOGOFF_EVENT   equ 5
CTRL_SHUTDOWN_EVENT equ 6

ENABLE_PROCESSED_INPUT equ 00001h
ENABLE_LINE_INPUT      equ 00002h
ENABLE_ECHO_INPUT      equ 00004h
ENABLE_WINDOW_INPUT    equ 00008h
ENABLE_MOUSE_INPUT     equ 00010h
ENABLE_PROCESSED_OUTPUT equ 00001h

ATTACH_PARENT_CONSOLE	equ -1


CHARTYPE UNION
 UnicodeChar    WORD ?
 AsciiChar      db ?
CHARTYPE ENDS

CHAR_INFO STRUCT
Char          CHARTYPE <>
Attributes    WORD      ?
CHAR_INFO ENDS

CONSOLE_SCREEN_BUFFER_INFO struct
dwSize           COORD <>
dwCursorPosition COORD <>
wAttributes      dw ?
srWindow         SMALL_RECT <?,?,?,?>
dwMaximumWindowSize dd ?
CONSOLE_SCREEN_BUFFER_INFO ends

KEY_EVENT_RECORD struct
bKeyDown          dd ?
wRepeatCount      dw ?
wVirtualKeyCode   dw ?
wVirtualScanCode  dw ?
AsciiChar         dw ?
dwControlKeyState dd ?
KEY_EVENT_RECORD ends

MOUSE_EVENT_RECORD struct
dwMousePosition   dd ?           ;word x,word y
dwButtonState     dd ?
dwControlKeyState dd ?
dwEventFlags      dd ?
MOUSE_EVENT_RECORD ends

INPUT_RECORD struct
EventType	dw ?
res			dw ?
union Event
KeyEvent	KEY_EVENT_RECORD <>
MouseEvent	MOUSE_EVENT_RECORD <>
ends                
INPUT_RECORD ends

; ControlKeyState flags

RIGHT_ALT_PRESSED  equ   0001h ;/ the right alt key is pressed.
LEFT_ALT_PRESSED   equ   0002h ;/ the left alt key is pressed.
RIGHT_CTRL_PRESSED equ   0004h ;/ the right ctrl key is pressed.
LEFT_CTRL_PRESSED  equ   0008h ;/ the left ctrl key is pressed.
SHIFT_PRESSED      equ   0010h ;/ the shift key is pressed.
NUMLOCK_ON         equ   0020h ;/ the numlock light is on.
SCROLLLOCK_ON      equ   0040h ;/ the scrolllock light is on.
CAPSLOCK_ON        equ   0080h ;/ the capslock light is on.
ENHANCED_KEY       equ   0100h ;/ the key is enhanced.

ALT_PRESSED        equ LEFT_ALT_PRESSED or RIGHT_ALT_PRESSED
CTRL_PRESSED       equ LEFT_CTRL_PRESSED or RIGHT_CTRL_PRESSED

FROM_LEFT_1ST_BUTTON_PRESSED equ 0001h
RIGHTMOST_BUTTON_PRESSED     equ 0002h
FROM_LEFT_2ND_BUTTON_PRESSED equ 0004h
FROM_LEFT_3RD_BUTTON_PRESSED equ 0008h
FROM_LEFT_4TH_BUTTON_PRESSED equ 0010h

;*** input event types ***

KEY_EVENT       equ 1
MOUSE_EVENT     equ 2

;*** mouse event flags ***

MOUSE_MOVED   equ 0001h
DOUBLE_CLICK  equ 0002h
MOUSE_WHEELED equ 0004h

CONSOLE_CURSOR_INFO   struct
dwSize   dd ?
bVisible dd ?
CONSOLE_CURSOR_INFO   ends

ifdef __JWASM__
    option dllimport:<kernel32.dll>
endif

AllocConsole                 proto near stdcall
FillConsoleOutputAttribute   proto near stdcall :dword, :dword, :dword, :COORD, :ptr dword
FillConsoleOutputCharacterA  proto near stdcall :dword, :dword, :dword, :COORD, :ptr dword
FlushConsoleInputBuffer      proto near stdcall :dword
FreeConsole                  proto near stdcall
GetConsoleCursorInfo         proto near stdcall :dword, :ptr CONSOLE_CURSOR_INFO
GetConsoleMode               proto near stdcall :dword, :ptr dword
GetConsoleScreenBufferInfo   proto near stdcall :dword, :ptr CONSOLE_SCREEN_BUFFER_INFO
GetConsoleTitleA             proto near stdcall :ptr, :dword
GetLargestConsoleWindowSize  proto near stdcall :dword
GetNumberOfConsoleInputEvents proto near stdcall :dword, :dword
GetStdHandle                 proto near stdcall :dword
PeekConsoleInputA            proto near stdcall :dword, :ptr INPUT_RECORD, :dword, :ptr dword
ReadConsoleA                 proto near stdcall :dword, :ptr BYTE, :dword,:ptr dword, :dword
ReadConsoleInputA            proto near stdcall :dword, :ptr INPUT_RECORD, :dword, :ptr dword
ReadConsoleOutputA           proto near stdcall :dword, :ptr CHAR_INFO, :COORD, :COORD, :ptr SMALL_RECT
ReadConsoleOutputCharacterA  proto near stdcall :dword, :ptr BYTE, :dword, :COORD, :ptr dword
ScrollConsoleScreenBufferA   proto near stdcall :dword, :ptr SMALL_RECT, :ptr SMALL_RECT, :COORD, :ptr CHAR_INFO
SetConsoleActiveScreenBuffer proto near stdcall :dword
SetConsoleCtrlHandler        proto near stdcall :ptr, :dword
SetConsoleCursorInfo         proto near stdcall :dword, :ptr CONSOLE_CURSOR_INFO
SetConsoleCursorPosition     proto near stdcall :dword, :COORD
SetConsoleMode               proto near stdcall :dword, :dword
SetConsoleScreenBufferSize   proto near stdcall :dword, :COORD
SetConsoleTextAttribute      proto near stdcall :ptr, :DWORD
SetConsoleTitleA             proto near stdcall :ptr
SetConsoleWindowInfo         proto near stdcall :dword, :dword, :dword
WriteConsoleA                proto near stdcall :dword, :ptr BYTE, :dword, :ptr dword, :dword
WriteConsoleInputA           proto near stdcall :dword, :ptr INPUT_RECORD, :dword, :ptr dword
WriteConsoleOutputA          proto near stdcall :dword, :ptr CHAR_INFO, :COORD, :COORD, :ptr SMALL_RECT
WriteConsoleOutputCharacterA proto near stdcall :dword, :ptr BYTE, :dword, :COORD, :ptr dword

ifdef __JWASM__
    option dllimport:none
endif

