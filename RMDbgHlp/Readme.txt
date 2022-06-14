

History

27.9.2006: catch real-mode int 21h, ax=2501h and ax=2503h

23.7.2005: int 21, ax=4b01 now called with a "call cs:dword ptr", not
           a real int to avoid the previous int 21h hookers to see the
           int.
