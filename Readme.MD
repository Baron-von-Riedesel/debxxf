
The debxxf debuggers are quite ancient, actually the first versions were from 1992.

There exists a copy of my old site: https://www.japheth.de,
where one can download almost 15 year old versions.

Since those old versions had quite a few bugs and were without source, I decided to publish the source now here. One or two parts are supplied in object format only, the source is  missing because it's in a bad shape (the dissassembler, for example ). Perhaps I will find the time to change this later ...

As for the DOS debugger versions, it's a good idea to use the DPMI loaders contained in the newest (v3.20) HX runtime. These loaders will detect the presence of a debugger and then set a breakpoint at protected-mode program entry. The debuggers are already adjusted to this behavior and might be unable to stop at program entry if older versions of the loaders are used.
