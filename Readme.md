# ACWJ-RV

This version of the compiler is based on the 62nd version of [acwj](https://github.com/DoctorWkt/acwj). RISC-V 64 bit support has been added.

## Build & Run

`make cwjrv` will launch the build if you have a C compiler installed.

The call to risc-v cross toolchain utilies are hardcoded in defs.h. You may want to change them to reflect your toolchain PREFIX.

`make triple` will launch triple test. If your system has qemu-user registered as binfmt interpreter or you are testing on a RISC-V 64 system, it should pass without any issue.

## Some Adjustments for this version

To enabe building on macOS, the call to C preprocessor was adjusted such that there is no space between the header include path and `-isystem`. Though it is finally solved by using preprocessor provided by cross toolchain, the change should not affect overall behaviour on GNU toolchains. Also note that the stock C preprocessor on macOS (llvm version) will not strip comments. You may use a GNU version or strip comments yourself.
