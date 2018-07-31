# Alonzo Compiler Toolchain

## Building and running

Install cabal. Make sure pacakges are up to date.

```bash
$ cabal update
```

Build with cabal new-build

``` sh
# Build the project.
cabal new-build all
```

Run with cabal new-run

```
# Run alci
cabal new-run alci
```

## Installing LLVM On Windows

To build on windows, llvm must be built and installed manually. This process can take a very, very long time.

First, install MSYS2. Then install the following packages.

```
pacman -Syu
pacman -S msys2-devel mingw-w64-x86_64-toolchain gcc binutils bash automake make python2 mingw-w64-x86_64-cmake
```

Next, download the llvm source and unzip it. Next, open the MSSY2 MingGW 64-bit command prompt. Navigate to the source directory. Then build llvm with the following commands.

```
mkdir build && cd build

cmake.exe .. -DCMAKE_INSTALL_PREFIX=/c/llvm -DLLVM_TARGETS_TO_BUILD="AArch64" -DBUILD_SHARED_LIBS=True -G"MSYS Makefiles"

cmake.exe --build . --target install
```

Finally, add `C:\llvm\bin` to the path.