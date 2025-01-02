# proto-diff
Identify differences in function declarations across different code versions.

## Dependencies

```
sudo apt install clang llvm-18-dev 
sudo apt install clang libclang-18-dev 
```

## Build

```
mkdir build && cd build
cmake ../proto-diff/ -DCLANG_DIR=/usr/lib/llvm-18/lib/cmake/clang -DLLVM_DIR=/usr/lib/llvm-18/lib/cmake/llvm
make proto-diff
```

## Run

```
$ ./bin/proto-diff 
error: [Failed to parse arguments] proto-diff: Not enough positional command line arguments specified!
Must specify at least 1 positional argument: See: ./bin/proto-diff --help
$ cat file1.c 
int fn() {
  return 0;
}

$ cat file2.c 
int fn(int x) {
  return 0;
}

$ ./bin/proto-diff file1.c file2.c 
[1/2] Processing file test-proto-diff/file1.c.
[2/2] Processing file test-proto-diff/file2.c.
[1/2] Processing file test-proto-diff/file1.c.
[2/2] Processing file test-proto-diff/file2.c.
=== Differences in Function Declarations ===
Function "fn" has changed:
  Return type: int -> int
  Parameters: int,  -> 

```

## Test it on a Linux LKM

```
$ sudo apt install bear
$ cd ${lkm_dir}
# create compile_commands.json
$ bear make
```

Test against newer linux. Lets download the source:

```
$ sudo apt-get install flex
$ sudo apt-get install bison
$ sudo apt-get install libelf-dev
$ wget https://cdn.kernel.org/pub/linux/kernel/v6.x/linux-6.8.tar.xz
$ tar -xJf linux-6.8.tar.xz 
$ cd linux-6.8/
$ make defconfig
$ make modules_prepare
```

Run from LKM dir:
```
$ ../build_proto_diff/bin/proto-diff -p=. -symbols-file=input_symbols.txt -target-linux-source=/path/to/linux-6.8/ /path/to/src/fs.c
```
