# Original Paper

```
The Google SSTable file format is used internally to
store Bigtable data. An SSTable provides a persistent,
ordered immutable map from keys to values, where both
keys and values are arbitrary byte strings. 

Operations are provided to look up the value associated 
with a specified key, and to iterate over all key/value 
pairs in a specified key range.

Internally, each SSTable contains a sequence
of blocks (typically each block is 64KB in size, but this
is configurable). 

A block index (stored at the end of the
SSTable) is used to locate blocks; the index is loaded
into memory when the SSTable is opened. A lookup
can be performed with a single disk seek: we first find
the appropriate block by performing a binary search in
the in-memory index, and then reading the appropriate
block from disk. Optionally, an SSTable can be completely 
mapped into memory, which allows us to perform
lookups and scans without touching disk.
```

https://storage.googleapis.com/pub-tools-public-publication-data/pdf/68a74a85e1662fe02ff3967497f31fda7f32225c.pdf

![SSTable](https://www.igvita.com/posts/12/xsstable.png.pagespeed.ic.IkMoqaKZX9.webp "SSTable")

```
A “Sorted String Table” then is exactly what it sounds 
like, it is a file which contains a set of arbitrary, 
sorted key-value pairs inside. 

Duplicate keys are fine, there is no need for “padding” 
for keys or values, and keys and values are arbitrary 
blobs. 

Read in the entire file sequentially and you have a 
sorted index. 

Optionally, if the file is very large, we can also 
prepend, or create a standalone key:offset index for 
fast access. 

That’s all an SSTable is: very simple, but also a 
very useful way to exchange large, sorted data segments.
```

https://www.igvita.com/2012/02/06/sstable-and-log-structured-storage-leveldb   

# What a release Rust binary depend upon?

```
> cargo build --release
    Finished release [optimized] target(s) in 0.01s
> ldd target/release/sstable
        linux-vdso.so.1 (0x00007fff838be000)
        libc.so.6 => /lib/x86_64-linux-gnu/libc.so.6 (0x00007f43ec8e1000)
        /lib64/ld-linux-x86-64.so.2 (0x00007f43ecc5f000)
        libpthread.so.0 => /lib/x86_64-linux-gnu/libpthread.so.0 (0x00007f43ec8be000)
        libdl.so.2 => /lib/x86_64-linux-gnu/libdl.so.2 (0x00007f43ec8b8000)
        libgcc_s.so.1 => /lib/x86_64-linux-gnu/libgcc_s.so.1 (0x00007f43ec89d000)
```

Quite a lot for such a small application. Let us understand them:

## linux-vdso.so.1

vdso - overview of the virtual ELF dynamic shared object
https://man7.org/linux/man-pages/man7/vdso.7.html  


```
The "vDSO" (virtual dynamic shared object) is a small shared library
that the kernel automatically maps into the address space of all
user-space applications.  Applications usually do not need to concern
themselves with these details as the vDSO is most commonly called by
the C library.  This way you can code in the normal way using
standard functions and the C library will take care of using any
functionality that is available via the vDSO.

Why does the vDSO exist at all?  There are some system calls the
kernel provides that user-space code ends up using frequently, to the
point that such calls can dominate overall performance.  This is due
both to the frequency of the call as well as the context-switch
overhead that results from exiting user space and entering the
kernel.
```

For example:

https://github.com/torvalds/linux/tree/948a64995aca6820abefd17f1a4258f5835c5ad9/arch/x86/um/vdso  

## libc.so.6

libc stands for "standard C library"

```
The term "libc" is commonly used as a shorthand for the "standard C
library", a library of standard functions that can be used by all C
programs (and sometimes by programs in other languages).  Because of
some history (see below), use of the term "libc" to refer to the
standard C library is somewhat ambiguous on Linux.
```
https://man7.org/linux/man-pages/man7/libc.7.html  

The original place for the GNU libc, "glibc" is:  
https://www.gnu.org/software/libc  

## /lib64/ld-linux-x86-64.so.2

This dynamic library is actually who interprets ELF binaries in linux. 
You can call this binary directly:

```
> /lib64/ld-linux-x86-64.so.2
Usage: ld.so [OPTION]... EXECUTABLE-FILE [ARGS-FOR-PROGRAM...]
You have invoked `ld.so', the helper program for shared library executables.
This program usually lives in the file `/lib/ld.so', and special directives
in executable files using ELF shared libraries tell the system's program
loader to load the helper program from this file.  This helper program loads
the shared libraries needed by the program executable, prepares the program
to run, and runs it.  You may invoke this helper program directly from the
command line to load and run an ELF executable file; this is like executing
that file itself, but always uses this helper program from the file you
specified, instead of the helper program file specified in the executable
file you run.  This is mostly of use for maintainers to test new versions
of this helper program; chances are you did not intend to run this program.

  --list                list all dependencies and how they are resolved
  --verify              verify that given object really is a dynamically linked
                        object we can handle
  --inhibit-cache       Do not use /etc/ld.so.cache
  --library-path PATH   use given PATH instead of content of the environment
                        variable LD_LIBRARY_PATH
  --inhibit-rpath LIST  ignore RUNPATH and RPATH information in object names
                        in LIST
  --audit LIST          use objects named in LIST as auditors
  --preload LIST        preload objects named in LIST
```

More on: https://linux.die.net/man/8/ld-linux    
More on: https://lwn.net/Articles/631631  

The real question: if this /lib/ld-linux.so.2 is who interprets the ELF binary
and runs it, how it is marked as dependency?


## libpthread.so.0

```
POSIX.1 specifies a set of interfaces (functions, header files) for
threaded programming commonly known as POSIX threads, or Pthreads.  A
single process can contain multiple threads, all of which are
executing the same program.  These threads share the same global
memory (data and heap segments), but each thread has its own stack
(automatic variables).
```
https://man7.org/linux/man-pages/man7/pthreads.7.html  

## libdl.so.2 

```
Programming interface to dynamic linking loader
```
https://linux.die.net/man/3/dlopen  

## libgcc_s.so.1

```
GCC provides a low-level runtime library, 
libgcc.a or libgcc_s.so.1 on some platforms. 
GCC generates calls to routines in this library 
automatically, whenever it needs to perform some 
operation that is too complicated to emit inline code for.
```
https://gcc.gnu.org/onlinedocs/gccint/Libgcc.html  

12.12. Interfaces for libgcc_s  
https://refspecs.linuxbase.org/LSB_4.1.0/LSB-Core-generic/LSB-Core-generic/libgcc-s.html  