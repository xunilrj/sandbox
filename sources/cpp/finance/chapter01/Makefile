BUILDDIR=.build

ELFLIBCNONSHARED=/usr/lib/x86_64-linux-gnu/libc_nonshared.a
ELFLIBGCC=/usr/lib/gcc/x86_64-linux-gnu/9/libgcc.a
ELFLIBC=/usr/lib/x86_64-linux-gnu/libc.a
ELFCRT1=/usr/lib/x86_64-linux-gnu/crt1.o
ELFCRTI=/usr/lib/x86_64-linux-gnu/crti.o	
ELFLIBM=/usr/lib/x86_64-linux-gnu/libm.a
ELFLIBSTDCPP=/usr/lib/gcc/x86_64-linux-gnu/9/libstdc++.a
ELFLIBGCC_S=/lib/x86_64-linux-gnu/libgcc_s.so.1
ELFLIBS= $(ELFLIBCNONSHARED) $(ELFLIBGCC) $(ELFLIBGCC_S) $(ELFLIBC) $(ELFCRTI) $(ELFCRT1) $(ELFLIBM) $(ELFLIBSTDCPP)
%.o: %.cpp
	mkdir .build -p
	clang++-10 $< -g -c -static -o $(BUILDDIR)/$@ 

elf: main.o Random1.o
	# "/usr/bin/ld" -z relro --hash-style=gnu --build-id --eh-frame-hdr -m elf_x86_64 -dynamic-linker /lib64/ld-linux-x86-64.so.2 -o .build/main /usr/bin/../lib/gcc/x86_64-linux-gnu/9/../../../x86_64-linux-gnu/crt1.o /usr/bin/../lib/gcc/x86_64-linux-gnu/9/../../../x86_64-linux-gnu/crti.o /usr/bin/../lib/gcc/x86_64-linux-gnu/9/crtbegin.o -L/usr/bin/../lib/gcc/x86_64-linux-gnu/9 -L/usr/bin/../lib/gcc/x86_64-linux-gnu/9/../../../x86_64-linux-gnu -L/lib/x86_64-linux-gnu -L/lib/../lib64 -L/usr/lib/x86_64-linux-gnu -L/usr/bin/../lib/gcc/x86_64-linux-gnu/9/../../.. -L/usr/lib/llvm-10/bin/../lib -L/lib -L/usr/lib .build/main.o .build/Random1.o -lstdc++ -lm -lgcc_s -lgcc -lc -lgcc_s -lgcc /usr/bin/../lib/gcc/x86_64-linux-gnu/9/crtend.o /usr/bin/../lib/gcc/x86_64-linux-gnu/9/../../../x86_64-linux-gnu/crtn.o
	clang++-10 $(BUILDDIR)/main.o $(BUILDDIR)/Random1.o -g -o $(BUILDDIR)/main -v
	#ld.lld-10 $(ELFLIBS) $(BUILDDIR)/main.o $(BUILDDIR)/Random1.o -g -o $(BUILDDIR)/main

wasm: main.o Random1.o
	wams-ld main.o Random1.o -g -o main.wasm

clean:
	rm $(BUILDDIR) -rf