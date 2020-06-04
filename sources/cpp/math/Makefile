DEPS = 
LLVMCONFIG := $(shell llvm-config-10 --cxxflags --ldflags)
LLVM_VERSION = 10
OBJ = math.o
OUTPUT = math.wasm
EXTRACTCONTRACT = -Xclang -load -Xclang $(PWD)/plugin/wasmPlugin.so -Xclang -add-plugin -Xclang wasm-extract-contract -Xclang -plugin-arg-wasm-extract-contract -Xclang 
COMPILE_FLAGS = --target=wasm32 \
		-nostdlib \
		--std=c++2a \
		-flto \
		-ffunction-sections \
		-fdata-sections \
		-O3

$(OUTPUT): src/%.o $(OBJ) Makefile
	wasm-ld-$(LLVM_VERSION) \
		-o $(OUTPUT) \
		--no-entry \
		-O3 \
		-lto-O3 \
		--export-dynamic \
		--gc-sections \
		--import-memory \
		$(OBJ)

%.o: src/%.cpp $(DEPS) Makefile	
	clang++-$(LLVM_VERSION) \
		-c \
		$(COMPILE_FLAGS) \
		-o $@ \
		$<

contract: math.cpp
	clang++-$(LLVM_VERSION) \
		--target=wasm32 -c math.cpp -o math.o -std=c++17 -O3 \
		$(EXTRACTCONTRACT) $(PWD)/math.json

plugin: ./plugin/wasmPlugin.cpp Makefile
	clang++-$(LLVM_VERSION) -g -std=c++2a plugin/wasmPlugin.cpp $(LLVMCONFIG) -o plugin/wasmPlugin.so -shared -fPIC 