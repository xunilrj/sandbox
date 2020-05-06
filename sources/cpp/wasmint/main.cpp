#define CATCH_CONFIG_MAIN 
#include "../catch/catch.hpp"

#include <Windows.h>
#include <string>

#include <stack>
#include <unordered_map>


template <typename T>
T& read(uint8_t*& p)
{
    auto* ptr = (T*)p;
    p += sizeof(T);

    return *ptr;
}

uint32_t readULEB128(uint8_t*& p)
{
    uint32_t result = 0;
    uint32_t shift = 0;
    while (true)
    {
        uint8_t byte = (*p); ++p;
        result |= (byte & 0b01111111) << shift;
        if ((byte & 0b10000000) == 0)
            break;
        shift += 7;
    }

    return result;
}

int32_t readLEB128(uint8_t*& p)
{
    size_t size = 32;

    int32_t result = 0;
    int32_t shift = 0;

    uint8_t byte;
    do {
        byte = (*p); ++p;
        result |= (byte & 0b01111111) << shift;
        shift += 7;
    } while ((byte & 0b10000000) != 0);

    if ((shift < size) && (byte & 0b10000000))
        result |= (~0 << shift);

    return result;
}

std::string readString(uint8_t*& p)
{
    auto nameSize = readULEB128(p);

    auto str = std::string((char*)p, nameSize);

    p += nameSize;

    return str;
}

float readFloat(uint8_t*& p)
{
    auto v = *(float*)p;
    p += 4;
    return v;
}

template <typename T>
std::vector<T> readVector(uint8_t*& p)
{
    auto v = std::vector<T>{};

    auto vectorSize = readULEB128(p);
    while (vectorSize > 0)
    {
        auto item = T::read(p);
        v.push_back(item);

        --vectorSize;
    }

    return v;
}

uint8_t readOPCode(uint8_t*& p)
{
    auto opcode = *p;
    ++p;

    return opcode;
}

namespace wasm
{
    

    struct TypeSection
    {
        uint8_t id;
        uint32_t size;
    };

    struct Export
    {
        static Export read(uint8_t*& p)
        {
            auto name = ::readString(p);
            auto type = ::read<uint8_t>(p);
            auto idx = ::read<uint8_t>(p);

            return { name, type, idx };
        }

        std::string name;
        uint8_t type;
        uint8_t idx;
    };

    struct ExportSection
    {
        static ExportSection read(uint8_t*& p)
        {
            auto size = readULEB128(p);
            auto exports = readVector<wasm::Export>(p);

            auto temp = ExportSection{ size, exports };
            temp.buildIndex();

            return temp;
        }

        uint32_t size;
        std::vector<Export> exports;

        ExportSection()
        {
        }

        ExportSection(uint32_t size, const std::vector<Export>& exports) : size{ size }, exports{ exports }
        {
        }

        const Export* operator [] (const std::string& name) const
        {
            auto it = byName.find(name);
            if (it != byName.end()) return it->second;
            else return nullptr;
        }
    private:
        std::unordered_map<std::string, Export*> byName;
        void buildIndex()
        {
            byName.clear();
            for (auto&& exp : exports)
            {
                byName.emplace(exp.name, &exp);
            }
        }
    };

    struct CodeLocal
    {
        static CodeLocal read(uint8_t*& p)
        {
            auto n = readULEB128(p);
            auto type = readLEB128(p);
            return { n, type };
        }

        uint32_t n;
        int32_t type;
    };

    struct Code
    {
        static Code read(uint8_t*& p)
        {
            auto size = readULEB128(p);
            auto end = p + size;
            auto locals = readVector<wasm::CodeLocal>(p);
            auto temp = Code{ size, locals, p };

            p = end;

            return temp;
        }

        uint32_t size;
        std::vector<CodeLocal> locals;
        uint8_t* start;
    };

    struct CodeSection
    {
        static CodeSection read(uint8_t*& p)
        {
            auto size = ::readULEB128(p);
            auto codes = ::readVector<wasm::Code>(p);

            return { size, codes };
        }
       
        uint32_t size;
        std::vector<Code> codes;
    };

    struct Import
    {
        static Import read(uint8_t*& p)
        {
            auto mod = ::readString(p);
            auto name = ::readString(p);
            auto type = ::read<uint8_t>(p);
            auto param1 = ::readULEB128(p);

            return { mod , name, type, param1 };
        }

        std::string mod;
        std::string name;
        uint8_t type;
        uint32_t param1;
    };

    struct ImportSection
    {
        static ImportSection read(uint8_t*& p)
        {
            auto size = ::readULEB128(p);
            auto imports = ::readVector<wasm::Import>(p);

            return { size, imports };
        }

        uint32_t size;
        std::vector<Import> imports;
    };

    struct Func
    {
        static Func read(uint8_t*& p)
        {
            auto typeidx = ::readULEB128(p);

            return { typeidx };
        }

        uint32_t typeidx;
    };

    struct FuncSection
    {
        static FuncSection read(uint8_t*& p)
        {
            auto size = ::readULEB128(p);
            auto funcs = ::readVector<wasm::Func>(p);

            return { size, funcs };
        }

        uint32_t size;
        std::vector<Func> funcs;
    };

    struct FuncRecord
    {
        static FuncRecord import(Import* imp)
        {
            return { true, imp, 0 };
        }

        static FuncRecord code(uint32_t idx, Code code)
        {
            return { false, nullptr, idx };
        }

        bool imported;
        Import* imp;
        uint32_t codeidx;
    };

    struct FunctionRegister
    {
        std::vector<FuncRecord> funcs;
    };

    namespace execution
    {
        struct value
        {
            union
            {
                int32_t i32;
                int64_t i64;

                uint32_t u32;
                uint64_t u64;

                float f32;
                double f64;
            };

            value(int32_t v) : i32{ v } {}
            value(float v) : f32{ v } {}
        };

        struct callframe
        {
            FunctionRegister* funcs;
            uint8_t* IP;
        };

        enum class step_result_type
        {
            normal,
            end,
            call_imported
        };

        struct step_result_call_imported
        {
            FuncRecord* f;
        };

        struct step_result
        {
            step_result_type type;
            union
            {
                step_result_call_imported call_imported;
            };

            step_result() {}

            step_result(step_result_type type) : type { type }
            {
            }

            step_result(step_result_call_imported imp)
                : type{ step_result_type::call_imported }, call_imported{ imp }
            {

            }
        };

        struct thread
        {
            std::stack<callframe> callstack;
            std::stack<value> valuestack;

            thread(FunctionRegister* mod, uint8_t* ip)
            {
                if(ip != nullptr)
                    callstack.push({ mod,ip });
            }

            step_result step()
            {
                if (callstack.size() == 0)
                    return { step_result_type::end };

                auto& callframe = callstack.top();
                uint8_t*& IP = callframe.IP;

                //https://developercommunity.visualstudio.com/idea/965226/supoport-the-labels-as-values-in-cc-compiler-to-he.html
                //https://gcc.gnu.org/onlinedocs/gcc/Labels-as-Values.html
                //https://eli.thegreenplace.net/2012/07/12/computed-goto-for-efficient-dispatch-tables
                static void* dispatch_table[] = {
                    0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 
                    &&call                    
                };
                auto opcode = readOPCode (IP);

                // 0x10 x:funcidx => call x
                if (opcode == 0x10)
                {
                    call:
                    auto operand = readULEB128(IP);
                    auto& f = callframe.funcs->funcs[operand];
                    if (f.imported)
                    {
                        return { step_result_call_imported{&f} };
                    }
                    //TODO find this function
                }
                else if (opcode == 0x41)
                {
                    auto operand = readLEB128(IP);
                    valuestack.push(operand);
                }
                else if (opcode == 0x43)
                {
                    auto operand = readFloat(IP);
                    valuestack.push(operand);
                }
                else if (opcode == 0x0b)
                {
                    callstack.pop();
                    //end

                    return { step_result_type::end };
                }

                return { step_result_type::normal };
            }

            step_result stepUntilImport()
            {
                step_result r;
                do
                {
                    r = step();
                } while (r.type != step_result_type::call_imported);

                return r;
            }
        };
    }

    struct ModuleHeader
    {
        char magic[4];
        uint32_t version;
    };

    struct Module
    {
        ModuleHeader header;
        ExportSection exportSection;
        CodeSection codeSection;
        ImportSection importSection;
        FuncSection funcSection;

        FunctionRegister funcRegister;

        Module from(uint8_t*& p)
        {
            auto mod = wasm::Module{};
            mod.header = read<wasm::ModuleHeader>(p);

            return mod;
        }
        

        void readAllSections(uint8_t*& p, uint32_t size)
        {
            auto lastSection = 0;
            while (size > 0)
            {
                auto [section,read] = readSection(p);
                size -= read;

                if (read == 0) break;

                lastSection = section;
                auto nextSection = *p;

                if ((nextSection < 0) && (nextSection >= 11)) break;
            }
            //assert(size == 0); //file bigger than necessary. Why?

            setupFuncRegister();
        }

        std::tuple<int8_t,uint32_t> readSection(uint8_t*& p)
        {
            auto sectionType = read<uint8_t>(p);
            switch (sectionType)
            {
                case 2:
                {
                    importSection = wasm::ImportSection::read(p);
                    return { sectionType, importSection.size };
                }
                case 3:
                {
                    funcSection = wasm::FuncSection::read(p);
                    return  { sectionType,funcSection.size };
                }
                case 7:
                {
                    exportSection = wasm::ExportSection::read(p);
                    return  { sectionType,exportSection.size };
                }
                case 10:
                {
                    codeSection = wasm::CodeSection::read(p);
                    return  { sectionType,codeSection.size };
                }
                default:
                {
                    
                    auto size = readULEB128(p);
                    p += size;
                    return  { sectionType,size };
                }
            }

            return { -1, 0 };
        }

        void setupFuncRegister()
        {
            for (auto&& x : importSection.imports)
            {
                funcRegister.funcs.emplace_back(FuncRecord::import(&x));
            }

            for (auto i = 0; i < codeSection.codes.size(); ++i)
            {
                auto& x = codeSection.codes[i];
                funcRegister.funcs.emplace_back(FuncRecord::code(i, x));
            }
        }

        execution::thread spawn(const std::string& name)
        {
            auto* exp = exportSection[name];
            if (exp == nullptr) return { &funcRegister, nullptr };

            auto codeidx = funcRegister.funcs[exp->idx].codeidx;
            auto code = codeSection.codes[codeidx];
            return execution::thread{ &funcRegister, code.start };
        }
    };
}


TEST_CASE("Fake.Test.Will Pass", "[ok]")
{
    //auto fileName = TEXT("D:/github/sandbox/sources/cpp/wasmint/example1.wasm");
    auto fileName = TEXT("D:/github/sandbox/sources/cpp/wasmint/script.c.wasm");

    SYSTEM_INFO SysInfo;
    GetSystemInfo(&SysInfo);
    DWORD dwSysGran = SysInfo.dwAllocationGranularity;

    DWORD FILE_MAP_START = 0;
    DWORD BUFFSIZE = 1024;
    DWORD dwFileMapStart = (FILE_MAP_START / dwSysGran) * dwSysGran;
    DWORD dwFileMapSize = FILE_MAP_START + BUFFSIZE;
    DWORD dwMapViewSize = (FILE_MAP_START % dwSysGran) + BUFFSIZE;

    HANDLE hFile = CreateFile(fileName,
            GENERIC_READ | GENERIC_WRITE,
            0,
            NULL,
            OPEN_EXISTING,
            FILE_ATTRIBUTE_NORMAL,
            NULL);
    REQUIRE(hFile != INVALID_HANDLE_VALUE);
    
    DWORD dwFileSize = GetFileSize(hFile, nullptr);

    HANDLE hMapFile = CreateFileMapping(hFile,
            NULL,
            PAGE_READWRITE,
            0,
            dwFileMapSize,
            NULL);
    REQUIRE(hMapFile != NULL);

    auto* lpMapAddress = (uint8_t *)MapViewOfFile(hMapFile,
            FILE_MAP_ALL_ACCESS,
            0,
            dwFileMapStart,
            dwMapViewSize);
    REQUIRE(lpMapAddress != NULL);

    auto mod = wasm::Module{};
    
    mod.header = wasm::ModuleHeader::from(lpMapAddress);
    mod.readAllSections(lpMapAddress, dwFileSize);

    auto t = mod.spawn("helloWorld1");
    auto r = t.stepUntilImport();
}