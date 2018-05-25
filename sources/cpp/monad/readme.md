# Searching a abstraction free C++ Monad

## First Try

    #include <functional>
    template<typename T>
    class nullable
    {
    public:
        nullable() : IsNull(true), Data(nullptr) { }
        nullable(std::nullptr_t) : IsNull(true), Data(nullptr) { }
        nullable(T v) : IsNull(false), Value(v) { }
        operator T () const { return Value; }
    private:
        bool IsNull;
        union
        {
            void* Data;
            T Value;
        };
    };
    #define F2 [](nullable<int> a, nullable<int> b) -> nullable<int>
    #define LIFT(F) [](nullable<int> a, nullable<int> b) -> nullable<int> {F(a,b);}
    int square(int num) {
        auto mul = F2{return a*b;};
        auto div = F2{
            if(b == 0) return nullptr;
            return a/b;
        };   
        return div(num, mul(num,num));
    }
    int square2(int num) {
        auto r = num*num;
        if(r == 0) return 0;
        else return r / num;
    }

### Generated Assembly

    GCC 8.1 -O3

    square(int):    ;int EDI
    mov ecx, edi    ;ECX = EDI
    imul ecx, edi   ;ECX *= EDI
    test ecx, ecx   ;ZF = !(ECX & ECX)
    je .L1          ;IF(ZF == 1) goto .L1
    mov eax, edi    ;EAX = EDI
    cdq             ;EDX = 0 - EAX[31] fills EDX with the most significant bit EAX: 
    idiv ecx        ;EDX:EAX /= ECX
    mov ecx, eax    ;ECX = EAX
    .L1:
    mov eax, ecx    ;EAX = ECX
    ret             ;return
    square2(int):   ;int EDI
    mov eax, edi    ;EAX = EDI
    imul eax, edi   ;EAX *= EDI
    test eax, eax   ;ZF = !(EAX & EAX)
    cmovne eax, edi ;IF(ZF == 1) EAX = EDI
    ret
