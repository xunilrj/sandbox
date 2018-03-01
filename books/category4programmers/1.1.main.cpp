#include <string>
#include <sstream>
#include <iostream>
#include <functional>

int f (double x){ return x; }
std::string g(int x) { 
    std::stringstream ss;
    ss << x;
    return ss.str();
}

std::string g_after_f(double x) {return g(f(x));}

template <typename T1, typename TR>
using Func = TR(*)(T1);

template <typename T1, typename TR1, typename TR>
std::function<TR(T1)> compose(Func<T1,TR1> f, Func<TR1,TR> g)
{
    return [=](auto x) { return g(f(x)); };
}

//TODO template for biggest return
template <typename T1, typename TR, int N>
class F
{
public:
    int* Targets[N];
    F(){}
    F(Func<T1,TR> target)
    {        
        Targets[0] = (int*)(void*)target;
    }
    template <typename RTR, int RN>
    F<T1,RTR,N+RN> operator | (const F<TR,RTR,RN> r) const noexcept
    {
        F<T1,RTR,N+RN> nf;
        memcpy(nf.Targets, this->Targets, sizeof(int*)*N);
        memcpy(nf.Targets + N, r.Targets, sizeof(int*)*RN);
        return nf;
    }
    TR operator () (T1 x) const
    {
        //allocate on the stack biggest element
        //call all functions
        auto first = Targets[0];
        return ((Func<T1,TR>)(void*)first)(x);
    }
};

template <typename T1, typename TR>
F<T1,TR,1> make_f(Func<T1,TR> f)
{
    return {f};
}

template <typename T> T id(T x) { return x; }

int main()
{
    std::cout << g_after_f(12.0) << std::endl;
    std::cout << compose(f, g)(14.0) << std::endl;

    auto of = make_f(f);
    auto og = make_f(g);
    auto composed = of|og;

    // std::cout << composed(24.0) << std::endl;

    std::cout << id(10) << std::endl;

    auto f1 = compose(f,id<int>);
    auto f2 = compose(id<double>,f);

    //randomly call f1,f2,f and check for equal
}