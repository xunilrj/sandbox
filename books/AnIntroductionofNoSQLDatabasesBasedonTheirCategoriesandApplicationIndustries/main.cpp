
#include <vector>
#include <algorithm>

template <typename T, template <typename, typename = std::allocator<T>> class TContainer>
class containerSet
{
    TContainer<T> items;
public:
    containerSet<T, TContainer>& operator << (const T& item) & { this->add(item); return *this; }
    bool operator += (const T& item) & { return this->add(item); }
    bool add(const T& item) &
    {
        auto end = items.end();
        auto it = std::find(items.begin(), end, item);
        if(it == end) {
            items.push_back(item);
            return true;
        } else {
            return false;
        }
    }

    bool operator[](const T& item) const & { return this->contains(item); }
    bool contains(const T& item) const &
    {
        auto end = items.end();
        auto it = std::find(items.begin(), end, item);
        if(it == end) {
            return false;
        } else {
            return true;
        }
    }

    auto begin() const & { return items.begin();}
    auto end() const & { return items.end();}

    containerSet<T, TContainer> operator +(const containerSet<T, TContainer>& set) const { return this->merge(set); }
    containerSet<T, TContainer> merge(const containerSet<T, TContainer>& set) const
    {
        auto newSet = containerSet<T, TContainer>{};
        for(auto&& x : *this) newSet.add(x);
        for(auto&& x : set) newSet.add(x);
        return newSet;
    }

    containerSet<T, TContainer> operator ^(const containerSet<T, TContainer>& set) const { return this->intersection(set); }
    containerSet<T, TContainer> intersection(const containerSet<T, TContainer>& set) const
    {
        auto newSet = containerSet<T, TContainer>{};
        for(auto&& x : *this) {
            if(set.contains(x))
                newSet.add(x);
        }
        for(auto&& x : set) {
            if(this->contains(x))
                newSet.add(x);
        }
        return newSet;
    }

    containerSet<T, TContainer> operator -(const containerSet<T, TContainer>& set) const { return this->difference(set); }
    containerSet<T, TContainer> difference(const containerSet<T, TContainer>& set) const
    {
        auto newSet = containerSet<T, TContainer>{};
        for(auto&& x : *this) {
            if(!set.contains(x))
                newSet.add(x);
        }        
        return newSet;
    }
};

template <typename T> using vectorSet = containerSet<T, std::vector>;


#include <windows.h>
#include <iostream>
#include <string>
using namespace std::literals;
template <typename T> 
struct colorOutput { int color; const T value; };
colorOutput<const char *> operator ""_red(const char * value, unsigned long long) { return {FOREGROUND_RED, value}; }
colorOutput<const char *> operator ""_green(const char * value, unsigned long long) { return {FOREGROUND_GREEN, value}; }
template <typename T> std::ostream& operator << (std::ostream& out, const colorOutput<T>&& txt)
{
    HANDLE                      m_hConsole;
    WORD                        m_currentConsoleAttr;
    CONSOLE_SCREEN_BUFFER_INFO   csbi;

    m_hConsole=GetStdHandle(STD_OUTPUT_HANDLE);
    if(GetConsoleScreenBufferInfo(m_hConsole, &csbi))
        m_currentConsoleAttr = csbi.wAttributes;

    SetConsoleTextAttribute (
            m_hConsole,
            txt.color);

    out << txt.value;

    SetConsoleTextAttribute (
            m_hConsole,
            m_currentConsoleAttr);

    return out;
}
template <typename T>
void assert(const char * str, T expected, T actual)
{
    auto r = expected == actual;
    if(r)
        std::cout << str << ": " << "ok"_green << std::endl;
    else
    std::cout << str << ": " << "false"_red << " expected: [" << expected << "] actual: [" << actual << "]" << std::endl;
}

int main(int argc, char** argv)
{
    auto set1 = vectorSet<int>{};
    assert("set1 add  0", true, set1 += 0);
    assert("set1 add  1", true, set1 += 1);
    assert("set1 add  2", false, set1 += 1);
    assert("set1 add  3", true, set1[1]);
    assert("set1 add  4", 2ll, std::count_if(set1.begin(), set1.end(), [](auto&& x) { return true;}));

    auto set2 = vectorSet<int>{};
    set2 << 2;

    auto set3 = set1 + set2;
    assert("set1 add  5", 3ll, std::count_if(set3.begin(), set3.end(), [](auto&& x) { return true;}));

    auto set4 = set3  ^ set1;
    assert("set1 add  6", 2ll, std::count_if(set4.begin(), set4.end(), [](auto&& x) { return true;}));
    assert("set1 add  7", true, set4[0]);
    assert("set1 add  8", true, set4[1]);

    auto set5 = set3 - set1;
    assert("set1 add  9", 1ll, std::count_if(set5.begin(), set5.end(), [](auto&& x) { return true;}));
    assert("set1 add 10", true, set5[2]);
    return 0;
}