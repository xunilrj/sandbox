#include <cassert>
#include <array>
#include <string>
#include <iostream>
#include <algorithm>

template<size_t N>
class Counter{
    public:
        Counter(size_t index):Values(),Index(index)
        {
            assert(index < N);
        }

        Counter& operator++(){
            ++Values[Index];
            return *this;
        }

        Counter operator & (const Counter<N>& other) const
        {
            auto result = Counter<N>(this->Index);
            for(int i = 0;i < N; ++i){
                result.Values[i] = std::max(this->Values[i],other.Values[i]);
            }            
            return result;
        }
    private:
        size_t Index;
        std::array<int, N> Values;

        template<size_t N2>
        friend std::ostream& operator <<(
            std::ostream& out,
            const Counter<N2>& counter);
};

template<class T, size_t N>
std::ostream& operator <<(
    std::ostream& out,
    const std::array<T, N> arr)
{
    out << "[";
    auto size = arr.size();
    for(int i = 0; i < size; ++i){    
        out << arr[i];
        if(i < (size-1)){
            out << ",";
        }
    }
    return out << "]";
}

template<size_t N>
std::ostream& operator << (
    std::ostream& out,
    const Counter<N>& counter)
{
    return std::cout << counter.Values;
}

int main(int argc, char ** argv)
{
    std::cout << "Running..." << std::endl;
    
    auto likesServer1 = Counter<2>{0};
    ++likesServer1;
    std::cout << likesServer1 << std::endl;

    auto likesServer2 = Counter<2>{1};
    ++likesServer2;
    ++likesServer2;
    std::cout << likesServer2 << std::endl;

    likesServer1 = likesServer1 & likesServer2;
    std::cout << likesServer1 << std::endl;

    return 0;
}