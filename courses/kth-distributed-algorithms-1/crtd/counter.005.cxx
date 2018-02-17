#include <cassert>
#include <array>
#include <string>
#include <iostream>
#include <algorithm>
#include <numeric>

template<size_t N>
class Counter{
    public:
        Counter(size_t index):Index(index),
            Increments(),
            Decrements()
        {
            assert(index < N);
        }

        Counter& operator++(){
            ++Increments[Index];
            return *this;
        }

        Counter& operator--(){
            ++Decrements[Index];
            return *this;
        }

        Counter operator & (const Counter<N>& other) const
        {
            auto result = Counter<N>(this->Index);
            for(int i = 0;i < N; ++i){
                result.Increments[i] = std::max(
                    this->Increments[i],
                    other.Increments[i]);
            }   
            for(int i = 0;i < N; ++i){
                result.Decrements[i] = std::max(
                    this->Decrements[i],
                    other.Decrements[i]);
            }          
            return result;
        }

        int getCurrentValue() const
        {
            auto value = std::accumulate(
                std::begin(Increments), std::end(Increments),
                0, [](auto& acc, auto& current){
                    return acc + current;
                });
            return std::accumulate(
                std::begin(Decrements), std::end(Decrements),
                value, [](auto& acc, auto& current){
                    return acc - current;
                });
        }
    private:
        size_t Index;
        std::array<int, N> Increments;
        std::array<int, N> Decrements;

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
    std::cout << "Increments: " << counter.Increments << std::endl;
    std::cout << "Decrements: " << counter.Decrements << std::endl;
}

int main(int argc, char ** argv)
{
    std::cout << "Running..." << std::endl;
    
    auto likesServer1 = Counter<2>{0};
    ++likesServer1;
    std::cout << "Server 1" << std::endl;
    std::cout << likesServer1 << std::endl;

    auto likesServer2 = Counter<2>{1};
    ++likesServer2;
    ++likesServer2;
    --likesServer2;
    std::cout << "Server 2" << std::endl;
    std::cout << likesServer2 << std::endl;

    likesServer1 = likesServer1 & likesServer2;
    std::cout << "Merged Server 1" << std::endl;
    std::cout << likesServer1 << std::endl;

    std::cout << "Final Value" << std::endl;
    std::cout << likesServer1.getCurrentValue() << std::endl;

    return 0;
}