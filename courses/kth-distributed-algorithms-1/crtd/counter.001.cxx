#include <iostream>

class Counter{
    public:
        Counter():Value(0){
        }

        Counter& operator++(){
            ++Value;
            return *this;
        }
    private:
        int Value;

        friend std::ostream& operator <<(
            std::ostream& out,
            const Counter& counter);
};

std::ostream& operator << (
    std::ostream& out,
    const Counter& counter)
{
    return std::cout << counter.Value;
}

int main(int argc, char ** argv)
{
    std::cout << "Running..." << std::endl;
    
    auto likesServer1 = Counter{};
    ++likesServer1;
    std::cout << likesServer1 << std::endl;

    auto likesServer2 = Counter{};
    ++likesServer2;
    ++likesServer2;
    std::cout << likesServer2 << std::endl;

    return 0;
}