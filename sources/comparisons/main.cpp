#include <iostream>
#include <array>
#include <vector>
#include <algorithm>

void axpy(float __restrict__  *a, float __restrict__  * x, float __restrict__  * y, int n)
{
    for(auto i = 0;i < n;i+=4)
    {
        a[i+0] = x[i+0]+y[i+0];
        a[i+1] = x[i+1]+y[i+1];
        a[i+2] = x[i+2]+y[i+2];
        a[i+3] = x[i+3]+y[i+3];
    }
}

int main(int argc, char ** argv)
{
    const int size = 10 * 1000 * 1000;
    auto a = std::vector<float>(size);
    auto x = std::vector<float>(size);
    auto y = std::vector<float>(size);

    std::generate(std::begin(a),std::end(a), [](){return (float)std::rand();});
    std::generate(std::begin(x),std::end(x), [](){return (float)std::rand();});
    std::generate(std::begin(y),std::end(y), [](){return (float)std::rand();});

    for(auto i = 0;i < 1000; ++i)
    {
        axpy(&a[0], &x[0],&y[0], size);
    }
    auto result = 0.0f;
    for(auto i = 0;i < size; ++i)
    {
        result += x[i];
    }
    return result;
}