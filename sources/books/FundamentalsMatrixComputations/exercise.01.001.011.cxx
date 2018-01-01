#include <cstdlib>
#include <algorithm>
#include <array>
#include <chrono>
#include <iostream>

float randf()
{
    return (float)std::rand() * static_cast<float>(RAND_MAX);
}

void generate_rand_n(unsigned int n, float * buffer)
{
    std::generate_n(buffer, n, randf);
}

template<class T>
void generate_rand(T& container)
{
    std::generate(
        std::begin(container),
        std::end(container),
        randf);
}

void matrix_vector_mul(int N, const float * A,
    const float* b,
    float * c)
{
    auto at = [&](const int x, const int y) {return (x*N)+y;};
    for(int j  = 0;j < N; ++j)
    {
        for(int i = 0; i < N; ++i)    
        {
            c[j] = c[j] + A[at(i,j)] * b[i];
        }
    }
}

int main(int argc, char ** argv)
{
    using milli = std::chrono::milliseconds;
    auto start = std::chrono::high_resolution_clock::now();

    const int size = 32678;
    const int size2 = size*size;
    float * A = new float[size2];
    float * b = new float[size];
    generate_rand_n(size2, A);
    generate_rand_n(size, b);
    float * r = new float[size];
    matrix_vector_mul(size, A,b,r);

    auto finish = std::chrono::high_resolution_clock::now();
    std::cout << "matrix_vector_mul() took "
              << std::chrono::duration_cast<milli>(finish - start).count()
              << " milliseconds\n";
    return 0;
}