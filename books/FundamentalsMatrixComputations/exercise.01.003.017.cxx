
template <typename T>
class localarray{
public:
    localarray(T * v) : Value(v) {}
    ~localarray() { delete [] Value;}
private:
    T * Value;
};


bool matrix_foward_substitution_row_oriented(int N, float * A, float * b)
{
    auto at = [&](const int x, const int y) {return (x*N)+y;};
    int i = 0;
    for(;i < N;++i)
    {
        float& bi = b[i];
        if(bi == 0) continue;
        else break;
    }
    for(;i < N; ++i)
    {
        float& bi = b[i];
        for(int j = 0; j < (i-1);++j)
        {
            bi = bi - A[at(i,j)]*b[j];
        }
        auto denominator = A[at(i,i)];
        if(denominator == 0) return false;
        bi = bi / denominator;
    }
    return true;
}

bool matrix_foward_substitution_column_oriented(int N, float * A, float * b)
{
    auto at = [&](const int x, const int y) {return (x*N)+y;};
    int i = 0;
    for(;i < N;++i)
    {
        float& bi = b[i];
        if(bi == 0) continue;
        else break;
    }
    for(;i < N; ++i)
    {
        float& Aii = A[at(i,i)];
        if(Aii == 0) return false;

        float& bi = b[i];
        bi /= Aii;
        for(int j = i + 1;j < N; ++j)
        {
            float& bj = b[j];
            bj -= A[at(j,i)]*bi;
        }
    }
    return true;
}

bool matrix_backwards_substitution_row_oriented(int N, float * A, float * b)
{
    auto at = [&](const int x, const int y) {return (x*N)+y;};
    int i = N-1;
    for(;i >= 0;--i)
    {
        float& bi = b[i];
        if(bi == 0) continue;
        else break;
    }
    for(;i >= 0; --i)
    {
        float& bi = b[i];
        for(int j = N-1; j > i;--j)
        {
            bi = bi - A[at(i,j)]*b[j];
        }
        auto denominator = A[at(i,i)];
        if(denominator == 0) return false;
        bi = bi / denominator;
    }
    return true;    
}

bool matrix_backwards_substitution_column_oriented(int N, float * A, float * b)
{
    auto at = [&](const int x, const int y) {return (x*N)+y;};
    int i = N-1;
    for(;i >= 0;--i)
    {
        float& bi = b[i];
        if(bi == 0) continue;
        else break;
    }
    for(;i >= 0; --i)
    {
        float& Aii = A[at(i,i)];
        if(Aii == 0) return false;

        float& bi = b[i];
        bi /= Aii;
        for(int j = i - 1;j >= 0; --j)
        {
            float& bj = b[j];
            bj -= A[at(j,i)]*bi;
        }
    }
    return true;
}

#include <iostream>

std::ostream& print(std::ostream& out, int N, float* vector)
{
    out << "[";
    for(int i = 0;i < N;++i)
    {
        out << vector[i] << ",";
    }
    return out << "]";
}

#include <algorithm>

template <typename F>
void run_and_print(F f, int N, float * A, float * originalB)
{
    float *b = new float[N];
    auto x =  localarray<float>{b};

    std::copy_n(originalB, N, b);
    auto r = f(3, A, b);
    std::cout << std::boolalpha << r << " ";
    print(std::cout, 3, b) << std::endl;
}

int main(int argc, char **argv)
{
    float A1[] = {3,2,1,0, 0,1,2,3, 0,0,-2,1, 0,0,0,4};
    float b1[] = {-10,10,1,12};
    
    run_and_print(matrix_foward_substitution_row_oriented, 4, A1, b1);
    run_and_print(matrix_foward_substitution_column_oriented, 4, A1, b1);
    run_and_print(matrix_backwards_substitution_row_oriented, 4, A1, b1);
    run_and_print(matrix_backwards_substitution_column_oriented, 4, A1, b1);
    return 0;
}