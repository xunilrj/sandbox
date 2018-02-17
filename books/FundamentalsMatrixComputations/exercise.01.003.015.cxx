
//n^2 flops
bool matrix_backwards_substitution(int N, float * A, float * b)
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

void run_and_print(int N, float * A, float * b)
{
    auto r = matrix_backwards_substitution(3, A, b);
    std::cout << std::boolalpha << r << " ";
    print(std::cout, 3, b) << std::endl;
}

int main(int argc, char **argv)
{
    float A1[] = {2,0,0, 0,2,0, 0,0,2};
    float b1[] = {2,4,6};
    run_and_print(3, A1, b1);

    float A2[] = {2,0,0, 0,2,0, 0,0,2};
    float b2[] = {2,0,6};
    run_and_print(3, A2, b2);
    return 0;
}