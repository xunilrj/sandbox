
//n^2 flops
bool matrix_foward_substitution(int N, float * A, float * b)
{
    auto at = [&](const int x, const int y) {return (x*N)+y;};
    for(int i = 0; i < N; ++i)
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

int main(int argc, char **argv)
{
    float A[] = {2,0,0, 0,2,0, 0,0,2};
    float b[] = {2,4,6};
    auto r = matrix_foward_substitution(3, A, b);
    std::cout << std::boolalpha << r << " ";
    print(std::cout, 3, b) << std::endl;
    return 0;
}