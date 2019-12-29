#include <iostream>
#include <string>
#include "deltaTime.h"

struct SomeData
{
    char junk[32];
    float x;
    char doesntMatter[32];
};

int main(int argc, char** argv)
{
    for(int i = 0;i < argc; ++i)
    {
        std::cout << i << ":" << argv[i] << std::endl;
    }

    std::string argv1 = ""; if(argc >= 2) { argv1 = argv[1]; }
    std::string argv2 = ""; if(argc >= 3) { argv2 = argv[2]; }

    std::cout << "size" << ";" << "colunar" << ";" << "row" << std::endl;
    auto sizes = {1000, 10000, 100000, 1000000, 10000000};
    for(auto size : sizes)
    {
        std::cout << size << ";";
        if((argv1 == "col")||(argv2 == "col"))
        {
            float* f = new float[size];
            auto t = deltaTime<double>{true};

            float accum = 0;
            for(int i = 0; i < size; ++i)
            {
                accum += f[i];
            }

            auto t1 = t.elapsed();
            delete[] f;
            std::cout << t1 << ";";
        }

        if((argv1 == "row")||(argv2 == "row"))
        {
            SomeData* arr = new SomeData[size];
            auto t = deltaTime<double>{true};

            float accum = 0;
            for(int i = 0; i < size; ++i)
            {
                accum += arr[i].x;
            }

            auto t2 = t.elapsed();
            delete[] arr;
            std::cout << t2 << ";";
        }

        std::cout << std::endl;
    }
    return 0;
}