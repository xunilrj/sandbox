#include <iostream>
    #include <string>
    
    struct Person
    {
        std::string Name;
    };

    int main(int argc, char** argv)
    {
        std::cout << "Hello before" << std::endl;

        Person* me = nullptr;
        me->Name = "Daniel";

        std::cout << "Hello after" << std::endl;
        return 0;
    }