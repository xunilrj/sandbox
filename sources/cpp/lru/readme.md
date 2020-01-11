# Build - CMake

```
> mkdir lru
> cd lru
> code .
```

First, we will create a file named "CMakeLists.txt". A little bit exotic name, but this file will be used by CMake to generate the compilation artefacts for all c++ toolchains. In our case here we will be using "Visual Studio".

So, our first step is to install CMake. If you are using windows and have Chocolatey installed it is as simple as 

```
> choco install CMake
```

If you don't have Chocolatey and to install it just run, or see more at https://chocolatey.org/install.

```
> Set-ExecutionPolicy Bypass -Scope Process -Force; iex ((New-Object System.Net.WebClient).DownloadString('https://chocolatey.org/install.ps1'))
```

The first line in the CMakeLists.txt file is the minimum required version. Mine is 3.7.1. Ideally, it would be best if you chose the lowest possible. We will go if this for now.

Then we tell CMake that we are configuring out project "lru". Or the name you prefer.

After, we put inside a list named "APP_FILES" all our files. Currently just main.cpp

We create a variable named "APP_NAME" and set its value.

And we tell that inside project "lru" we have an executable called whatever value is in the variable "APP_NAME" and contains all files inside the list "APP_FILES".

```CMake
cmake_minimum_required(VERSION 3.7.1)
project(lru)

list(APPEND APP_FILES main.cpp)

set (APP_NAME "lru.tests")
add_executable(${APP_NAME} ${APP_FILES})
```

Now we need to create a do-nothing c++ file name "main.cpp" like:

```c++
int main() { return 0; }
```
Now we can use CMake to create all the "Visual Studio" junk: solutions, project files etc...

```
> mkdir .build
> pushd .build
> cmake .. -G "Visual Studio 16 2019" 
> start lru.sln
> popd
```
This script will create a folder called ".build" and inside of it will be created the "Visual Studio" solution with the name you specified as the "CMake" project.

Now you can choose to compile the code from inside the 
"Visual Studio" with the default "Build Solution" option, or from the console using the command

```
> pushd .build
> cmake --build .
> popd
```
# CMake + Catch2

Now we want to integrate CMake build with Catch2. Catch2 is a very simple c++ unit test. To make this integration, we can download some files like this:

```
wget https://github.com/catchorg/Catch2/releases/download/v2.11.1/catch.hpp -OutFile catch.hpp
wget https://raw.githubusercontent.com/catchorg/Catch2/ac94bd05209d6dffb6aa7cb9750cfc45cbc4ac72/contrib/Catch.cmake -OutFile Catch.cmake
wget https://raw.githubusercontent.com/catchorg/Catch2/ac94bd05209d6dffb6aa7cb9750cfc45cbc4ac72/contrib/CatchAddTests.cmake -OutFile CatchAddTests.cmake
wget https://raw.githubusercontent.com/xunilrj/sandbox/master/sources/cpp/catch/ReferenceTests.runsettings -OutFile ReferenceTests.runsettings
```
Now we change our "CMakeLists.txt", as shown below. First, we include "CTest" and the "Catch.cmake" file. As the last step, we ask Catch to find out tests with "catch_discover_tests".

A nice touch to keep the "Visual Studio" solution organized is to enable "USE_FOLDERS".

```
cmake_minimum_required(VERSION 3.7.1)

project(lru)

include(CTest)
include(Catch.cmake)

set_property(GLOBAL PROPERTY USE_FOLDERS ON)

list(APPEND APP_FILES main.cpp)

set (APP_NAME "lru.tests")
add_executable(${APP_NAME} ${APP_FILES})

catch_discover_tests(${APP_NAME})
```

The last step is inside the "Visual Studio". Catch2 tests will be visible inside the "Test Explorer" if you install the Catch2 plugin. 

Test Adapter for Catch2  
https://marketplace.visualstudio.com/items?itemName=JohnnyHendriks.ext01

And if you select the "ReferenceTests.runsettings" you downloaded before with "Tests -> Configure Run Settings -> Select Solution Wide runsetting file".

Now we are ready to make our first test.

# First Unit Test

We can go back to our "main.cpp" file and create our first, and useless, unit test.

````
TEST_CASE("Cache.LRU.Must return -1 if not present", "[cache][lru][ok]")
{   
}
```` 
The test name allows a conventions that goes like "<NAMESPACE>.<CLASS>.<TESTNAME>". This helps you to organize your tests. The second parameter are tags. You can run tests by tags.

I am using the "ok" tag here because I want to separate the correctness tests from the performance tests.

We can now run out tests both from inside the "Visual Studio" when we are developing because it is more productive, but we can also run them from the command line. 

```
> pushd .build
> cmake .. -G "Visual Studio 16 2019" 
> cmake --build .
> CTest
> popd
```
Script that we will split in:

"build.ps1"
```
mkdir .build -EA SilentlyContinue | Out-Null
pushd .build
    cmake .. -G "Visual Studio 16 2019"
    cmake --build .
popd
```
and "test.ps1"
```
pushd .build
    ctest
popd
```

# First test - return when does not exist

Our first test is when we retrieve something that does not exist inside the LRUCache. The "leetcode" specification asks that we return -1. This not a very good interface for a Cache, but we will improve our LRUCache later.

```c++
class LRUCache
{
public:
    LRUCache(size_t capacity = 1000) {}

    void put(int key, int value) {}
    int get(int key) { return -1; }
};

TEST_CASE("Cache.LRU.Must return -1 if not present", "[cache][lru][ok]")
{
   auto cache = LRUCache{};
   REQUIRE(cache.get(1) == -1);
}
```
If we run out tests, we will see that this test is already working.

```
>..\test.ps1   
Test project D:/github/sandbox/sources/cpp/lru/.build
    Start 1: Cache.LRU.Must return -1 if not present
1/1 Test #1: Cache.LRU.Must return -1 if not present ...   Passed    0.02 sec

100% tests passed, 0 tests failed out of 1

Total Test time (real) =   0.04 sec
```