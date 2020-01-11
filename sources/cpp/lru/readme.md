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
TEST_CASE("Cache.LRU.Must return -1 if not present", "[cache][lru][nok]")
{   
}
```` 
The test name allows a conventions that goes like "<NAMESPACE>.<CLASS>.<TESTNAME>". This helps you to organize your tests. The second parameter are tags. You can run tests by tags.

I am using the "nok" tag here because I want to separate the correctness/exceptions tests from the performance tests. I use "okay" for correctness and "nok" for exception cases.

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

TEST_CASE("Cache.LRU.Must return -1 if not present", "[cache][lru][nok]")
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
# Second test put/get

Our second test here to enable getting an item that was put in the cache.

```c++
TEST_CASE("Cache.LRU.Must get after put", "[cache][lru][ok]")
{
    auto cache = LRUCache{};
    cache.put(1, 1);
    REQUIRE(cache.get(1) == 1);
}
```
This will be the last case that I will show the output of running the unit tests, just because now our test suite is failling.
```
>test.ps1
Test project D:/github/sandbox/sources/cpp/lru/.build
    Start 1: Cache.LRU.Must return -1 if not present
1/2 Test #1: Cache.LRU.Must return -1 if not present ...   Passed    0.02 sec
    Start 2: Cache.LRU.Must get after put
2/2 Test #2: Cache.LRU.Must get after put ..............***Failed    0.02 sec

50% tests passed, 1 tests failed out of 2

Total Test time (real) =   0.09 sec

The following tests FAILED:
          2 - Cache.LRU.Must get after put (Failed)
Errors while running CTest
``` 
It shows precisely which test failed, so this process alone allows us to develop, even outside "Visual Studio" if one prefers.

This test has nothing special, it is just mimicking a map container, so, for now, we are just going to use one. I will go for std::undordered_map because, at least for now, I do not care for sorted items inside the map.

So we are going to something like this:

```c++
#include <unordered_map>

class LRUCache
{
public:
    LRUCache(size_t capacity = 1000) {}

    void put(int key, int value)
    {
        items.emplace(key, value);
    }
    int get(int key)
    {
        auto it = items.find(key);
        if (it == items.end()) return -1;
        else return it->second;
    }
private:
    std::unordered_map<int, int> items;
};
``` 

First, we include the needed header. Always include the header that you need. A lot of "tutorials" on the internet and Youtube only import some headers, they do this because some headers include others. Never rely on that, because it will work on your compile/environment and break on others.

```c++
#include <unordered_map>
```

Then we define a private map inside our cache.

```c++
class LRUCache
{
public:
 ...
private:
    std::unordered_map<int, int> items;
};
```

The put is straightforward. Why "try_emplace" and not insert? See more [here](https://jguegant.github.io/blogs/tech/performing-try-emplace.html).

//TODO WE NEED C++ 17 from now on

```c++
 void put(int key, int value)
    {
        items.try_emplace(key, value);
    }
```

"get" is a little bit more complicated because we need to know if the "key" exists or not inside the "std::unordered_map".

```c++
 int get(int key)
    {
        auto it = items.find(key);
        if (it == items.end()) return -1;
        else return it->second;
    }
```
Now both our tests pass.

# Third Test - Replace

We must also be able to replace items using "put" with a different value. So let us create our test.

```c++
TEST_CASE("Cache.LRU.Must replace items", "[cache][lru][ok]")
{
    auto cache = LRUCache{};

    cache.put(1, 1);
    REQUIRE(cache.get(1) == 1);

    cache.put(1, 2);
    REQUIRE(cache.get(1) == 2);
}
```
This test, off course, fail. The reason why it fails if stated in the documentation:

```
Inserts a new element into the container constructed in-place with the given args if there is no element with the key in the container.
```
https://en.cppreference.com/w/cpp/container/unordered_map/try_emplace

"try_emplace" does nothing when the key already exists, but, luckily, return if the item was inserted or not, allowing us to update the value when necessary.

```c++
void put(int key, int value)
    {
        auto [it, inserted] = items.try_emplace(key, value);
        if (!inserted) {
            it->second = value;
        }
    }
```

Okay. Now our third test is green.

# Forth test - eviction policy

This test is pretty straightforward, and it is the core of the class. We need the evict the least recently used item when we insert more than the capacity, specified through the constructor.

```c++
TEST_CASE("Cache.LRU.Must evict when capacity is full", "[cache][lru][ok]")
{
    auto cache = LRUCache{2}; // 2 items max

    cache.put(1, 1);
    cache.put(2, 2);
    cache.put(3, 3);

    REQUIRE(cache.get(1) == -1); // 1 was evicted
    REQUIRE(cache.get(2) == 2);
    REQUIRE(cache.get(3) == 3);
}
```` 

Now we have to stop a little and think. 

Knowing THE least recently used is not enough, because after evicting this item, we will need to evict another in case of another "put". Otherwise, a simple "lruKey" updated every time with the key when "put" or "get" is called would suffice.

So we need a collection here. But which one?

We have a limited amount of items, so std::array and/or std::vector are good candidates.

My strategy here is, every time a "key" is used, we will move this key to the end of the collection. So we are planning: "remove from the middle", and "insert at the end".

Every time I need to evict an item, I will just look at the first item. So we are also planning on "removing from the start".

std::array does not have the concept of "inserting" or "removing". So let us ignore it.

std::vector has "emplace_back" to insert items at the end. Promising. But it does not have fast methods for "remove from the middle" and "remove from the start". Off course we can do this using "insert" and "erase", but the problem is that we need to move all the memory around.

As the documentation says:

```
Complexity
Linear: the number of calls to the destructor of T is the same as the number of elements erased, the assignment operator of T is called the number of times equal to the number of elements in the vector after the erased elements
```
https://en.cppreference.com/w/cpp/container/vector/erase

So we need to find another collection. We have two lists: std::foward_list and std::list. They are implemented as linked-lists and doubly-linked-lists, respectively.

Both have "pop_front" to remove from the beginning as O(1) methods.

```
Complexity: Constant.
```
https://en.cppreference.com/w/cpp/container/forward_list/pop_front

```
Complexity: Constant.
```
https://en.cppreference.com/w/cpp/container/list/pop_front

A good start, but std::foward_list does not have anything to "insert at the end". So we will ignore it here.

std::list have.

```
Complexity: Constant.
```
https://en.cppreference.com/w/cpp/container/list/push_back

And luckly, std::list also have "remove from the middle" and with constant complexity.

```
Complexity: Constant.
```
https://en.cppreference.com/w/cpp/container/list/erase

So I think we have a winner here. We will use std::list. The questions boil down now to how we are going to use this. We can start as simple as possible. We will store just the keys.

We start including the "std::list" header.

```c++
#include <list>
```
And we define the "lru" field inside our class.

```c++
class LRUCache
{
public:
 ...
private:
    std::unordered_map<int, int> items;
    std::list<int> lru;
};
```

Now we create a private method "update_iterator" that updates the "record" struct and remove the key from the std::list.

But we have a problem. How do we find the key inside the std::list, it does not have O(1) search by value. And linearly searching the key inside the list is out of the question.

So we need a map, that allows O(1) in search by key. Our luck is that we already have one. We can extend the value of our std::unordered_map to tell us where the key is inside the std::list.

We all know that when we want to "point" to something inside containers we need to use iterators. In this case we need std::list<int>::iterator. To avoid typing this much we will use a trick.

```c++
class LRUCache
{
public:
...
private:
    std::list<int> lru;
    struct record
    {
        int value;
        decltype(lru)::iterator it;
    };
    std::unordered_map<int, record> items;
};
```

"decltype" allows me to reference the type of a variable. In this case, even if we change the type of "lru" we just need an internal type named "iterator".
I also create an internal and private type record because it gives me more evident names than std::tuple.

A fair warning before we continue. We should be careful when storing pointers, references and iterators. In this case, we are assuming that the function that we are gonna use on the std::list does not invalidate the iterators. Let us check.

```
References and iterators to the erased elements are invalidated. Other references and iterators are not affected.
```
https://en.cppreference.com/w/cpp/container/list/erase

```
No iterators or references are invalidated.
```
https://en.cppreference.com/w/cpp/container/list/push_back

```
References and iterators to the erased element are invalidated.
```
https://en.cppreference.com/w/cpp/container/list/pop_front

So we are okay. Iterators to the elements continue to be valid after these operations. This does not necessarily happen with other collections. If you want/need this, you probably need to develop something custom.

Given that we are safe, we can fix the code we broke.

```c++
    LRUCache(size_t capacity = 1000) {}

    void put(int key, int value)
    {
        auto [it, inserted] = items.try_emplace(key, record{ value, lru.end() /*mock value need fix*/ });
        if (!inserted) {
            it->second.value = value;
        }
    }

    int get(int key)
    {
        auto it = items.find(key);
        if (it == items.end()) return -1;
        else return it->second.value;
    }
```

Okay. Now our code is compiling again. Our old tests are also passing. The problem is that we are now saving a fake iterator in the dictionary. We need to fix this now.

Our strategy is: if we are "putting" the key for the first time it does not exist in the std::unordered_map nor in the list. So if we inserted in the std::unordered_map we need to add it to the std::list.

We have a chicken-egg kind of problem here. We use "try_emplace" to see if we need to insert to the list, but we already passed the list iterator to the "try_emplace". 

Our luck is that in this case we will move the key to end of the list. If is the first time we will just insert at the end. If we are updating a key we will also remove from the middle of the std::list.

So we are going to always insert the key at the end of the std::list. The problem is that the only method that "insert" and return the iterator to the inserted item is the "emplace" method.

Before let us finish the "update_iterator" method:

```c++
void update_iterator(record& r, decltype(lru)::iterator& it)
    {
        lru.erase(r.it);
        r.it = it;
    }
```

and now we can fix our "put" and "get".

```
  void put(int key, int value)
    {
        auto lit = lru.emplace(lru.end(), key);
        auto [it, inserted] = items.try_emplace(key, record{ value, lit });
        if (!inserted) {
            it->second.value = value;
            update_iterator(it->second, lit);
        }
    }

    int get(int key)
    {
        auto it = items.find(key);
        if (it == items.end())
        {
            return -1;
        }
        else
        {
            auto lit = lru.emplace(lru.end(), key);
            update_iterator(it->second, lit);

            return it->second.value;
        }
    }
```

Okay. Our code compiles, but the test still fails. But now we are ready to implement the policy. First, we need to save the capacity specified.

```c++
class LRUCache
{
public:
    LRUCache(size_t capacity = 1000) : max_size{ capacity } {}
...
private:
    size_t max_size;
...
}
```

Our last step is now to check if we need to remove one item when we insert a new one in the std::unordered_map.

```c++
 void put(int key, int value)
    {
        auto lit = lru.emplace(lru.end(), key);
        auto [it, inserted] = items.try_emplace(key, record{ value, lit });
        if (!inserted) {
            it->second.value = value;
            update_iterator(it->second, lit);
        } else {
            if (items.size() > max_size)
            {
                auto to_remove = lru.front();
                lru.pop_front();
                items.erase(to_remove);
            }
        }
    }
```
Ans now we pass out last test.

This implementation is already passing all leetcode tests, but there is a small problem. Can you spot it?

```c++
class LRUCache
{
public:
    LRUCache(size_t capacity = 1000) : max_size{ capacity } {}

    void put(int key, int value)
    {
        auto lit = lru.emplace(lru.end(), key);
        auto [it, inserted] = items.try_emplace(key, record{ value, lit });
        if (!inserted) {
            it->second.value = value;
            update_iterator(it->second, lit);
        } else {
            if (items.size() > max_size)
            {
                auto to_remove = lru.front();
                lru.pop_front();
                items.erase(to_remove);
            }
        }
    }

    int get(int key)
    {
        auto it = items.find(key);
        if (it == items.end())
        {
            return -1;
        }
        else
        {
            auto lit = lru.emplace(lru.end(), key);
            update_iterator(it->second, lit);

            return it->second.value;
        }
    }
private:
    size_t max_size;
    std::list<int> lru;
    struct record
    {
        int value;
        decltype(lru)::iterator it;
    };
    std::unordered_map<int, record> items;

    void update_iterator(record& r, decltype(lru)::iterator& it)
    {
        lru.erase(r.it);
        r.it = it;
    }
};
```

Yes! We insert before we test the capacity. So for a very short time, we have our std::unordered_map with more items than allowed.

If this is a problem or not, depends on the specification, off source.

Another problem is that this is very hard to test. Some times these specifications are called "invariances". This a rule that should always be valid: "items.size() <= max_size".

Unfortunately, in c++, there is no easy way to write this invariance.

# Refactor - improving the code

Now that we have out test suite, we can freely improve the code. The first thing we are going to do is keep the LRUCache class and move all the code to another class: Cache. So we keep the leetcode interface to submit the code, but we develop a more realistic LRUCache.

The first step is, off course, change the "key" and "value" to templates.

Our second modification is to decide how to pass the "keys" and "values". "int" is so simple that it does not make sense. But when the "key" and "value" types are more complex it will make a difference. Our first immediate choice is to accept "const T&".

A third modification is to return the reference to the inserted item in "put". This is a dangerous one. If this reference outlives the cache, we will have a dangling pointer. Be aware.

Our fourth modification is that the "get" returns an std::optional. It better models the fact that sometimes we do not have a result to return.

We lost the reference to LRU in the name, but we will recover this later.

```c++
#include <optional>
template <typename TKey, typename TValue>
class Cache
{
public:
    using key_type = TKey;
    using mapped_type = TValue;

    Cache(size_t capacity = 1000) : max_size{ capacity } {}

    TValue& put(const TKey& key, const TValue& value)
    {
        auto lit = lru.emplace(lru.end(), key);
        auto [it, inserted] = items.try_emplace(key, record{ value, lit });
        if (!inserted) {
            it->second.value = value;
            update_iterator(it->second, lit);
        } else {
            if (items.size() > max_size)
            {
                auto to_remove = lru.front();
                lru.pop_front();
                items.erase(to_remove);
            }
        }

        return it->second.value;
    }

     std::optional<TValue*> get(const TKey& key)
    {
        auto it = items.find(key);
        if (it == items.end()) return {};
        else
        {
            auto lit = lru.emplace(lru.end(), key);
            update_iterator(it->second, lit);

            return &it->second.value;
        }
    }
private:
    size_t max_size;
    std::list<TKey> lru;
    using lru_iterator = typename decltype(lru)::iterator;
    struct record
    {
        TValue value;
        lru_iterator it;
    };
    std::unordered_map<TKey, record> items;

    void update_iterator(record& r, lru_iterator& it)
    {
        lru.erase(r.it);
        r.it = it;
    }
};

class LRUCache
{
public:
    LRUCache(size_t capacity = 1000) : cache{ capacity } {}
    void put(int key, int value) { cache.put(key, value); }
    int get(int key) {
        auto r = cache.get(key);
        if (r.has_value()) return *r.value();
        else return -1;
    }
private:
    Cache<int, int> cache;
};
```

# Second Refactor - Moveable types

Before releasing this code to production, I think that we must at least analyze how it works with moveable types.

Move semantic is a complex topic on itself, so at least for now, let us just acknowledge that moveable types exist and that std::unique_ptr is one of them. We can see this at the documentation.

```
The class satisfies the requirements of MoveConstructible and MoveAssignable, but not the requirements of either CopyConstructible or CopyAssignable.
```
https://en.cppreference.com/w/cpp/memory/unique_ptr

So std::unique_ptr is moveable but not copyable.

So our first modification is on the "record" struct. That struct owns the data. We have been working with a trivially copyable type like "int". So no problem. Just copy the value to the "record", and that is it. With moveable type is different. We need to "move" the value to inside the "struct". We do this with:

```c++
 struct record
    {
        TValue value;
        lru_iterator it;

        record(const TValue& v, lru_iterator i) : value{ v }, it{ i } {}
        record(TValue&& v, lru_iterator i) : value{ std::move(v) }, it{ i } {}
    };
```

This means something like: if we try to create the "struct" with a copyable type, call the constructor that will just copy everything; if, on the other side, we try to create the "struct" with a moveable type, call the constructor that will move the value.

It is important to note that the r-value is NOT const. Because when we move a value, the "source" is nulled.

Now we need the same options for the "put" method.

```c++

    TValue& put(const TKey& key, const TValue& value)
    {
...
    }

    TValue& put(const TKey& key, TValue&& value)
    {
...
}
```

These methods are pretty much the same. The only difference is that we always need to move the "value" in the "moveable" version.

```c++
    TValue& put(const TKey& key, TValue&& value)
    {
        auto lit = lru.emplace(lru.end(), key);        
        auto [it, inserted] = items.try_emplace(key, record{ std::move(value), lit });
        if (!inserted) {
            it->second.value = std::move(value);
            update_iterator(it->second, lit);
        }
        else {
            evict_if_necessary();
        }

        return it->second.value;
    }
```

At this point, it is essential to remember why "try_emplace".

```
Unlike insert or emplace, these functions do not move from rvalue arguments if the insertion does not happen
```
https://en.cppreference.com/w/cpp/container/unordered_map/try_emplace

If this was not the case, our code would be wrong because we have two moves from the same variable. After finishing this section, you can try changing from "try_emplace" to "emplace" and seeing one of the tests not working.

But!

If we analyze our code, look at what we have done. We construct a "record" before calling the "try_emplace". And this constructor actually moves the value. So we ARE moving twice from the same variable, and this is a recipe for disaster.

We can see this using this test.

```c++
TEST_CASE("Cache.LRU.Must replace with moveable type", "[cache][lru][ok]")
{
    using int_ptr = std::unique_ptr<int>;
    auto cache = Cache<int, int_ptr>{ 2 };

    auto ptr1 = std::make_unique<int>(1);
    cache.put(1, std::move(ptr1));

    auto ptr2 = std::make_unique<int>(2);
    cache.put(1, std::move(ptr2));

    auto r = cache.get(1);
    REQUIRE(r.has_value());
    REQUIRE(**r.value() == 2);
}
```
Now this test will fail. Exactly because of our "double-move". What is the solution?

Well.. We were creating the "record" struct before calling "try_emplace" for a reason. It was the unique available way of using "try_emplace" for us at that moment.

Now that we have our two constructors, we can simplify the code and make it work. Double bonus!

Let us first change the "okay" "put" method. See what we did here? We no longer build a "record", but we pass the parameters of the constructor to the "try_emplace".

The problem is that the code that actually builds the object uses the constructor syntax (using ()) and not the aggregate construction (using {}).

```
2) Calls ::new((void *)p) U(std::forward<Args>(args)...)
```
https://en.cppreference.com/w/cpp/memory/allocator/construct

But now that our struct has constructors we can use this option.

```c++
TValue& put(const TKey& key, const TValue& value)
    {
        auto lit = lru.emplace(lru.end(), key);
        auto [it, inserted] = items.try_emplace(key, value, lit);
        if (!inserted) {
            it->second.value = value;
            update_iterator(it->second, lit);
        } else {
            evict_if_necessary();
        }
        return it->second.value;
    }
```

And the event better part is that now we can fix the "move" "put".

```c++
   TValue& put(const TKey& key, TValue&& value)
    {
        auto lit = lru.emplace(lru.end(), key);        
        auto [it, inserted] = items.try_emplace(key, std::forward<TValue>(value), lit);
        if (!inserted) {
            it->second.value = std::move(value);
            update_iterator(it->second, lit);
        }
        else {
            evict_if_necessary();
        }
        return it->second.value;
    }
```
We need the "std::foward" in this case because otherwise the compiler would try to call the "const TValue&" constructor. Why? We can see the explanation and an example in the documentation.

```
rvalue reference variables are lvalues when used in expressions
```
https://en.cppreference.com/w/cpp/language/reference

So when you use the "TValue&&" in the "try_emplace" it is treated as a l-value, so the compiler calls the l-value constructor. That is why you need the std::foward because it preserves the "r-value-ness" of the parameter. This is called "perfect fowarding."

Now all our tests are passing.