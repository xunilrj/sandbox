#define CATCH_CONFIG_MAIN 
#include "../catch/catch.hpp"

//class LRUCache
//{
//public:
//    LRUCache(size_t capacity = 1000) {}
//
//    void put(int key, int value) {}
//    int get(int key) { return -1; }
//};


TEST_CASE("Cache.LRU.Must return -1 if not present", "[cache][lru][ok]")
{
   /* auto cache = LRUCache{};
    REQUIRE(cache.get(1) == -1);*/
}
//
//TEST_CASE("Cache.LRU.Must get after put", "[cache][lru][ok  ]")
//{
//    auto cache = LRUCache{ 2 };
//    cache.put(1, 1);
//    REQUIRE(cache.get(1) == 1);
//}
