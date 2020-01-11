#define CATCH_CONFIG_MAIN 
#include "../catch/catch.hpp"

#include <unordered_map>
#include <list>
#include <optional>
#include <utility>

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
        auto [it, inserted] = items.try_emplace(key, value, lit);
        if (!inserted) {
            it->second.value = value;
            update_iterator(it->second, lit);
        } else {
            evict_if_necessary();
        }

        return it->second.value;
    }

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

        record(const TValue& v, lru_iterator i) : value{ v }, it{ i } {}
        record(TValue&& v, lru_iterator i) : value{ std::move(v) }, it{ i } {}
    };
    std::unordered_map<TKey, record> items;

    void evict_if_necessary()
    {
        if (items.size() > max_size)
        {
            auto to_remove = lru.front();
            lru.pop_front();
            items.erase(to_remove);
        }
    }

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


TEST_CASE("Cache.LRU.Must return -1 if not present", "[cache][lru][nok]")
{
   auto cache = LRUCache{};
   REQUIRE(cache.get(1) == -1);
}

TEST_CASE("Cache.LRU.Must get after put", "[cache][lru][ok]")
{
    auto cache = LRUCache{};
    cache.put(1, 1);
    REQUIRE(cache.get(1) == 1);
}

TEST_CASE("Cache.LRU.Must replace items", "[cache][lru][ok]")
{
    auto cache = LRUCache{};

    cache.put(1, 1);
    REQUIRE(cache.get(1) == 1);

    cache.put(1, 2);
    REQUIRE(cache.get(1) == 2);
}

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

#include <memory>

TEST_CASE("Cache.LRU.Must work with moveable type", "[cache][lru][ok]")
{
    using int_ptr = std::unique_ptr<int>;
    auto cache = Cache<int, int_ptr>{ 2 };

    auto ptr = std::make_unique<int>(1);
    cache.put(1, std::move(ptr));

    REQUIRE(ptr == nullptr);

    auto r = cache.get(1);
    REQUIRE(r.has_value());
    REQUIRE(**r.value() == 1);
}

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
