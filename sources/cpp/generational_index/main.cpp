#define CATCH_CONFIG_MAIN 
#include "../catch/catch.hpp"

#include <vector>
#include <tuple>

template <typename T>
struct Maybe
{
    Maybe(bool hasValue, T v) : has_value{ hasValue }, value{ v }
    {
    }

    operator bool() const { return has_value; }
    T& operator *() const { return &value; }

private:
    bool has_value;
    T value;
};

struct GI
{    
    uint32_t generation;
    size_t index;
};

template <typename T>
struct GIItem
{
    uint32_t generation;
    bool has_value;
    T value;
};

template <typename T>
class GenerationVector
{
public:
    GenerationVector() : next{ 0 }
    {
    }

    GI push_back(const T& value)
    {
        auto itemsSize = items.size();
        while ((next < itemsSize) && items[next].has_value) ++next;

        if (next >= itemsSize)
        {
            next = itemsSize;
            items.push_back({ 0, false, {} });
        }

        auto& item = items[next];
        ++item.generation;
        item.has_value = true;
        item.value = value;

        return { item.generation, next };
    }

    void erase(GI& i)
    {
        auto itemsSize = items.size();
        if (i.index >= itemsSize)
        {
            return;
        }

        auto& item = items[i.index];
        ++item.generation;
        item.has_value = false;

        next = i.index;
    }

    Maybe<T*> operator[] (GI& i)
    {
        if (i.index >= items.size()) 
            return { false, nullptr };

        auto& item = items[i.index];
        if(!item.has_value || item.generation != i.generation)
            return { false, nullptr };

        return { true, &item.value };
    }

    Maybe<std::tuple<GI, T*>> operator[] (size_t i)
    {
        if (i >= items.size())
            return { false, std::make_tuple<GI,T*>({}, nullptr) };

        auto& item = items[i];
        if (item.has_value)
            return { false, std::make_tuple<GI,T*>({}, nullptr) };

        return { true, std::make_tuple<GI,T*>({item.generation, i}, &item.value) };
    }
private:
    std::vector<GIItem<T>> items;
    size_t next;
};


struct PlayerPosition
{
    float x;
    float y;
    float z;
};


template <typename T>
class IsSomeMatcher : public Catch::MatcherBase<Maybe<T>>
{
public:
    bool match(Maybe<T> const& m) const override { return m; }
    virtual std::string describe() const override {
        std::ostringstream ss;
        ss << "must have value";
        return ss.str();
    }
};

template <typename T>
class IsNoneMatcher : public Catch::MatcherBase<Maybe<T>>
{
public:
    bool match(Maybe<T> const& m) const override { return !m; }
    virtual std::string describe() const override {
        std::ostringstream ss;
        ss << "must be empty";
        return ss.str();
    }
};

template <typename T> IsSomeMatcher<T> isSome(Maybe<T> m) { return {}; }
template <typename T> IsNoneMatcher<T> isNone(Maybe<T> m) { return {}; }

TEST_CASE("Fake.Test.Will Pass", "[ok]")
{
    auto vec = GenerationVector<PlayerPosition>{};
    auto i1 = vec.push_back({ 1,0,0 });
    vec.push_back({ 2,0,0 });
    vec.push_back({ 3,0,0 });

    {
        auto x = vec[i1];
        REQUIRE_THAT(x, isSome(x));
    }

    {
        vec.erase(i1);
        auto x = vec[i1];
        REQUIRE_THAT(x, isNone(x));
    }

    {
        i1 = vec.push_back({ 1,0,0 });
        auto x = vec[i1];
        REQUIRE_THAT(x, isSome(x));
    }

    {
        vec.erase(i1);
        vec.push_back({ 1,0,0 });
        auto x = vec[i1];
        REQUIRE_THAT(x, isNone(x));
    }

    {
        auto i2 = vec.push_back({ 1,0,0 });
        auto x1 = vec[i2.index];
        REQUIRE_THAT(x1, isNone(x1));

        vec.erase(i2);
        auto x2 = vec[i2.index];
        REQUIRE_THAT(x2, isSome(x2));
    }
}