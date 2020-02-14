#define CATCH_CONFIG_MAIN 
#include "../catch/catch.hpp"

#include <array>
#include <list>
#include <unordered_map>
#include <algorithm>
#include <iostream>

template <typename T>
struct record
{
	uint64_t id;
	T value;
};

template <typename T>
class col_iterator
{
public:
	col_iterator(std::vector<record<T>>& v, bool begin) : items{ v }, it { v.begin() }
	{
		if (begin) it = v.begin();
		else it = v.end();
	}

	uint64_t id() const { return it->id; }

	void to(uint64_t id)
	{
		while ((it != items.end()) && (it->id < id))
		{
			++it;
		}
	}

	bool operator == (const col_iterator<T>& it) const { return this->it == it.it; }
	bool operator != (const col_iterator<T>& it) const { return this->it != it.it; }

	col_iterator<T>& operator ++ () { ++it;	return *this; }
	T operator* () const { return it->value; }
private:
	std::vector<record<T>>& items;
	typename std::vector<record<T>>::iterator it;
};

template <typename TValue, size_t SIZE>
class col
{
public:
	void emplace(uint64_t id, TValue v)
	{
		auto i = std::lower_bound(items.begin(), items.end(), id, [&](auto&& a, auto&&b) {
			return a.id < id;
		});
		if (i == items.end() || (id != i->id))
			items.emplace(i, record<TValue>{ id, v });
	}

	col_iterator<TValue> begin()
	{
		return { items, true };
	}

	col_iterator<TValue> end()
	{
		return { items, false };
	}

	TValue* at(size_t id)
	{
		auto it = ids.find(id);
		if (it == ids.end()) return nullptr;
		else
		{
			auto i = *it;
			if (i >= items.size()) return searchAndUpdate(it, id);

			auto& item = items[i];
			if (item.id == id) return &item.value;
			else return searchAndUpdate(it, id);
		}
	}
private:
	TValue* searchAndUpdate(typename std::unordered_map<size_t, size_t>::iterator it, size_t id)
	{
		auto iit = std::find(items.begin(), items.end(), [=](auto&& item) { return item.id == id});
		if (iit == items.end())
		{
			ids.erase(it);
			return nullptr;
		}
		else
		{
			*it = std::distance(iit, items.begin());
			return *iit.value;
		}
	}
	std::unordered_map<size_t, size_t> ids;
	std::vector<record<TValue>> items;
};

template <typename... TArgs, std::size_t... Id>
bool atEnd(std::tuple<TArgs...>& begins, std::tuple<TArgs...>& ends, std::index_sequence<Id...> id)
{
	return (... || (std::get<Id>(begins) == std::get<Id>(ends)));
}

template <typename F>
struct FoldStruct
{
	F function;
	FoldStruct(F f) : function(f)
	{
	}

	template<size_t I, typename... TArgs>
	auto get (std::tuple<TArgs...>& t)
	{
		return std::make_tuple(I, &std::get<I>(t));
	}

	template <typename T>
	FoldStruct<F>& operator && (std::tuple<size_t, T*> t)
	{
		function(std::get<0>(t), *std::get<1>(t));
		return *this;
	}
};
template <typename... TArgs, std::size_t... Id>
auto advance(std::tuple<TArgs...>& begins, std::index_sequence<Id...> id)
{
	auto min = std::numeric_limits<uint64_t>::max(); size_t min_index = 0;
	auto max = std::numeric_limits<uint64_t>::min(); size_t max_index = 0;
	auto minmax = FoldStruct{ [&](auto i, auto&& v) { 
		auto id = v.id();
		if (id < min) { min = id; min_index = i; }
		if (id > max) { max = id; max_index = i; }
	}};
	(minmax && ... && minmax.get<Id>(begins));

	if (min == max) return 0;

	auto advances = 0;
	auto advance = FoldStruct{ [&](auto i, auto&& v) {
		if (i != max_index)
		{
			v.to(max);
			++advances;
		}
	}};
	(advance && ... && advance.get<Id>(begins));
	return advances;
}

template <typename F, typename... TArgs, std::size_t... Id>
void call(F f, std::tuple<TArgs...> t, std::index_sequence<Id...> id)
{
	f(*std::get<Id>(t)...);
}

template <typename F, typename... TArgs>
void join(F f, TArgs... args)
{
	auto its = std::make_tuple(args.begin()...);
	auto ends = std::make_tuple(args.end()...);

	while (true)
	{
		auto someAtEnd = atEnd(its, ends, std::index_sequence_for<TArgs...>{});
		if (someAtEnd) break;

		auto advances = advance(its, std::index_sequence_for<TArgs...>{});
		if (advances > 0) continue;

		call(f, its, std::index_sequence_for<TArgs...>{});

		++std::get<0>(its);
	}
}

TEST_CASE("Fake.Test.Will Pass", "[ok]")
{
	auto col1 = col<int, 10>{};
	auto col2 = col<int, 10>{};

	col1.emplace(1, 0);
	col1.emplace(11, 1);
	col1.emplace(31, 2);

	col2.emplace(1, 3);
	col2.emplace(21, 4);	
	col2.emplace(31, 5);


	join([](auto&& a, auto&& b) {
		std::cout << a << b;
	}, col1, col2);

	auto it1 = col1.begin();
	auto it2 = col2.begin();

	auto end1 = col1.end();
	auto end2 = col2.end();

	

	// JOIN both cols
	// no need to keep entities indices
	// allow sparse components
	while (true)
	{
		if (it1 == end1) break;
		if (it2 == end2) break;
		if (it1.id() < it2.id()) { it1.to(it2.id()); continue; }
		if (it2.id() < it1.id()) { it2.to(it1.id()); continue; }

		auto v1 = *it1;
		auto v2 = *it2;

		++it2;
	}
}