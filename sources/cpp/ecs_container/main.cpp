#define CATCH_CONFIG_MAIN 
#include "../catch/catch.hpp"

#include <array>
#include <list>
#include <unordered_map>
#include <algorithm>
#include <iostream>
#include <numeric>
#include <chrono>



template <typename T>
struct record
{
	uint64_t id;
	bool has_value;
	T value;
};

template <typename TValue>
struct Bucket
{
	size_t bucket;
	std::vector<record<TValue>>* ptr;
};

template <typename TValue>
class col_iterator
{
public:
	col_iterator(std::vector<Bucket<TValue>>& veclist)
	{
		ll = veclist.data();
		lllast = ll + veclist.size();

		it = ll->ptr->data();
		itlast = ll->ptr->data() + ll->ptr->size();
	}

	uint64_t id() const { return it->id; }

	void to(uint64_t id)
	{
		while (!atEnd() && (it->id < id))
		{
			++it;
		}
	}

	bool atEnd() const { return ll > lllast; }

	//bool operator == (const col_iterator<TValue>& it) const
	//{
	//	return (this->end == it.end) && (this->it == it.it);
	//}

	//bool operator != (const col_iterator<TValue>& it) const
	//{
	//	return (this->end == it.end) && (this->it != it.it);
	//}

	TValue& operator* () const { return it->value; }
	col_iterator<TValue>& operator ++ ()
	{
		++it;
		if (it <= itlast) { return *this; }
		else
		{
			++ll;
			if (ll <= lllast)
			{
				it = ll->ptr->data();
				_mm_prefetch((const char*)(const void*)(it), _MM_HINT_T0);
				itlast = it + ll->ptr->size();
			}
		}
		return *this;
	}
private:
	Bucket<TValue>* ll;
	Bucket<TValue>* lllast;
	record<TValue>* it;
	record<TValue>* itlast;
};

template <typename TValue>
class col
{
public:
	col() : size{ 100 }
	{
	}

	void emplace(uint64_t id, const TValue& v)
	{
		auto& vec = getOrAdd(id / size);

		auto it = std::upper_bound(vec.begin(), vec.end(), id,
		[](auto&& id, auto&& b) {
			return id == b.id;
		});
		vec.insert(it, { id, true, v });
	}

	col_iterator<TValue> begin() { return { ll }; }
	col_iterator<TValue> end() { return { ll }; }

	TValue* at(uint64_t id)
	{
		auto& vec = getOrAdd(id / size);
		auto it = std::binary_search(vec.begin(), vec.end(), [&](auto&& x) {
			return x.id == id;
		});
		if (it == vec.end()) return nullptr;
		if (!it->has_value) return nullptr;
		else return &it->value;
	}
private:
	using vec_type = std::vector<record<TValue>>;

	uint16_t size;
	std::unordered_map<uint64_t, vec_type*> vectors;
	std::vector<Bucket<TValue>> ll;

	vec_type& getOrAdd(size_t bucket)
	{
		auto it = vectors.find(bucket);
		if (it != vectors.end()) { return *it->second; }
		else {
			vec_type* newvec = new vec_type();
			newvec->reserve(size);
			vectors.insert(std::make_pair(bucket, newvec));

			auto it = std::upper_bound(ll.begin(), ll.end(), bucket,
				[](auto&& bucket, auto&& b) {
					return bucket == b.bucket;
				});
			ll.insert(it, { bucket, newvec });

			return *newvec;
		}
	}
};

template <typename... TArgs, std::size_t... Id>
bool atEnd(std::tuple<TArgs...>& begins, std::index_sequence<Id...> id)
{
	return (... || (std::get<Id>(begins).atEnd()));
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

	while (true)
	{
		auto someAtEnd = atEnd(its, std::index_sequence_for<TArgs...>{});
		if (someAtEnd) break;

		auto advances = advance(its, std::index_sequence_for<TArgs...>{});
		if (advances > 0) continue;

		call(f, its, std::index_sequence_for<TArgs...>{});

		++std::get<0>(its);
	}
}

TEST_CASE("Fake.Test.Will Pass", "[ok]")
{
	/*auto col1 = col<int>{};
	auto col2 = col<int>{};

	col1.emplace(1, 0);
	col1.emplace(11, 1);
	col1.emplace(31, 2);

	col2.emplace(1, 3);
	col2.emplace(21, 4);	
	col2.emplace(31, 5);

	auto it = col1.begin();
	++it;
	++it;
	++it;
	++it;


	join([](auto&& a, auto&& b) {
		std::cout << a << b;
	}, col1, col2);

	auto v1 = col1.at(1);
	auto v2 = col1.at(31);
	auto v3 = col1.at(11);
	auto v4 = col1.at(21);*/
	
	// PERFORMANCE
	////////////////////////////////////VECTOR
	std::vector<int> v(1000);
	std::iota(v.begin(), v.end(), 1);

	std::chrono::steady_clock::time_point begin = std::chrono::steady_clock::now();

	int times = 100000;
	uint64_t agg = 0;
	while (times > 0) {
		for (auto x : v)
			agg += x;
		--times;
	}
	
	std::chrono::steady_clock::time_point end = std::chrono::steady_clock::now();
	auto time1 = std::chrono::duration_cast<std::chrono::milliseconds>(end - begin).count();
	std::cout << "vector " << agg << " " << time1 << std::endl;;

	////////////////////////////////////////DEQUE
	std::deque<int> d(1000);
	std::iota(d.begin(), d.end(), 1);

	begin = std::chrono::steady_clock::now();

	times = 100000;
	agg = 0;
	while (times > 0) {
		for (auto x : d)
			agg += x;
		--times;
	}

	end = std::chrono::steady_clock::now();
	auto time2 = std::chrono::duration_cast<std::chrono::milliseconds>(end - begin).count();
	std::cout << "deque " << agg << " " << time2 << std::endl;;

	////////////////////////////////////ECSCONTAINER

	auto col3 = col<int>{};
	times = 1000;
	while (times > 0) {
		col3.emplace(times, times);
		--times;
	}

	begin = std::chrono::steady_clock::now();

	times = 100000;
	agg = 0;
	while (times > 0) {
		auto it = col3.begin();
		while (!it.atEnd())
		{
			agg += *it;
			++it;
		}
		--times;
	}

	end = std::chrono::steady_clock::now();
	auto time3 = std::chrono::duration_cast<std::chrono::milliseconds>(end - begin).count();
	std::cout << "ecs " << agg << " " << time3 << std::endl;

	std::cout << "deque/vector " << (float)time2 / time1 << " " << time2 << " " << time1 << std::endl;
	std::cout << "ecs/vector " << (float)time3 / time1 << " " << time3 << " " << time1 << std::endl;
}