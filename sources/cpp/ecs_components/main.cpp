#define CATCH_CONFIG_MAIN 
#include "../catch/catch.hpp"

struct Maybe
{
	bool hasData;
	void* data;

	template <typename T>
	T& as()
	{
		return (T&)*(T*)data;
	}
};

struct GenerationIndex
{
};

template <typename T>
class VectorStore
{
	using iterator = std::vector<T>::iterator;
private:
	std::vector<T> data;
};



template <typename TStore>
class ComponentContainer
{
	typename TStore::iterator begin() { return store.begin(); }
	typename TStore::iterator end() { return store.end(); }
	Maybe operator[] (GenerationIndex& i) { return store[i]; }
private:
	TStore store;
};

class Components
{

};


TEST_CASE("Fake.Test.Will Pass", "[ok]")
{
	auto components = Components{};
	components.add<VectorStore>()
}