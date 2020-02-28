#define CATCH_CONFIG_MAIN 
#include "catch.hpp"
#include "../src/math.h"

template <size_t D> void assert_zero_vector()
{
	auto v = math::zeros<D>();
	auto f = [](auto&& x) {return x == 0; };
	REQUIRE(all(v, f));
}

TEST_CASE("math.vectors.zeros", "[ok]")
{
	assert_zero_vector<2>();
	assert_zero_vector<3>();
	assert_zero_vector<4>();
	assert_zero_vector<5>();
}

TEST_CASE("math.vectors.vec2", "[ok]")
{
	auto v = math::vec2{ 1,2 };
	REQUIRE(v.x == 1);
	REQUIRE(v.y == 2);
	REQUIRE(v[0] == 1);
	REQUIRE(v[1] == 2);
	//REQUIRE(v[2] == 2); // waiting compilation time error being possible
	
	math::vec2 c = v.xx.;
}