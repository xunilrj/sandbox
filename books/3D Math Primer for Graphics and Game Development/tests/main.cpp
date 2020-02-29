#define CATCH_CONFIG_MAIN 
#include "catch.hpp"
#include "../src/math.h"

//TODO change to safe_float

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

TEST_CASE("math.vec2.acessors", "[ok]")
{
	auto v = math::vec2{ 1,2 };
	REQUIRE(v.x == 1); REQUIRE(v.y == 2);
	REQUIRE(v[0] == 1); REQUIRE(v[1] == 2); //REQUIRE(v[2] == 2); //TODO: waiting compilation time error being possible
	
	math::vec2 xx = v.xx(); REQUIRE(xx.x == 1);	REQUIRE(xx.y == 1);
	math::vec2 yy = v.yy(); REQUIRE(yy.x == 2);	REQUIRE(yy.y == 2);
}

TEST_CASE("math.vec2.negate", "[ok]")
{
	auto v = math::vec2{ 1,2 };
	
	auto nv1 = -v; REQUIRE(nv1.x == -1); REQUIRE(nv1.y == -2);
	auto nv2 = math::neg(v); REQUIRE(nv2.x == -1); REQUIRE(nv2.y == -2);
}

TEST_CASE("math.vec2.multiplyByScalar", "[ok]")
{
	auto v = math::vec2{ 1,2 };
	
	auto nv2 = v * -1; REQUIRE(nv2.x == -1); REQUIRE(nv2.y == -2);
	auto nv3 = -1 * v; REQUIRE(nv3.x == -1); REQUIRE(nv3.y == -2);
	auto nv4 = math::mul(v, -1); REQUIRE(nv4.x == -1); REQUIRE(nv4.y == -2);
	auto nv5 = math::mul(-1, v); REQUIRE(nv5.x == -1); REQUIRE(nv5.y == -2);
	auto& nv1 = v *= -1; REQUIRE(nv1.x == -1); REQUIRE(nv1.y == -2);
}

TEST_CASE("math.vec2.sum", "[ok]")
{
	auto a = math::vec2{ 1,2 };
	auto b = math::vec2{ 3,5 };
	
	auto s = math::vec2{ 4,7 };

	REQUIRE((a + b) == s);
	REQUIRE((b + a) == s);
	REQUIRE(math::sum(a, b) == s);
	REQUIRE(math::sum(b, a) == s);

	REQUIRE((a += b) == s);
}

TEST_CASE("math.vec2.difference", "[ok]")
{
	auto a = math::vec2{ 1,2 };
	auto b = math::vec2{ 3,5 };

	auto ba = math::vec2{ 2,3 };

	REQUIRE((a - b) == -ba);
	REQUIRE((b - a) == ba);
	REQUIRE(math::diff(a, b) == -ba);
	REQUIRE(math::diff(b, a) == ba);

	REQUIRE((b -= a) == ba);
}

TEST_CASE("math.vec2.linearAlgebra", "[ok]")
{
	auto a = math::vec2{ 1,2 };
	auto b = math::vec2{ 3,4 };
	auto c = math::vec2{ 5,6 };

	REQUIRE(a + b == b + a);
	REQUIRE(a - b == a + (-b));
	REQUIRE((a + b) + c == a + (b + c));

	float s = 2;
	float t = 3;
	REQUIRE((s * t) * a == s * (t * a));
	REQUIRE(2 * (a + b) == (2 * a + 2 * b));
	REQUIRE(2 * (a - b) == (2 * a - 2 * b));

	REQUIRE(std::abs(s) * a.norm() == (s * a).norm());
	REQUIRE(a.norm() > 0);

	REQUIRE(a.norm2() + b.norm2() != (a + b).norm2()); // DOES NOT FORM TRIANGLE
	REQUIRE(a.norm() + b.norm() >= (a + b).norm());

	REQUIRE(a * b == b * a);
	REQUIRE(a.norm() == std::sqrt(a * a));
	REQUIRE(t * (a * b) == (t * a) * b);
	REQUIRE(t * (a * b) == a * (t * b));
	
	REQUIRE(a * (b + c) == (a * b) + (a * c));
}

TEST_CASE("math.vec3.linearAlgebra", "[ok]")
{
	auto a = math::vec3{ 1,2,3 };
	auto b = math::vec3{ 4,5,6 };
	auto c = math::vec3{ 7,8,9 };

	REQUIRE(a % a == math::vec3{ 0,0,0 });
	REQUIRE(a % b == -(b % a));
	REQUIRE(a % b == (-a % -b));

	float s = 2;
	float t = 3;

	REQUIRE(t * (a % b) == (t * a) % b);
	REQUIRE(t * (a % b) == a % (t * b));

	REQUIRE(a % (b + c) == ((a % b) + (a % c)));
}

TEST_CASE("math.vec2.norm2", "[ok]")
{
	auto a = math::vec2{ 1,1 };
	
	REQUIRE(a.norm() == sqrt(2.0f));
	REQUIRE(a.norm<2>() == sqrt(2.0f));
}

TEST_CASE("math.vec2.normalize", "[ok]")
{
	auto a = math::vec2{ 2,0 };

	REQUIRE(a.normalized() == math::vec2{ 1, 0});
	REQUIRE(a.normalized().normalized() == a.normalized()); //is idempotent
	REQUIRE(a.normalize() == math::vec2{ 1, 0 });
}

TEST_CASE("math.distance.distance", "[ok]")
{
	REQUIRE(math::dist(math::vec2{ 1,1 }, math::vec2{ 0,0 }) == std::sqrt(2.f));
}

TEST_CASE("math.vec2.dotproduct", "[ok]")
{
	auto a = math::vec2{ 1,2 };
	auto b = math::vec2{ 3,4 };

	REQUIRE((a * b) == 11);
}

TEST_CASE("math.vec3.crossproduct", "[ok]")
{
	auto a = math::vec3{ 1,0,0 };
	auto b = math::vec3{ 0,1,0 };

	REQUIRE((a % b) == math::vec3{ 0,0,1 });
	REQUIRE((b % a) == math::vec3{ 0,0,-1 });
}
