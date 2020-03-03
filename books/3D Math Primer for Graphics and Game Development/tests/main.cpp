#include <ostream>

#define CATCH_CONFIG_MAIN 
#include "catch.hpp"
#include "../src/math.h"
#include "properties.h"

#include <Windows.h>

//https://stackoverflow.com/questions/5195512/namespaces-and-operator-resolution
namespace autocheck {
	namespace detail {
		std::ostream& operator<< (std::ostream& out, const math::vec2& v)
		{
			return out << "[" << v.x << ";" << v.y << "]";
		}

		std::ostream& operator<< (std::ostream& out, const math::vec3& v)
		{
			return out << "[" << v.x << ";" << v.y << ";" << v.z << "]";
		}
	}
}

#include <autocheck/autocheck.hpp>

template <size_t D> void assert_zero_vector()
{
	/*auto v = math::zeros<D>();
	auto f = [](auto&& x) {return x == 0; };
	REQUIRE(all(v, f));*/
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
	
	//math::vec2 xx = v.xx(); REQUIRE(xx.x == 1);	REQUIRE(xx.y == 1);
	//math::vec2 yy = v.yy(); REQUIRE(yy.x == 2);	REQUIRE(yy.y == 2);
}

TEST_CASE("math.vec2.negate", "[ok]")
{
	auto v = math::vec2{ 1,2 };
	
	REQUIRE(-v == math::vec2{ -1, -2 });
	//REQUIRE(math::neg(v) == math::vec2{ -1, -2 });
}

TEST_CASE("math.vec2.multiplyByScalar", "[ok]")
{
	auto v = math::vec2{ 1,2 };
	
	auto nv2 = v * -1; REQUIRE(nv2.x == -1); REQUIRE(nv2.y == -2);
	auto nv3 = -1 * v; REQUIRE(nv3.x == -1); REQUIRE(nv3.y == -2);
	//auto nv4 = math::mul(v, -1); REQUIRE(nv4.x == -1); REQUIRE(nv4.y == -2);
	//auto nv5 = math::mul(-1, v); REQUIRE(nv5.x == -1); REQUIRE(nv5.y == -2);
	auto& nv1 = v *= -1; REQUIRE(nv1.x == -1); REQUIRE(nv1.y == -2);
}

TEST_CASE("math.vec2.sum", "[ok]")
{
	auto a = math::vec2{ 1,2 };
	auto b = math::vec2{ 3,5 };
	
	auto s = math::vec2{ 4,7 };

	REQUIRE((a + b) == s);
	REQUIRE((b + a) == s);
	//REQUIRE(math::sum(a, b) == s);
	//REQUIRE(math::sum(b, a) == s);

	REQUIRE((a += b) == s);
}

TEST_CASE("math.vec2.difference", "[ok]")
{
	auto a = math::vec2{ 1,2 };
	auto b = math::vec2{ 3,5 };

	auto ba = math::vec2{ 2,3 };

	REQUIRE((a - b) == -ba);
	REQUIRE((b - a) == ba);
	//REQUIRE(math::diff(a, b) == -ba);
	//REQUIRE(math::diff(b, a) == ba);

	REQUIRE((b -= a) == ba);
}

template <size_t D>
void vector_properties()
{
	using namespace math;

	auto anyfloat = any_float();
	auto anyvec = any_vec<D>();

	auto r = anyvec();

	is_abelian_group<PlusOperator, MinusOperator, MinusOperator>("vec2PlusMinusAbelianGroup", anyvec, math::zeros<D>());
	check_distributivity<TimesOperator, PlusOperator>("vec2PlusDistributivity", anyfloat, anyvec);

	check_commutativity<TimesOperator>("vec2DotProduct", anyvec);
	check_distributivity<TimesOperator, PlusOperator>("vec2DotDistributivity", anyvec, anyvec);

	//REQUIRE(a.norm2() + b.norm2() != (a + b).norm2()); // DOES NOT FORM TRIANGLE

	check("dotproduct is norm squared", anyvec, anyvec,
		[&](auto&& a, auto&& b)
		{
			//return  a.norm() + b.norm() >= (a + b).norm();
			return true;
		});
	check("dotproduct is norm squared", anyvec,
		[&](auto&& a)
		{
			return  a.norm2() == a * a;
		});
	check("dotproduct associativity", anyfloat, anyvec, anyvec,
		[&](auto&& f, auto&& a, auto&& b)
		{
			auto r1 = f * (a * b);
			auto r2 = (f * a) * b;
			auto r3 = a * (f * b);
			return  r1 == r2 && r1 == r3;
		});
}

TEST_CASE("math.vec2.algebraProperties", "[ok]")
{
	vector_properties<2>();
	vector_properties<3>();
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
