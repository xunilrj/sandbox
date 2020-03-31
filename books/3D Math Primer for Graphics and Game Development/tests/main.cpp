#define LX [&](auto&&x)

#include <ostream>
#include <locale>

#define CATCH_CONFIG_MAIN 
#include "catch.hpp"
#include "../src/math.h"
#include "properties.h"
#include <decimal.h>

#include <Windows.h>

std::ostream& operator<< (std::ostream& out, const math::vec2& v)
{
	return out << "[" << v.x << ";" << v.y << "]";
}

std::ostream& operator<< (std::ostream& out, const math::vec3& v)
{
	return out << "[" << v.x << ";" << v.y << ";" << v.z << "]";
}



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

		

		template <typename T>
		std::ostream& operator<< (std::ostream& out, const math::mat4_t<T>& v)
		{
			std::cout.imbue(std::locale("en"));
			out << "{";
			for (size_t i = 0; i < 4; ++i)
			{
				for (size_t j = 0; j < 4; ++j)
					out << v(i, j) << ",";
				out << std::endl;
			}
			out << "}";
			return out;
		}
	}
}

bool nearEnough(math::mat4 a, math::mat4 b)
{
	return (a - b).all_of(LX{
		REQUIRE(std::abs(x) < 0.1f);
		return true;
	});
}

#include <autocheck/autocheck.hpp>

TEST_CASE("f32.equality.bignumbers")
{
	//TODO analyze
	math::f32 a = -8.74227766e-08;
	math::f32 b = 0;
	auto r = a == b;
	REQUIRE(r);

	/*
		+ [4]{ value = -40914.9805 }	math::f32
		+ [5]{ value = 7921.84766 }	math::f32
		+ [6]{ value = -117605.023 }	math::f32
		+ [7]{ value = -25055.8125 }	math::f32
		+ [8]{ value = 29136.3203 }	math::f32
		+ [9]{ value = -1154.84375 }	math::f32
		+ [10]{ value = 87454.1797 }	math::f32
		+ [11]{ value = 20150.0469 }	math::f32
		+ [12]{ value = 14917.4092 }	math::f32
		+ [13]{ value = 10.7871094 }	math::f32
		+ [14]{ value = 51313.8828 }	math::f32
		+ [15]{ value = 12570.4160 }	math::f32
		
		+ [4]{ value = -40914.9844 }	math::f32
		+ [5]{ value = 7921.84668 }	math::f32
		+ [6]{ value = -117605.016 }	math::f32
		+ [7]{ value = -25055.8125 }	math::f32
		+ [8]{ value = 29136.3242 }	math::f32
		+ [9]{ value = -1154.84253 }	math::f32
		+ [10]{ value = 87454.1797 }	math::f32
		+ [11]{ value = 20150.0469 }	math::f32
		+ [12]{ value = 14917.4141 }	math::f32
		+ [13]{ value = 10.7922363 }	math::f32
		+ [14]{ value = 51313.8789 }	math::f32
		+ [15]{ value = 12570.4199 }	math::f32*/

}

template <size_t D> void assert_zero_vector()
{
	auto v = math::zeros<D>();
	REQUIRE(v.all_of([](auto&& x) {return x == 0; }));
	REQUIRE(v.any_of([](auto&& x) {return x != 0; }) == false);
	REQUIRE(v.none_of([](auto&& x) {return x != 0; }));
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
			return a.norm() + b.norm() >= (a + b).norm();
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
	/*
	[46] Failure: cross product associativity with scalar - Falsifiable, after 66 tests :
	(-14, 5519, [-13, 3776; -19, 4982; 11, 6272], [28, 7027; -11, 6362; 6, 9427]) in D : \github\sandbox\books\3D Math Primer for Graphicsand Game Development\tests\properties.h:line 46
	*/

	if constexpr (D == 3)
	{
		auto x = math::vec3{ 1,0,0 };
		auto y = math::vec3{ 0,1,0 };
		auto z = math::vec3{ 0,0,1 };

		REQUIRE(x % y ==  z);
		REQUIRE(x % z == -y);

		REQUIRE(y % x == -z);
		REQUIRE(y % z ==  x);

		REQUIRE(z % x == y);
		REQUIRE(z % y == -x);

		check("cross product anti-comutativity", anyvec,
			[&](auto&& a)
			{
				return a % a == math::vec3{ 0,0,0 };
			});
		check("cross product anti-comutativity", anyvec, anyvec,
			[&](auto&& a, auto&& b)
			{
				return  a % b == -(b % a);
			});
		check("cross product anti-comutativity", anyvec, anyvec,
			[&](auto&& a, auto&& b)
			{
				return a % b == (-a % -b);
			});

		check("cross product associativity with scalar", anyfloat, anyvec, anyvec,
			[&](auto&& t, auto&& a, auto&& b)
			{
				return t * (a % b) == (t * a) % b;
			});
		check("cross product associativity with scalar", anyfloat, anyvec, anyvec,
			[&](auto&& t, auto&& a, auto&& b)
			{
				return t * (a % b) == a % (t * b);
			});
		check("cross product distributivity over sum", anyvec, anyvec, anyvec,
			[&](auto&& a, auto&& b, auto&& c)
			{
				return a % (b + c) == ((a % b) + (a % c));
			});
	}
}

TEST_CASE("math.vec2.algebraProperties", "[ok]")
{
	vector_properties<2>();
	vector_properties<3>();
}

TEST_CASE("math.vec2.norml2", "[ok]")
{
	auto a = math::vec2{ 1,1 };
	
	REQUIRE(a.norm() == sqrt(2.0f));
	REQUIRE(a.norm<2>() == sqrt(2.0f));
	REQUIRE(a.norm2() == 2.0_f32);
	REQUIRE(a.norm2<2>() == 2.0_f32);
}

TEST_CASE("math.vec2.normalize", "[ok]")
{
	auto a = math::vec2{ 2,0 };

	REQUIRE(a.normalized() == math::vec2{ 1, 0});
	REQUIRE(a.normalize() == math::vec2{ 1, 0 });

	check("normalized norm must be 1 or zero", any_vec<2>(),
		[&](auto&& a)
		{
			auto l2 = a.normalized().norm();
			return (l2 == 1.0_f32) || (l2 == 0.0_f32);
		});

	check("norm is idempotent", any_vec<2>(),
		[&](auto&& a)
		{
			return a.normalized().normalized() == a.normalized();
		});
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

	/*check("dot product is associative", any_vec<2>(), any_vec<2>(), any_vec<2>(),
		[&](auto&& a, auto&& b, auto&& c)
		{
			return (a * b) * c == a * (b * c);
		});*/
}

TEST_CASE("math.vec3.crossproduct", "[ok]")
{
	auto a = math::vec3{ 1,0,0 };
	auto b = math::vec3{ 0,1,0 };

	REQUIRE((a % b) == math::vec3{ 0,0,1 });
	REQUIRE((b % a) == math::vec3{ 0,0,-1 });
}

// MATRICES TESTS

TEST_CASE("math.mat4.identity", "[ok]")
{
	auto a = math::mat4::id();
	a.for_each([](auto i, auto j, auto&& x) {
		if (i == j) REQUIRE(x == 1.0_f32);
		else REQUIRE(x == 0.0_f32);
	});

	REQUIRE(a.determinant() == 1);

	check("id is Identity in matrix-matrix multiplication", any_mat4(),
		[&](auto&& m)
		{
			return a * m == m;
		});

	check("id is Identity in matrix-vector multiplication", any_vec<4>(),
		[&](auto&& v)
		{
			return a * v == v;
		});
}

TEST_CASE("math.mat4.diagonal", "[ok]")
{
	auto a = math::mat4::diagonal(0,1,2,3);
	a.for_each([](auto i, auto j, auto&& x) {
		if (i == j) REQUIRE(x == (float)i);
		else REQUIRE(x == 0.0_f32);
	});
}

TEST_CASE("math.mat4.transpose", "[ok]")
{
	// 1 5  9 13
	// 2 6 10 14
	// 3 7 11 15
	// 4 8 12 16
	auto a = math::mat4{ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 };
	//  1  2  3  4
	//  5  6  7  8
	//  9 10 11 12
	// 13 14 15 16
	REQUIRE(a.transposed() == math::mat4{ 1, 5, 9, 13, 2, 6, 10, 14, 3, 7, 11, 15, 4, 8, 12, 16 });
	REQUIRE(math::mat4::id().transposed() == math::mat4::id());

	check("matrix transpose is its inverse", any_mat4(),
		[&](auto&& a)
		{
			return a.transposed().transposed() == a;
		});
	check("matrix diagonal transpose does not change the matrix", diagonal_mat4(),
		[&](auto&& a)
		{
			return a.transposed() == a;
		});
	check("transpose does not change determinant", any_mat4(),
		[&](auto&& a)
		{
			return a.transposed().determinant() == a.determinant();
		});
}

TEST_CASE("math.mat4.multiply scalar", "[ok]")
{
	auto a = math::mat4{ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 };

	REQUIRE(a * 2 == math::mat4{ 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32 });
	REQUIRE(2 * a == math::mat4{ 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32 });
}

TEST_CASE("math.mat4.multiply", "[ok]")
{
	auto a = math::mat4{ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 };
	REQUIRE(a * a == math::mat4{90, 100, 110, 120, 202, 228, 254, 280, 314, 356, 398, 440, 426, 484, 542, 600});

	check("matrix multiplication is associative", any_mat4(), any_mat4(), any_mat4(),
		[&](auto&& a, auto&& b, auto&& c)
		{
			auto r1 = a *(b * c);
			auto r2 = (a * b) * c;
			
			//return r1 == r2;
			return nearEnough(r1, r2);
		});


	// NOT STABLE
	check("matrix multiplication is associative with scalar", any_float(), any_mat4(), any_mat4(),
		[&](auto&& f, auto&& a, auto&& b)
		{
			auto r1 = (f * a) * b;
			auto r2 = a * (f * b);
			auto r3 = f * (a * b);

			CHECK(r1 == r2);
			CHECK(r1 == r3);

			//return r1 == r2 && r2 == r3;
			return nearEnough(r1, r2) && nearEnough(r1, r3);
		});

	check("transpose of multiplication is inversed multiplication of transpose", any_mat4(), any_mat4(),
		[&](auto&& a, auto&& b)
		{
			return (a * b).transposed() == (b.transposed() * a.transposed());
		});

	check("determinant of product is product of determinants", any_mat4(), any_mat4(),
		[&](auto&& a, auto&& b)
		{
			return (a * b).determinant() == (a.determinant() * b.determinant());
		});

	

	//left associativity with vector
	//(vA)B = v(AB).
}

TEST_CASE("math.mat4vec4.multiply", "[ok]")
{
	// 1 5  9 13   17    17*1 + 18*5 + 19*9  + 20*13
	// 2 6 10 14   18    17*2 + 18*6 + 19*10 + 20*14
	// 3 7 11 15 * 19 =  17*3 + 18*7 + 19*11 + 20*15
	// 4 8 12 16   20    17*4 + 18*8 + 19*12 + 20*16
	auto a = math::mat4{ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 };
	auto v = math::vec4{ 17, 18, 19, 20 };

	REQUIRE(a * v == math::vec4{
		17 * 1 + 18 * 5 + 19 * 9 + 20 * 13,
		17 * 2 + 18 * 6 + 19 * 10 + 20 * 14,
		17 * 3 + 18 * 7 + 19 * 11 + 20 * 15,
		17 * 4 + 18 * 8 + 19 * 12 + 20 * 16
		});

	check("mat * vec distributivity", any_vec<4>(), any_vec<4>(), any_mat4(),
		[&](auto&& a, auto&& b, auto&& m)
		{
			return m * (a + b) == (m * a) + (m * b);
		});
}

TEST_CASE("math.mat4vec4.translation", "[ok]")
{
	check("mat can be translation", any_float(), any_float(), any_float(), any_vec<4>(),
		[&](auto&& tx, auto&& ty, auto&& tz, auto&& v)
		{
			v.w = 1;
			auto m = math::mat4::translation(tx, ty, tz, 1);
			return m * v == v + math::vec4{ tx, ty, tz, 0 };
		});
}

TEST_CASE("math.mat4vec4.scale", "[ok]")
{
	check("mat can be scale", any_float(), any_float(), any_float(), any_vec<4>(),
		[&](auto&& sx, auto&& sy, auto&& sz, auto&& v)
		{
			v.w = 1;
			auto m = math::mat4::scale(sx, sy, sz, 1);
			return m * v == math::vec4{ v.x * sx, v.y * sy, v.z * sz, 1.0 };
		});
}

TEST_CASE("math.mat4vec4.rotation", "[ok]")
{
	auto xaxis = math::vec3::e<0>();
	auto zaxis = math::vec4{ 0, 0, 1, 1 };
	auto yaxis = math::vec4{ 0, 1, 0, 1 };
	REQUIRE(math::mat4::rotation(xaxis, 0) * zaxis == zaxis);
	REQUIRE(math::mat4::rotation(xaxis, math::pif / 2) * zaxis == yaxis);
	REQUIRE(math::mat4::rotation(xaxis, math::pif) * zaxis == math::vec4{ 0, 0, -1, 1 });

	check("rotation angle 2k*pi must not change vector", any_int(),
		[&](auto&& k)
		{
			auto rx = math::mat4::rotation(math::vec3::e<0>(), 2 * k * math::pif);
			auto ry = math::mat4::rotation(math::vec3::e<1>(), 2 * k * math::pif);
			auto rz = math::mat4::rotation(math::vec3::e<2>(), 2 * k * math::pif);
			return (rx * zaxis == zaxis) && (ry * zaxis == zaxis) && (rz * zaxis == zaxis);
		});

	check("mat rotation does not change distance", any_float(),
		[&](auto&& angle)
		{
			auto m = math::mat4::rotation(math::vec3::e<0>(), angle);
			return math::dist(m * zaxis, math::vec4{ 0,0,0,1 }) == 1;
		});

	check("mat rotation is ortogonal", any_float(), any_float(), any_float(), any_float(),
		[&](auto&& x, auto&& y, auto&& z, auto&& angle)
		{
			auto m = math::mat4::rotation(math::vec3{x,y,z}, angle);
			return m * m.transposed() == math::mat4::id();
		});

	check("mat rotation determinant is 1", any_float(), any_float(), any_float(), any_float(),
		[&](auto&& x, auto&& y, auto&& z, auto&& angle)
		{
			auto m = math::mat4::rotation(math::vec3{ x,y,z }, angle);
			return m.determinant() == 1;
		});

	check("matrix to_quat rotation and matrix rotation must be equal",
		any_float(), any_float(), any_float(), any_float(),
		[&](auto&& x, auto&& y, auto&& z, auto&& angle)
		{
			auto m = math::mat4::rotation(math::vec3{ x,y,z }, angle);
			auto qr = m.to_quat().rotate(yaxis.xyz());
			return (m * yaxis).xyz() == qr;
		});
}

TEST_CASE("math.mat4vec4.inverse", "[ok]")
{
	auto id = math::mat4::id();
	auto [r, inv] = id.inverse();
	REQUIRE(inv == id);

	check("mat inverse times m must return id", any_mat4(),
		[&](auto&& m)
		{
			auto [r, inv] = m.inverse();
			if (!r) return true;

			return m * inv == math::mat4::id();
		});

	check("transpose of the inverse is the inverse of the transposed", any_mat4(),
		[&](auto&& m)
		{
			auto [r, minv] = m.inverse();
			if (!r) return true;

			auto [r2, mtinv] = m.transposed().inverse();
			if (!r2) return true;

			return mtinv == minv.transposed();
		});

	check("inverse of multiplication is the inverted multiplication of inverses", any_mat4(), any_mat4(),
		[&](auto&& a, auto&& b)
		{
			if (
				auto [r, abinv, ra, ainv, rc, binv] = std::tuple_cat(
					(a * b).inverse(),
					a.inverse(),
					b.inverse()
				); r && ra && rc)
			{
				return abinv == binv * ainv;
			}
			else
			{
				return true;
			}
		});

	check("mat inverse determinant is 1 over mat determinant", any_mat4(),
		[&](auto&& m)
		{
			auto [r, inv] = m.inverse();
			if (!r) return true;

			return inv.determinant() * m.determinant() == 1;
		});

	// NOT STABLE YET
	// read slooooowly
	/*check("mat inverse inverse is mat inverse", any_mat4(),
		[&](auto&& m)
		{
			if (auto [r, inv] = m.inverse(); r)
			{
				auto [r2, inv2] = inv.inverse();
				CHECK(r2);

				auto diff = inv2 - m;

				return inv2 == m;
			}
			else return true;
		});*/
}

template <typename T>
void mat4mul(T * A, T * B, T * R)
{
	#define alpha( i,j ) A[ (j)*4 + i ]   // map alpha( i,j ) to array A 
	#define beta( i,j )  B[ (j)*4 + i ]   // map beta( i,j )  to array B
	#define gamma( i,j ) R[ (j)*4 + i ]   // map gamma( i,j ) to array C

	#define LOOPI for ( int i=0; i<4; i++ )
	#define LOOPJ for ( int j=0; j<4; j++ )
	#define LOOPP for ( int p=0; p<4; p++ )
	LOOPJ
		LOOPP
			LOOPI
				gamma(i, j) += alpha(i, p) * beta(p, j);
}

TEST_CASE("math.mat4.multiply must not be slower than manual multiply", "[ok]")
{
#ifdef NDEBUG
	float A[16] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 };
	float B[16] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 };
	float R[16] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 };

	std::clock_t start = std::clock();
	for (size_t i = 0; i < 100000000; ++i)
	{
		mat4mul(R, A, B);
	}
	auto manualTime = (std::clock() - start) / (double)(CLOCKS_PER_SEC / 1000);

	REQUIRE(std::all_of(std::begin(R), std::end(R), [](auto&& x) { return x >= 0 || x <= 0; }));

	auto a = math::mat4::id();
	auto b = math::mat4::id();
	auto r = math::mat4::diagonal(0, 0, 0, 0);

	start = std::clock();
	for (size_t i = 0; i < 100000000; ++i)
	{
		math::mat4::mul(r, a,  b);
	}
	auto mathTime = (std::clock() - start) / (double)(CLOCKS_PER_SEC / 1000);

	std::cout << "manual: " << manualTime << " math: " << mathTime << std::endl;

	REQUIRE(r == math::mat4::id());

	REQUIRE(mathTime <= manualTime * 1.05); // NOT GOOD ENOUGH
#endif
}

TEST_CASE("math.quat.identity", "[ok]")
{
	auto id = math::quat::id();
	REQUIRE(id == math::quat{ 0,0,0,1 });
	REQUIRE(id.norm() == 1);

	check("quat identity is identity for multiplication", any_quat(),
		[&](auto&& q)
		{
			auto qq = q * id;
			return q * id == q;
		});
}

TEST_CASE("math.quat.normalization", "[ok]")
{
	auto sqrt30 = std::sqrt(30.0f);
	auto a = math::quat(2,3,4, 1);
	REQUIRE(a.norm() == sqrt30);
	REQUIRE(a.normalize() == math::quat(2.0f / sqrt30,
		3.0f / sqrt30,
		4.0f / sqrt30,
		1.0f / sqrt30)
	);
}

TEST_CASE("math.quat.conjugate", "[ok]")
{
	auto q = math::quat(1, 2, 3, 4);
	REQUIRE(q.conjugated() == math::quat{-1,-2,-3, 4});

	check("unit quat conjugate is the inverse of quaternion", any_unitquat(),
		[&](auto&& q)
		{
			return q * q.conjugated() == math::quat::id();
		});
}

TEST_CASE("math.quat.inverse", "[ok]")
{
	check("quat inverse is the inverse of quaternion", any_unitquat(),
		[&](auto&& q)
		{
			auto [r, inv] = q.inversed();
			return q * inv == math::quat::id();
		});
}

TEST_CASE("math.quat.multiplication", "[ok]")
{
	check("quat multiplication is associative", any_unitquat(), any_unitquat(), any_unitquat(),
		[&](auto&& a, auto&& b, auto&& c)
		{
			return (a * b) * c == a * (b * c);
		});
	check("quat multiplication norm is multiplication of norm", any_unitquat(), any_unitquat(),
		[&](auto&& a, auto&& b)
		{
			return (a * b).norm() == a.norm() * b.norm();
		});
	check("quat multiplication inverse if the inverse multiplication of inverses", any_unitquat(), any_unitquat(),
		[&](auto&& a, auto&& b)
		{
			auto [r1, inv1] = (a * b).inversed();
			auto [r2, inv2] = a.inversed();
			auto [r3, inv3] = b.inversed();
			if (r1 && r2 && r3) return inv1 == inv3 * inv2;
			else return true;
		});
}

TEST_CASE("math.quat.rotate", "[ok]")
{
	auto p = math::vec3{ 0, 0, 1 };
	REQUIRE(math::quat::from(1, 0, 0, math::pif / 2.0f).rotate(p) == math::vec3{ 0, -1, 0 });
	REQUIRE(math::quat::from(1, 0, 0, math::pif).rotate(p) == math::vec3{ 0, 0, -1 });

	check("quat rotation does not change distance", any_float(),
		[&](auto&& angle)
		{
			auto q = math::quat::from(1, 0, 0, angle);
			return math::dist(q.rotate(p), math::vec3{ 0,0,0 }) == 1;
		});
}

TEST_CASE("math.quat.slerp", "[ok]")
{
	auto q1 = math::quat{ 1, 0, 0, 0 };
	auto q2 = math::quat{ 0, 1, 0, 0 };

	REQUIRE(math::slerp(q1, q2, 0.0) == math::quat{ 1, 0, 0, 0 });
	REQUIRE(math::slerp(q1, q2, 0.1) == math::quat{ 0.9876883405951378, 0.15643446504023087, 0, 0 });
	REQUIRE(math::slerp(q1, q2, 0.2) == math::quat{ 0.9510565162951535, 0.3090169943749474, 0, 0 });
	REQUIRE(math::slerp(q1, q2, 0.3) == math::quat{ 0.8910065241883678, 0.4539904997395468, 0, 0 });
	REQUIRE(math::slerp(q1, q2, 0.4) == math::quat{ 0.8090169943749475, 0.5877852522924731, 0, 0 });
	REQUIRE(math::slerp(q1, q2, 0.5) == math::quat{ 0.7071067811865475, 0.7071067811865475, 0, 0 });
	REQUIRE(math::slerp(q1, q2, 0.6) == math::quat{ 0.5877852522924731, 0.8090169943749475, 0, 0 });
	REQUIRE(math::slerp(q1, q2, 0.7) == math::quat{ 0.4539904997395468, 0.8910065241883678, 0, 0 });
	REQUIRE(math::slerp(q1, q2, 0.8) == math::quat{ 0.3090169943749475, 0.9510565162951535, 0, 0 });
	REQUIRE(math::slerp(q1, q2, 0.9) == math::quat{ 0.156434465040231, 0.9876883405951377, 0, 0 });
	REQUIRE(math::slerp(q1, q2, 1.0) == math::quat{ 0, 1, 0, 0 });
}

TEST_CASE("math.quat.to_mat4", "[ok]")
{
	auto mid = math::mat4::id();
	REQUIRE(mid.to_quat() == math::quat::id());

	auto p4 = math::vec4{ 1,1,1,1 };

	check("quat to_mat4 to_quat must be equal original quat",
		any_float(), any_float(), any_float(), any_float(),
		[&](auto&& x, auto&& y, auto&& z, auto&& w)
		{
			auto q = math::quat::from(x, y, z, w).normalized();
			auto q2 = q.to_mat4().to_quat();
			return q2 == q || q2 == -q; // can we avoid this second condition?
		});

	check("quat to_mat4 must rotate point like rotate function", 
		any_float(), any_float(), any_float(), any_float(),
		[&](auto&& x, auto&& y, auto&& z, auto&& w)
		{
			auto q = math::quat::from(x, y, z, w).normalized();
			auto r1 = q.to_mat4() * p4;
			auto r2 = q.rotate(p4.xyz());
			return r1.xyz() == r2;
		});
}

float wrapPi(float theta)
{
	if(fabs(theta) <= math::pif) {
		const float TWOPPI = 2.0f*math::pif;
		float revolutions = floor((theta + math::pif) *
			(1.0f / TWOPPI));
		theta -= revolutions*TWOPPI;
	}
	return theta;
}

void canonical_euler(math::vec3& v)
{
	while(v.y < -math::pif) v.y += 2* math::pif;
	while(v.y >= math::pif) v.y -= 2 * math::pif;

	while (v.x < -math::pif / 2.0f) v.x += 2 * math::pif;
	while (v.x >= math::pif / 2.0f) v.x -= 2 * math::pif;

	if (v.x == math::pif) v.z = 0;
	if (v.x == -math::pif) v.z = 0;

	while (v.z < -math::pif) v.z += 2 * math::pif;
	while (v.z >= math::pif) v.z -= 2 * math::pif;
}

TEST_CASE("math.quat.euler_angles", "[ok]")
{
	auto p4 = math::vec3{ 1,1,1 };

	check("quat from_euler and to_euler must be equal",
		any_vec<3>(),
		[&](auto&& v)
		{
			//v = math::vec3{ -29.8576, -33.7777, 6.90189 };
			auto q = math::quat::from_euler(v.y, v.x, v.z);
			auto a = q.to_euler();
			auto q2 = math::quat::from_euler(a.y, a.x, a.z);

			// cannot compare euler angles
			// because of multi representation

			auto dist = math::dist(
				q.rotate(p4),
				q2.rotate(p4));
			return dist < 0.01;
		});

	check("quat from_euler must be equal to h*p*b",
		any_float(), any_float(), any_float(),
		[&](auto&& h, auto&& p, auto&& b)
		{
			auto q = math::quat::from_euler(h, p, b);
			
			auto hq = math::quat::from(0, 1, 0, h);
			auto pq = math::quat::from(1, 0, 0, p);
			auto bq = math::quat::from(0, 0, 1, b);
			return hq * pq * bq == q;
		});
}

TEST_CASE("math.AABB.matrix transformation", "[ok]")
{
	auto aabb = math::AABB3{};
	aabb.add(math::vec3{ 1,1,1 });
	aabb.add(math::vec3{ -1,-1,-1 });

	REQUIRE(aabb.inside(math::vec3{ 0, 0, 0 }) == true);
	REQUIRE(aabb.inside(math::vec3{ 2, 2, 2 }) == false);

	auto m = math::mat4::translation(2, 2, 2, 1);

	REQUIRE((m * aabb).inside(math::vec3{ 2, 2, 2 }) == true);
	
	//HOW TO TEST ROTATION
}

TEST_CASE("math.plane.from 3 points", "[ok]")
{
	using namespace math;
	{
		auto [r, p] = plane3::from(vec3{ 0,0,0 }, vec3{ 1,0,0 }, vec3{ 0,0,1 });

		REQUIRE(r);
		REQUIRE(p.normal == vec3{ 0, 1, 0 });
		REQUIRE(p.d == 0);
	}
	{
		auto [r, p] = plane3::from(vec3{ 0,1,0 }, vec3{ 1,1,0 }, vec3{ 0,1,1 });

		REQUIRE(r);
		REQUIRE(p.normal == vec3{ 0, 1, 0 });
		REQUIRE(p.d == 1);
	}
}

TEST_CASE("math.distance.point-plane", "[ok]")
{
	using namespace math;
	auto [r, p] = plane3::from(vec3{ 0,0,0 }, vec3{ 1,0,0 }, vec3{ 0,0,1 });

	REQUIRE(dist(vec3{ 0,0,0 }, p) == 0);
	REQUIRE(dist(vec3{ 0,1,0 }, p) == 1);
	REQUIRE(dist(vec3{ 0,-1,0 }, p) == 1);
}

TEST_CASE("math.intersection.ray-sphere", "[ok]")
{
	using namespace math;

	auto s = math::sphere3{ {0,0,0}, 1 };

	//
	//  +----------+
	//	|          |
	//	|          |
	//	|    x     | < ---------- +
	//	|  (0,0)   |            (10,0)
	//	|          |
	//	+----------+
	//
	auto r = math::ray3{ {10, 0, 0}, {-1, 0, 0} };
	auto points = math::intersection(r, s);

	REQUIRE(points.size() == 2);
	REQUIRE(points[0] == vec3{ 1,0,0 });
	REQUIRE(points[1] == vec3{-1,0,0 });

	//
	//                         +----------+
	//	                       |          |
	//	                       |          |
	//	< ---------- +         |    x     | 
	//	           (10,0)      |  (0,0)   | 
	//	                       |          |
	//	                       +----------+
	//

	r = math::ray3{ {-10, 0, 0}, {-1, 0, 0} };
	points = math::intersection(r, s);
	REQUIRE(points.size() == 0);

	//
	//        +----------+
	//	      |          |
	//	      |          |
	//	  <-----x  x     | 
	//	      |  (0,0)   | 
	//	      |          |
	//	      +----------+
	//

	r = math::ray3{ {-0.5, 0, 0}, {-1, 0, 0} };
	points = math::intersection(r, s);
	REQUIRE(points.size() == 1);
	REQUIRE(points[0] == vec3{-1,0,0 });

	//
	//   Only tangentially touch the sphere
	//
	//   +----------+   <--------x
	//	 |          |
	//	 |          |
	//	 |    x     | 
	//	 |  (0,0)   | 
	//	 |          |
	//	 +----------+
	//

	r = math::ray3{ {10, 1, 0}, {-1, 0, 0} };
	points = math::intersection(r, s);
	REQUIRE(points.size() == 1);
	REQUIRE(points[0] == vec3{ 0,1,0 });
}

TEST_CASE("math.intersection.line_segment-sphere", "[ok]")
{
	using namespace math;

	auto s = math::sphere3{ {0,0,0}, 1 };

	//
	//          +----------+
	//	        |          |
	//	        |          |
	//	  <----------------------------- +
	//	        |  (0,0)   |            (10,0)
	//	        |          |
	//	        +----------+
	//	        
	auto r = math::seg3{ {10, 0, 0}, {-10, 0, 0} };
	auto points = math::intersection(r, s);
	REQUIRE(points.size() == 2);
	REQUIRE(points[0] == vec3{ 1,0,0 });
	REQUIRE(points[1] == vec3{ -1,0,0 });

	//
	//          +----------+
	//	        |          |
	//	        |          |
	//	        |    <------------------- +
	//	        |  (0,0)   |            (10,0)
	//	        |          |
	//	        +----------+
	//	        
	r = math::seg3{ {10, 0, 0}, {0, 0, 0} };
	points = math::intersection(r, s);
	REQUIRE(points.size() == 1);
	REQUIRE(points[0] == vec3{ 1,0,0 });


	//
	//          +----------+
	//	        |          |
	//	        |          |
	//	        |          |    <-------- +
	//	        |  (0,0)   |   (5,0)    (10,0)
	//	        |          |
	//	        +----------+
	//	        
	r = math::seg3{ {10, 0, 0}, {5, 0, 0} };
	points = math::intersection(r, s);
	REQUIRE(points.size() == 0);
}


TEST_CASE("math.spatial-partitioning", "[ok]")
{
	using namespace math;

	math::ugrid3 g;

	auto s = math::sphere3{ {0,0,0}, 1 };
	g.insert(&s);

	auto p = g.new_presence({ 10,0,0 });

	auto events = g.step_to(p, { 0,0,0 });
	REQUIRE(events.size() == 1);
	REQUIRE(events[0].pos == vec3{ 1,0,0 });
	REQUIRE(events[0].status == entering_exiting::entering);

	events = g.step_to(p, {-10,0,0 });
	REQUIRE(events.size() == 1);
	REQUIRE(events[0].pos == vec3{-1,0,0 });
	REQUIRE(events[0].status == entering_exiting::exiting);

	events = g.step_to(p, { 10,0,0 });
	REQUIRE(events.size() == 2);
	REQUIRE(events[0].pos == vec3{ -1,0,0 });
	REQUIRE(events[0].status == entering_exiting::entering);
	REQUIRE(events[1].pos == vec3{ 1,0,0 });
	REQUIRE(events[1].status == entering_exiting::exiting);
}