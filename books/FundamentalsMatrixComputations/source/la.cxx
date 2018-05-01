#define CATCH_CONFIG_MAIN
#include <catch/catch.hpp>
#include "math.algebra.linear.hpp"

using namespace ma::math::algebra::linear;

template <typename T>
void assertIsIdentity(const T& m)
{
	m.map([](auto i, auto j, auto x) {
		if (i == j) REQUIRE(x == 1);
		else REQUIRE(x != 1);
	});
}
template <typename T>
void assertEqual(const T& actual, const T& expected)
{
	actual.zip(expected, [](auto i, auto j, auto lx, auto rx) {
		REQUIRE(lx == rx);
	});
}

TEST_CASE("matrix.identity.construction.staticmethod", "[ma.math.algebra.linear]") {
	assertIsIdentity(Matrixf<1, 1>::Identity());
	assertIsIdentity(Matrixf<2, 1>::Identity());
	assertIsIdentity(Matrixf<3, 1>::Identity());
	assertIsIdentity(Matrixf<1, 2>::Identity());
	assertIsIdentity(Matrixf<2, 2>::Identity());
	assertIsIdentity(Matrixf<3, 2>::Identity());
	assertIsIdentity(Matrixf<1, 3>::Identity());
	assertIsIdentity(Matrixf<2, 3>::Identity());
	assertIsIdentity(Matrixf<3, 3>::Identity());
}

TEST_CASE("matrix.identity.identity.multiply.identity.is.identity", "[ma.math.algebra.linear]") {
	assertIsIdentity(Matrixf<1, 1>::Identity() * MatrixIdentityf<1>{});
	assertIsIdentity(Matrixf<2, 2>::Identity() * MatrixIdentityf<2>{});
	assertIsIdentity(Matrixf<3, 3>::Identity() * MatrixIdentityf<3>{});
}

TEST_CASE("matrix.identity.identity.multiply.anymatrix.is.anymatrix", "[ma.math.algebra.linear]") {
	auto m11 = Matrixf<1, 1>{ 1.0f };
	auto m21 = Matrixf<2, 1>{ 1.0f, 2.0f };
	auto m31 = Matrixf<3, 1>{ 1.0f, 2.0f, 3.0f };
	assertEqual(Matrixf<1, 1>::Identity() * m11, m11);
	assertEqual(Matrixf<2, 1>::Identity() * m21, m21);
	assertEqual(Matrixf<3, 1>::Identity() * m31, m31);
	assertEqual(Matrixf<1, 2>::Identity() * m11, m11);
	assertEqual(Matrixf<2, 2>::Identity() * m21, m21);
	assertEqual(Matrixf<3, 2>::Identity() * m31, m31);
	assertEqual(Matrixf<1, 3>::Identity() * m11, m11);
	assertEqual(Matrixf<2, 3>::Identity() * m21, m21);
	assertEqual(Matrixf<3, 3>::Identity() * m31, m31);
}

TEST_CASE("matrix.identity.anymatrix.multiply.identity.is.anymatrix", "[ma.math.algebra.linear]") {
	auto m11 = Matrixf<1, 1>{ 1.0f };
	auto m12 = Matrixf<1, 2>{ 1.0f, 2.0f };
	auto m13 = Matrixf<1, 3>{ 1.0f, 2.0f, 3.0f };
	assertEqual(m11 * Matrixf<1, 1>::Identity(), m11);
	assertEqual(m12 * Matrixf<2, 1>::Identity(), m12);
	assertEqual(m13 * Matrixf<3, 1>::Identity(), m13);
	assertEqual(m11 * Matrixf<1, 2>::Identity(), m11);
	assertEqual(m12 * Matrixf<2, 2>::Identity(), m12);
	assertEqual(m13 * Matrixf<3, 2>::Identity(), m13);
	assertEqual(m11 * Matrixf<1, 3>::Identity(), m11);
	assertEqual(m12 * Matrixf<2, 3>::Identity(), m12);
	assertEqual(m13 * Matrixf<3, 3>::Identity(), m13);
}

TEST_CASE("matrix.multiply.1x1.1x1", "[ma.math.algebra.linear]") {
	auto m11 = Matrixf<1, 1>{ 1.0f };
	assertEqual(m11*m11, { 1.0f });
}

TEST_CASE("matrix.multiply.2x2.2x1", "[ma.math.algebra.linear]") {
	auto m22 = Matrixf<2, 2>{ 1.0f, 2.0f, 3.0f, 4.0f };
	auto v21 = Matrixf<2, 1>{ 5.0f, 6.0f };
	assertEqual(m22*v21, { 23.0f, 34.0f });
}

TEST_CASE("matrix.multiply.3x3.3x1", "[ma.math.algebra.linear]") {
	auto m33 = Matrixf<3, 3>{ 1.0f, 2.0f, 3.0f, 4.0f, 5.0f, 6.0f, 7.0f, 8.0f, 9.0f };
	auto v31 = Matrixf<3, 1>{ 10.0f, 11.0f, 12.0f };
	assertEqual(m33*v31, { 1.0f * 10 + 4 * 11 + 7 * 12, 2.0f * 10 + 5 * 11 + 8 * 12, 3.0f * 10 + 6 * 11 + 9 * 12 });
}

TEST_CASE("matrix.multiply.should.return.identity", "[ma.math.algebra.linear]") {
	auto m22 = Matrixf<2, 2>{ 1.0f, 0.0f, 0.0f, 1.0f };
	assertIsIdentity(m22*m22);

	auto m33 = Matrixf<3, 3>{ 1.0f, 0.0f, 0.0f, 0.0f, 1.0f, 0.0f, 0.0f, 0.0f, 1.0f };
	assertIsIdentity(m33*m33);
}

//octave:16> A
//A =
//
//   1   2   3
//   4   5   6
//
//octave:17> B
//B =
//
//    7   10
//    8   11
//    9   12
//
//octave:18> A*B
//ans =
//
//    50    68
//   122   167
//
TEST_CASE("matrix.multiply.32.23.equal.22", "[ma.math.algebra.linear]") {
	auto m32 = Matrixf<2, 3>{ 1.0f, 4.0f, 2.0f,  5.0f,  3.0f,  6.0f };
	auto m23 = Matrixf<3, 2>{ 7.0f, 8.0f, 9.0f, 10.0f, 11.0f, 12.0f };
	auto r22 = Matrixf<2, 2>{ 50.0f, 122.0f, 68.0f, 167.0f };
	assertEqual(m32*m23, r22);
}

//octave:19> A = [2,0,0,0;-1,2,0,0;3,1,-1,0;4,1,-3,3]
//A =
//
//   2   0   0   0
//  -1   2   0   0
//   3   1  -1   0
//   4   1  -3   3
//
//octave:20> b = [2,3,2,9]'
//b =
//
//   2
//   3
//   2
//   9
//
//octave:21> A\b
//ans =
//
//   1
//   2
//   3
//   4
//
//octave:22> A*[1,2,3,4]'
//ans =
//
//   2
//   3
//   2
//   9
//
//octave:23>
TEST_CASE("matrix.solve.foward.substituition", "[ma.math.algebra.linear]") {
	auto A = Matrixf<4, 4>::ByRow({
		 2.0f, 0.0f, 0.0f, 0.0f,
		-1.0f, 2.0f, 0.0f, 0.0f,
		 3.0f, 1.0f,-1.0f, 0.0f,
		 4.0f, 1.0f,-3.0f, 3.0f });
	auto b = Vectorf<4>{ 2.0f, 3.0f, 2.0f, 9.0f };
	
	auto r = A.solve(b);
	auto expected = Vectorf<4>{ 1.0f, 2.0f, 3.0f, 4.0f };

	REQUIRE(r == true);
	assertEqual(b, expected);
}