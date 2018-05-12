#define CATCH_CONFIG_MAIN
#include <catch/catch.hpp>
#include "math.algebra.linear.hpp"

using namespace ma::math::algebra::linear;

bool equal(float a, float b, float epsilon)
{
	if (fabs(a) < epsilon)
	{
		return fabs(b) < epsilon;
	}
	else
	{
		auto upper = a * 1.000000 + epsilon;
		auto lower = a * 1.000000 - epsilon;
		return b > lower && b < upper;
	}
}

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
	actual.zip(expected, [&](auto i, auto j, auto lx, auto rx) {
		if (!equal(lx, rx, 0.0001)) {
			std::cout << "Actual" << std::endl;
			std::cout << "------" << std::endl;
			actual.print(std::cout, i, j);

			std::cout << "Expected" << std::endl;
			std::cout << "------" << std::endl;
			expected.print(std::cout, i, j);

			REQUIRE(equal(lx, rx, 0.0001));
		}
		else REQUIRE(equal(lx, rx, 0.0001));
	});
}

template <typename T1, typename T2>
void assertEqual(const T1& actual, const T2& expected)
{
	actual.zip(expected, [&](auto i, auto j, auto lx, auto rx) {
		if (!equal(lx, rx, 0.0001)) {
			std::cout << "Actual" << std::endl;
			std::cout << "------" << std::endl;
			actual.print(std::cout, i, j);

			std::cout << "Expected" << std::endl;
			std::cout << "------" << std::endl;
			expected.print(std::cout, i, j);

			equal(lx, rx, 0.0001);
		}
		else equal(lx, rx, 0.0001);
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

	//row oriented
	auto b = Vectorf<4>{ 2.0f, 3.0f, 2.0f, 9.0f };
	auto r = A.solve(b, SolveAlgorithm::row_oriented);
	auto expected = Vectorf<4>{ 1.0f, 2.0f, 3.0f, 4.0f };
	REQUIRE(r == true);
	assertEqual(b, expected);

	//column oriented
	b = Vectorf<4>{ 2.0f, 3.0f, 2.0f, 9.0f };
	r = A.solve(b, SolveAlgorithm::column_oriented);
	expected = Vectorf<4>{ 1.0f, 2.0f, 3.0f, 4.0f };
	REQUIRE(r == true);
	assertEqual(b, expected);
}

TEST_CASE("matrix.solve.foward.substituition.zero.prelude", "[ma.math.algebra.linear]") {
	auto A = Matrixf<5, 5>::ByRow({
		 1.0f, 0.0f, 0.0f, 0.0f, 0.0f,
		 1.0f, 2.0f, 0.0f, 0.0f, 0.0f,
		 1.0f,-1.0f, 2.0f, 0.0f, 0.0f,
		 1.0f, 3.0f, 1.0f,-1.0f, 0.0f,
		 1.0f, 4.0f, 1.0f,-3.0f, 3.0f });

	//row oriented
	auto b = Vectorf<5>{ 0.0f, 2.0f, 3.0f, 2.0f, 9.0f };
	auto r = A.solve(b, SolveAlgorithm::row_oriented);
	auto expected = Vectorf<5>{ 0.0f, 1.0f, 2.0f, 3.0f, 4.0f };
	REQUIRE(r == true);
	assertEqual(b, expected);

	//column oriented
	b = Vectorf<5>{ 0.0f, 2.0f, 3.0f, 2.0f, 9.0f };
	r = A.solve(b, SolveAlgorithm::column_oriented);
	expected = Vectorf<5>{ 0.0f, 1.0f, 2.0f, 3.0f, 4.0f };
	REQUIRE(r == true);
	assertEqual(b, expected);
}

TEST_CASE("matrix.solve.backward.substitution", "[ma.math.algebra.libear]")
{
	auto A = Matrixf<4, 4>::ByRow({
		+3.0f,+2.0f,+1.0f,+0.0f,
		+0.0f,+1.0f,+2.0f,+3.0f,
		+0.0f,+0.0f,-2.0f,+1.0f,
		+0.0f,+0.0f,+0.0f,+4.0f });

	//row oriented
	auto b = Vectorf<4>{ -10.0f,10.0f, 1.0f,12.0f };
	auto r = A.solve(b, SolveAlgorithm::row_backward_substitution);
	auto expected = Vectorf<4>{ -3.0f, -1.0f, 1.0f, 3.0f };
	REQUIRE(r == true);
	assertEqual(b, expected);

	////column oriented
	//b = Vectorf<4>{ 2.0f, 3.0f, 2.0f, 9.0f };
	//r = A.solve(b, SolveAlgorithm::column_oriented);
	//expected = Vectorf<4>{ 1.0f, 2.0f, 3.0f, 4.0f };
	//REQUIRE(r == true);
	//assertEqual(b, expected);
}


TEST_CASE("matrix.factor.cholesky.example1", "[ma.math.algebra.linear]") {
	auto A = Matrixf<2, 2>::ByRow({
		4.0f, 0.0f,
		0.0f, 9.0f });
	auto expected = Matrixf<2, 2>{ 2.0f, 0.0f, 0.0f, 3.0f };

	//row oriented
	auto r = A.factorCholesky();
	REQUIRE(r == true);
	assertEqual(A, expected);
}

TEST_CASE("matrix.factor.cholesky.example2", "[ma.math.algebra.linear]") {
	auto A = Matrixf<4, 4>::ByRow({
		 4.0f,-2.0f, 4.0f, 2.0f,
		-2.0f,10.0f,-2.0f,-7.0f,
		 4.0f,-2.0f, 8.0f, 4.0f,
		 2.0f,-7.0f, 4.0f, 7.0f });
	auto expected = Matrixf<4, 4>::ByRow({
		2.0f,-1.0f, 2.0f, 1.0f,
		0.0f, 3.0f, 0.0f,-2.0f,
		0.0f, 0.0f, 2.0f, 1.0f,
		0.0f, 0.0f, 0.0f, 1.0f
		});

	//row oriented
	auto r = A.factorCholesky();
	REQUIRE(r == true);
	assertEqual(A, expected);

	//verify
	//multiply R^T*R and test if it is equal to A
}

TEST_CASE("matrix.factor.cholesky.erasure", "[ma.math.algebra.linear]") {
	auto A = Matrixf<4, 4>::ByRow({
		 4.0f,-2.0f, 4.0f, 2.0f,
		-2.0f,10.0f,-2.0f,-7.0f,
		 4.0f,-2.0f, 8.0f, 4.0f,
		 2.0f,-7.0f, 4.0f, 7.0f });
	auto expected = Matrixf<4, 4>::ByRow({
		2.0f,-1.0f, 2.0f, 1.0f,
		0.0f, 3.0f, 0.0f,-2.0f,
		0.0f, 0.0f, 2.0f, 1.0f,
		0.0f, 0.0f, 0.0f, 1.0f
		});

	//row oriented
	auto[result, R] = A.factorCholeskyErasure();
	REQUIRE(result == true);
	assertEqual(R, expected);
	//verify

	//multiply R^T*R and test if it is equal to A
}

#include <tuple>
#include <fstream>

std::tuple<
	bool,
	DMatrix<float, NONE>,
	DMatrix<float, NONE>,
	DMatrix<float, NONE>> readMatrix(std::ifstream& in)
{
	std::vector<float> data;

	char comma;

	int size;
	if (!(in >> size)) return std::make_tuple(
		false,
		DMatrix<float, NONE>(),
		DMatrix<float, NONE>(),
		DMatrix<float, NONE>());
	for (int x = 0; x < size; ++x)
	{
		for (int y = 0; y < size; ++y)
		{
			float v;
			in >> v;
			data.push_back(v);
			if (y != (size - 1)) in >> comma;
		}
	}
	auto M = DMatrix<float, NONE>::ByRow(size, size, &data[0]);

	data.clear();
	for (int x = 0; x < size; ++x)
	{
		for (int y = 0; y < size; ++y)
		{
			float v;
			in >> v;
			data.push_back(v);
			if (y != (size - 1)) in >> comma;
		}
	}
	auto A = DMatrix<float, NONE>::ByRow(size, size, &data[0]);

	data.clear();
	for (int x = 0; x < size; ++x)
	{
		for (int y = 0; y < size; ++y)
		{
			float v;
			in >> v;
			data.push_back(v);
			if (y != (size - 1)) in >> comma;
		}
	}
	auto R = DMatrix<float, NONE>::ByRow(size, size, &data[0]);
	return std::make_tuple(true, M, A, R);
}

TEST_CASE("matrix.factor.cholesky.compare.octave", "[ma.math.algebra.linear][.][compare.octave]") {
	/*TCHAR s[100];
	DWORD a = GetCurrentDirectory(100, s);*/
	std::ifstream in("data.csv");
	if (!in.good()) return;
	while (true)
	{
		auto[ok, M, A, R] = readMatrix(in);
		if (!ok) break;

		auto a = M.transpose() * M;
		assertEqual(A, a);

		auto r = A.factorCholesky();
		REQUIRE(r == true);
		assertEqual(R, A);
	}
}

TEST_CASE("matrix.cholesky.solve.example1", "[ma.math.algebra.linear]") {
	auto A = Matrixf<3, 3>::ByRow({
		+36, -30, +24,
		-30, +34, -26,
		+24, -26, +21 });
	auto b = Matrixf<3, 1>::ByRow({ 0, 12, -7 });
	auto expected = Matrixf<3, 1>::ByRow({ 1, 2, 1 });

	A.solve(b, SolveAlgorithm::cholesky);
	assertEqual(expected, b);
}

TEST_CASE("matrix.cholesky.solve.example2", "[ma.math.algebra.linear]") {
	auto A = Matrixf<5, 5>::ByRow({
		1, 1, 1, 1, 1,
		1, 2, 2, 2, 2,
		1, 2, 3, 3, 3,
		1, 2, 3, 4, 4,
		1, 2, 3, 4, 5});
	auto b = Matrixf<5, 1>::ByRow({ 5, 9, 12, 14, 15 });
	auto expected = Matrixf<5, 1>::ByRow({ 1, 1, 1, 1, 1 });

	A.solve(b, SolveAlgorithm::cholesky);
	assertEqual(expected, b);
}