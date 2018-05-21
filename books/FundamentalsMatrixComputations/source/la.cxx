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
		if (i == j) REQUIRE(equal(x, 1.0f, 0.0001f));
		else REQUIRE(equal(x, 0.0f, 0.0001f));
	});
}

template <typename T>
void assertEqual(const T& actual, const T& expected)
{
	actual.zip(expected, [&](auto i, auto j, auto lx, auto rx) {
		if (!equal(lx, rx, 0.0001f)) {
			std::cout << "Actual" << std::endl;
			std::cout << "------" << std::endl;
			actual.print(std::cout, i, j);

			std::cout << "Expected" << std::endl;
			std::cout << "------" << std::endl;
			expected.print(std::cout, i, j);

			REQUIRE(equal(lx, rx, 0.0001f));
		}
		else REQUIRE(equal(lx, rx, 0.0001f));
	});
}

template <typename T1, typename T2>
void assertEqual(const T1& actual, const T2& expected)
{
	actual.zip(expected, [&](auto i, auto j, auto lx, auto rx) {
		if (!equal(lx, rx, 0.0001f)) {
			std::cout << "Actual" << std::endl;
			std::cout << "------" << std::endl;
			actual.print(std::cout, i, j);

			std::cout << "Expected" << std::endl;
			std::cout << "------" << std::endl;
			expected.print(std::cout, i, j);

			equal(lx, rx, 0.0001f);
		}
		else equal(lx, rx, 0.0001f);
	});
}

#include <chrono>

template<typename TimeT = std::chrono::milliseconds>
struct measure
{
	template<typename F, typename ...Args>
	static typename TimeT execution(F&& func, Args&&... args)
	{
		auto start = std::chrono::steady_clock::now();
		std::forward<decltype(func)>(func)(std::forward<Args>(args)...);
		auto duration = std::chrono::duration_cast<TimeT>
			(std::chrono::steady_clock::now() - start);
		return duration;
	}
};

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
	auto r = A.solve(b, SolveAlgorithm::forward_row_oriented);
	auto expected = Vectorf<4>{ 1.0f, 2.0f, 3.0f, 4.0f };
	REQUIRE(r == true);
	assertEqual(b, expected);

	//column oriented
	b = Vectorf<4>{ 2.0f, 3.0f, 2.0f, 9.0f };
	r = A.solve(b, SolveAlgorithm::forward_column_oriented);
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
	auto r = A.solve(b, SolveAlgorithm::forward_row_oriented);
	auto expected = Vectorf<5>{ 0.0f, 1.0f, 2.0f, 3.0f, 4.0f };
	REQUIRE(r == true);
	assertEqual(b, expected);

	//column oriented
	b = Vectorf<5>{ 0.0f, 2.0f, 3.0f, 2.0f, 9.0f };
	r = A.solve(b, SolveAlgorithm::forward_column_oriented);
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
	auto r = A.solve(b, SolveAlgorithm::backward_row_substitution);
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
	auto r = A.choleskyFactor();
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
	auto r = A.choleskyFactor();
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
	auto[result, R] = A.getCholeskyFactor();
	REQUIRE(result == true);
	assertEqual(R, expected);
	//verify

	//multiply R^T*R and test if it is equal to A
}

TEST_CASE("matrix.banded.envelope.2x2", "[ma.math.algebra.linear]")
{
	BandedSchemeArray<float> array1(2, 2, {
		1.0f, 2.0f,
		2.0f, 3.0f
		});
	REQUIRE(array1.getIndex(0, 0) == -1);
	REQUIRE(array1.getIndex(1, 0) == -1);
	REQUIRE(array1.getIndex(1, 1) == -1);

	REQUIRE(array1.getIndex(0, 1) == 0);
}

TEST_CASE("matrix.banded.envelope.3x3", "[ma.math.algebra.linear]")
{
	BandedSchemeArray<float> array2(3, 3, {
		1.0f, 2.0f, 3.0f,
		2.0f, 5.0f, 6.0f,
		3.0f, 6.0f, 9.0f
		});
	REQUIRE(array2.getIndex(0, 0) == -1);
	REQUIRE(array2.getIndex(1, 1) == -1);
	REQUIRE(array2.getIndex(2, 2) == -1);

	REQUIRE(array2.getIndex(0, 1) == +0);
	REQUIRE(array2.getIndex(0, 2) == +1);
	REQUIRE(array2.getIndex(1, 2) == +2);

	REQUIRE(array2.getIndex(1, 0) == -1);
	REQUIRE(array2.getIndex(2, 0) == -1);
	REQUIRE(array2.getIndex(2, 1) == -1);
}

TEST_CASE("matrix.banded.cholesky.example1", "[ma.math.algebra.linear]") {
	auto A = make_banded2f({
		4.0f, 0.0f,
		0.0f, 9.0f });
	auto expected = make_col2f({ 2.0f, 0.0f, 0.0f, 3.0f });

	//row oriented
	auto[r, R] = A.getCholeskyFactor();
	REQUIRE(r == true);
	assertEqual(R, expected);
}

TEST_CASE("matrix.cholesky.example2", "[ma.math.algebra.linear]") {
	auto A = make_rowf(5, 5, {
		4.0f, 0.0f, 6.0f, 0.0f, 2.0f,
		0.0f, 1.0f, 3.0f, 0.0f, 2.0f,
		6.0f, 3.0f,19.0f, 2.0f, 6.0f,
		0.0f, 0.0f, 2.0f, 5.0f,-5.0f,
		2.0f, 2.0f, 6.0f,-5.0f,16.0f,
		});
	auto expected = make_rowf(5, 5, {
		2.0f, 0.0f, 3.0f, 0.0f, 1.0f,
		0.0f, 1.0f, 3.0f, 0.0f, 2.0f,
		0.0f, 0.0f, 1.0f, 2.0f,-3.0f,
		0.0f, 0.0f, 0.0f, 1.0f, 1.0f,
		0.0f, 0.0f, 0.0f, 0.0f, 1.0f });

	auto[r, R] = A.getCholeskyFactor();
	REQUIRE(r == true);
	assertEqual(R, expected);
}

TEST_CASE("matrix.cholesky.example3", "[ma.math.algebra.linear]") {
	auto A = make_rowf(5, 5, {
		1.0f, 2.0f, 4.0f,
		2.0f,13.0f,16.0f,
		4.0f,16.0f,10.0f
		});

	REQUIRE(A.isPositiveDefinite() == false);
}

TEST_CASE("matrix.cholesky.inverse", "[ma.math.algebra.linear]") {
	auto A = make_rowf<3,3>({
		1.0f / 1.0f, 1.0f / 2.0f, 1.0f / 3.0f,
		1.0f / 2.0f, 1.0f / 3.0f, 1.0f / 4.0f,
		1.0f / 3.0f, 1.0f / 4.0f, 1.0f / 5.0f
		});
	auto A1 = A.inverse();

	auto I = A * A1;
	assertIsIdentity(I);	
}

TEST_CASE("matrix.banded.cholesky.performance", "[ma.math.algebra.linear][.]") {
	//TODO we should assert performance gain here using a banded scheme
	//the real problem is that we make too many copies of the buffer,
	//what makes the "fast" algorithm to be much slower!

	auto A = make_toeplitzf(5000, { 1.0f, 2.0f, 3.0f, 3.0f });
	std::cout << "Time: " << measure<>::execution([&]() {
		auto[r, R] = A.getCholeskyFactor();
	}).count() << std::endl;

	auto banded = A.asBanded();
	std::cout << "Time: " << measure<>::execution([&]() {
		auto[r, R] = banded.getCholeskyFactor();
	}).count() << std::endl;
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
		1, 2, 3, 4, 5 });
	auto b = Matrixf<5, 1>::ByRow({ 5, 9, 12, 14, 15 });
	auto expected = Matrixf<5, 1>::ByRow({ 1, 1, 1, 1, 1 });

	A.solve(b, SolveAlgorithm::cholesky);
	assertEqual(expected, b);
}

TEST_CASE("matrix.toeplitz.2.2", "[ma.math.algebra.linear]")
{
	auto A = make_toeplitzf<2>({ 1.0f });
	auto expected = make_col2f({ 1.0f, 0.0f, 0.0f, 1.0f });
	assertEqual(A, expected);
}

TEST_CASE("matrix.toeplitz.4.4", "[ma.math.algebra.linear]")
{
	auto A = make_toeplitzf<4>({ 1.0f, 2.0f, 3.0f, 4.0f, 5.0f, 6.0f, 7.0f });
	auto expected = make_rowf<4, 4>({
		1.0f, 2.0f, 4.0f, 6.0f,
		3.0f, 1.0f, 2.0f, 4.0f,
		5.0f, 3.0f, 1.0f, 2.0f,
		7.0f, 5.0f, 3.0f, 1.0f,
		});
	assertEqual(A, expected);
}