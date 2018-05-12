#include <vector>
#include <functional>
#include <initializer_list>
#include <utility>
#include <tuple>
#include <iostream>

namespace ma
{
	namespace math
	{
		using NaturalNumber = unsigned int;
		namespace algebra
		{
			namespace linear
			{
				using MatrixIndex = unsigned int;
				class NONE {};
				class IDENTITY {};
				class UPPERTRIANGULAR {};
				class LOWERTRIANGULAR {};
				template <typename TNumber, NaturalNumber HEIGHT, NaturalNumber WIDTH, typename TAG = NONE> class Matrix;
				template <NaturalNumber HEIGHT, NaturalNumber WIDTH, typename TAG = NONE> using Matrixf = Matrix<float, HEIGHT, WIDTH, TAG>;
				template <typename TNumber, NaturalNumber HEIGHT> using MatrixIdentity = Matrix<TNumber, HEIGHT, HEIGHT, IDENTITY>;
				template <NaturalNumber HEIGHT> using MatrixIdentityf = Matrix<float, HEIGHT, HEIGHT, IDENTITY>;

				template <typename TNumber, NaturalNumber HEIGHT, typename TAG = NONE> using Vector = Matrix<TNumber, HEIGHT, 1, TAG>;
				template <NaturalNumber HEIGHT, typename TAG = NONE> using Vectorf = Matrix<float, HEIGHT, 1, TAG>;

				enum class SolveAlgorithm
				{
					row_oriented,
					column_oriented,
					cholesky,
					row_backward_substitution
				};

				enum class ByRowTrait
				{
					None,
					UpperTriangular
				};

				template <typename TNumber, NaturalNumber HEIGHT, NaturalNumber WIDTH, typename TAG>
				class Matrix
				{
				public:
					static Matrix<TNumber, HEIGHT, WIDTH, NONE> NonInitialized()
					{
						return {};
					}
					static MatrixIdentity<TNumber, HEIGHT> Identity()
					{
						return {};
					}

					static Matrix<TNumber, HEIGHT, WIDTH, TAG> ByRow(
						const std::initializer_list<TNumber>& l,
						ByRowTrait traits = ByRowTrait::None)
					{
						if (traits == ByRowTrait::None)
						{
							Matrix<TNumber, WIDTH, HEIGHT, TAG> result(l);
							return result.transpose();
						}
						//else if (traits == ByRowTrait::UpperTriangular)
						//{
						//	Matrix<TNumber, HEIGHT, WIDTH, NONE> result;
						//	int n = HEIGHT * WIDTH;
						//	//TODO not ideal
						//	std::vector<TNumber> arr(l.size());
						//	std::copy(l.begin(), l.end(), std::begin(arr));
						//	for (int i = 0; i < HEIGHT; ++i)
						//		for (int j = 0; j < WIDTH; ++j)
						//		{
						//			if (i > j) result.at(i, j) = (TNumber)0;
						//			else
						//			{
						//				int k = (n*(n - 1) / 2) - (n - i)*((n - i) - 1) / 2 + j - i - 1;
						//				result.at(i, j) = arr[k];
						//			}
						//		}
						//}
					}

					static Matrix<TNumber, WIDTH, HEIGHT, UPPERTRIANGULAR> UpperTriangular(
						float * l)
					{
						return { l };
					}

					Matrix(const std::initializer_list<TNumber>& l) {
						std::copy(l.begin(), l.end(), std::begin(Items));
					}

					void map(std::function<void(MatrixIndex i, MatrixIndex j, TNumber)> f) const noexcept
					{
						for (int i = 0; i < HEIGHT; ++i)
						{
							for (int j = 0; j < WIDTH; ++j)
							{
								f(i, j, at(i, j));
							}
						}
					}

					template <typename RTAG>
					void zip(
						const Matrix<TNumber, HEIGHT, WIDTH, RTAG>& m,
						std::function<void(MatrixIndex i, MatrixIndex j, TNumber, TNumber)> f) const noexcept
					{
						for (int i = 0; i < HEIGHT; ++i)
						{
							for (int j = 0; j < WIDTH; ++j)
							{
								if (i == j) f(i, j, at(i, j), m.at(i, j));
								else f(i, j, at(i, j), m.at(i, j));
							}
						}
					}

					//Fast multiplication by Identity
					Matrix<TNumber, HEIGHT, WIDTH, TAG> operator * (const MatrixIdentity<TNumber, WIDTH>&) const noexcept
					{
						return *this;
					}

					//Matrix Vector Multiplicatiom
					//
					//  ----- j ---->
					//	+-         -+  +- -+ |      +- -+  |				
					//	|  A  D  G  |  | A | |      | A |  |
					//	|           |  |   | |      |   |  |
					//	|  B  E  H  |  | B | j  =   | B |  i
					//	|           |  |   | |      |   |  |
					//	|  C  F  I  |  | C | |      | C |  |
					//	+-         -+  +- -+ \/     +- -+  \/
					//
					Matrix<TNumber, HEIGHT, 1, TAG> operator * (
						const Matrix<TNumber, WIDTH, 1>& vector) const noexcept
					{
						Matrix<TNumber, HEIGHT, 1, TAG> result;

						for (int i = 0; i < WIDTH; ++i)
						{
							auto& v = result.at(i, 0);
							v = 0;
							for (int j = 0; j < WIDTH; ++j)
							{
								v += at(i, j)*vector.at(j, 0);
							}
						}

						return result;
					}

					//Matrix Matrix Multiplicatiom
					//
					//  --- j ---->
					//	+-       -+  +-     -+ |      +-       -+  |				
					//	| A  D  G |  | A  D  | |      | A  D  G |  |
					//	|         |  |       | |      |         |  |
					//	| B  E  H |  | B  E  | j  =   | B  E  H |  i
					//	|         |  |       | |      |         |  |
					//	|+-      -+  | C  F  | |      |+-      -+  \/
					//	             +-     -+ \/                  
					//     this          right       =      result
					//     <2,3>         <3,2>       =      <2,2>
					//     <H,W>         <W,RW>      =      <H,RW>
					template <NaturalNumber RWIDTH, typename RTAG>
					Matrix<TNumber, HEIGHT, RWIDTH, NONE> operator * (
						const Matrix<TNumber, WIDTH, RWIDTH, RTAG>& right) const noexcept
					{
						Matrix<TNumber, HEIGHT, RWIDTH, NONE> result;

						for (int i = 0; i < HEIGHT; ++i)
						{
							for (int j = 0; j < RWIDTH; ++j)
							{
								auto& v = result.at(i, j);
								v = 0;
								for (int k = 0; k < WIDTH; ++k)
								{
									v += at(i, k) * right.at(k, j);
								}
							}
						}

						return result;
					}


					// Ax = b
					// x will be returned in b
					// return if computation went ok
					bool solve(
						Vector<TNumber, HEIGHT>& b,
						SolveAlgorithm algo = SolveAlgorithm::row_oriented)
					{
						if (algo == SolveAlgorithm::row_oriented)
						{
							return row_foward_substitution<TNumber, HEIGHT>(
								ColumnOrientedArray<TNumber, HEIGHT>(Items),
								ColumnOrientedArray<TNumber, HEIGHT>(b.Items));
						}
						else if (algo == SolveAlgorithm::column_oriented)
						{
							return column_foward_substitution<TNumber, HEIGHT>(
								ColumnOrientedArray<TNumber, HEIGHT>(Items),
								ColumnOrientedArray<TNumber, HEIGHT>(b.Items));
						}
						else if (algo == SolveAlgorithm::cholesky)
						{
							//TODO it does not return the result
							//create a immutable and inplace version
							auto[r, x] = solve_cholesky(*this, b);
							b = x;
							return r;
						}
						else if (algo == SolveAlgorithm::row_backward_substitution)
						{
							return row_backward_substitution<TNumber, HEIGHT>(
								ColumnOrientedArray<TNumber, HEIGHT>(Items),
								ColumnOrientedArray<TNumber, HEIGHT>(b.Items));
						}
					}

					Matrix<TNumber, WIDTH, HEIGHT, TAG> transpose() const noexcept
					{
						Matrix<TNumber, WIDTH, HEIGHT, TAG> result;
						map([&](auto i, auto j, auto x) {
							result.at(j, i) = x;
						});
						return result;
					}


					bool factorCholesky() noexcept
					{
						if (HEIGHT != WIDTH) return false;

						for (int i = 0; i < HEIGHT; ++i)
						{
							auto& aii = at(i, i);
							for (int k = 0; k < i; ++k)
							{
								auto aki = at(k, i);
								aii -= aki * aki;
							}
							if (aii <= 0) return false;
							//std::cout << "----------------------------" << std::endl;
							//print(std::cout, i, i);
							//std::cout << "............................" << std::endl;
							aii = sqrt(aii); //rii
							//print(std::cout, i, i);
							//std::cout << "----------------------------" << std::endl;
							for (int j = 0; j < i; ++j) at(i, j) = 0.0f;
							for (int j = i + 1; j < HEIGHT; ++j)
							{
								for (int k = 0; k < i; ++k)
								{
									at(i, j) -= at(k, i)*at(k, j);
								}
								//std::cout << "----------------------------" << std::endl;
								//print(std::cout, i, j);
								//std::cout << "............................" << std::endl;
								at(i, j) /= aii;
								//print(std::cout, i, j);
								//std::cout << "---------------------------" << std::endl;
							}
						}

						return true;
					}

					std::tuple<bool, Matrix<TNumber, HEIGHT, HEIGHT, UPPERTRIANGULAR>>
						factorCholeskyErasure() const noexcept
					{
						int n = HEIGHT;
						int Rsize = (((HEIGHT*WIDTH) - HEIGHT) / 2) + HEIGHT;
						auto R = std::vector<TNumber>(Rsize);
						auto get = [&](int i, int j) {
							if (j >= i) {
								int k = i + (j + 1)*j / 2;
								return R[k];
							}
							else
								return at(i, j);
						};

						//std::cout << "Init" << std::endl;
						//std::cout << "--------" << std::endl;
						for (int k = 0; k < Rsize; ++k) {
							int j = (int)((-1 + sqrt(8 * k + 1)) / 2);
							int i = k - j * (j + 1) / 2;
							//print(std::cout, i, j);
							//std::cout << "--------" << std::endl;
							R[k] = at(i, j);
						}
						for (int i = 0; i < HEIGHT; ++i)
						{
							auto aii = at(i, i);
							for (int k = 0; k < i; ++k)
							{
								auto aki = get(k, i);
								aii -= aki * aki;
							}
							if (aii <= 0)
								return std::make_tuple(false,
									Matrix<TNumber, HEIGHT, HEIGHT, UPPERTRIANGULAR>());

							/*std::cout << "----------------------------" << std::endl;
							print(std::cout, i, i);
							std::cout << "............................" << std::endl;*/
							aii = sqrt(aii);
							int k = i + (i + 1)*i / 2;
							R[k] = aii;
							//++ri;
							//print(std::cout, i, i);
							//std::cout << "----------------------------" << std::endl;

							//for (int j = 0; j < i; ++j) at(i, j) = 0.0f;
							for (int j = i + 1; j < HEIGHT; ++j)
							{
								auto aij = at(i, j);
								for (int k = 0; k < i; ++k)
								{
									aij -= get(k, i)*get(k, j);
								}
								/*std::cout << "----------------------------" << std::endl;
								print(std::cout, i, j);
								std::cout << "............................" << std::endl;*/
								aij /= aii;
								k = i + (j + 1)*j / 2;
								R[k] = aij;
								//++ri;
								//print(std::cout, i, j);
								//std::cout << "----------------------------" << std::endl;
							}
						}
						return std::make_tuple(true,
							Matrix<TNumber, HEIGHT, HEIGHT, UPPERTRIANGULAR>{&R[0]});
					}

					std::ostream& print(std::ostream& out, MatrixIndex cx, MatrixIndex cy) const noexcept
					{
						for (int i = 0; i < HEIGHT; i++) {
							for (int j = 0; j < WIDTH; j++) {
								if (i == cx & j == cy) {
									out << "[" << std::setprecision(5) << std::setw(5) << at(i, j) << "] ";
								}
								else {
									out << std::setprecision(5) << std::setw(5) << at(i, j) << " ";
								}
							}
							out << std::endl;
						}
						return out;
					}
					bool isPositiveDefinite() const noexcept
					{
						auto[r, R] = factorCholeskyErasure();
						return r;
					}
				private:
					Matrix()
					{
					}

					TNumber& at(MatrixIndex i, MatrixIndex j) noexcept { return Items[(j*HEIGHT) + i]; }
					TNumber at(MatrixIndex i, MatrixIndex j) const noexcept { return Items[(j*HEIGHT) + i]; }
					TNumber Items[HEIGHT*WIDTH];

					template <typename TNumber2, NaturalNumber HEIGHT2, NaturalNumber WIDTH2, typename TAG2> friend class Matrix;
				};

				template <typename TNumber, typename TAG>
				class DMatrix
				{
				public:
					DMatrix()
					{
					}

					static DMatrix<TNumber, TAG> ByRow(MatrixIndex h, MatrixIndex w,
						const TNumber* data,
						ByRowTrait traits = ByRowTrait::None)
					{
						if (traits == ByRowTrait::None)
						{
							DMatrix<TNumber, TAG> result(h, w, data);
							return result.transpose();
						}
					}

					DMatrix(MatrixIndex h, MatrixIndex w, const TNumber* data)
						: HEIGHT(h), WIDTH(w)
					{
						this->Items = new TNumber[HEIGHT*WIDTH];
						if (data != nullptr)
						{
							std::copy_n(data, w*h, this->Items);
						}
					}

					~DMatrix()
					{
						//TODO
						//delete[] this->Items;
					}

					void map(std::function<void(MatrixIndex i, MatrixIndex j, TNumber)> f) const noexcept
					{
						for (int i = 0; i < HEIGHT; ++i)
						{
							for (int j = 0; j < WIDTH; ++j)
							{
								f(i, j, at(i, j));
							}
						}
					}

					template <typename RTAG>
					void zip(
						const DMatrix<TNumber, RTAG>& m,
						std::function<void(MatrixIndex i, MatrixIndex j, TNumber, TNumber)> f) const noexcept
					{
						for (int i = 0; i < HEIGHT; ++i)
						{
							for (int j = 0; j < WIDTH; ++j)
							{
								if (i == j) f(i, j, at(i, j), m.at(i, j));
								else f(i, j, at(i, j), m.at(i, j));
							}
						}
					}

					DMatrix<TNumber, TAG> transpose() const noexcept
					{
						DMatrix<TNumber, TAG> result(WIDTH, HEIGHT, nullptr);
						map([&](auto i, auto j, auto x) {
							result.at(j, i) = x;
						});
						return result;
					}

					template <typename RTAG>
					DMatrix<TNumber, NONE> operator * (
						const DMatrix<TNumber, RTAG>& right) const noexcept
					{
						DMatrix<TNumber, NONE> result(this->HEIGHT, right.WIDTH, nullptr);

						for (int i = 0; i < HEIGHT; ++i)
						{
							for (int j = 0; j < right.WIDTH; ++j)
							{
								auto& v = result.at(i, j);
								v = 0;
								for (int k = 0; k < WIDTH; ++k)
								{
									v += at(i, k) * right.at(k, j);
								}
							}
						}

						return result;
					}

					bool factorCholesky() noexcept
					{
						if (HEIGHT != WIDTH) return false;
						for (int i = 0; i < HEIGHT; ++i)
						{
							auto& aii = at(i, i);
							for (int k = 0; k < i; ++k)
							{
								auto aki = at(k, i);
								aii -= aki * aki;
							}
							if (aii <= 0) return false;
							aii = sqrt(aii);
							for (int j = 0; j < i; ++j) at(i, j) = 0.0f;
							for (int j = i + 1; j < HEIGHT; ++j)
							{
								for (int k = 0; k < i; ++k)
								{
									at(i, j) -= at(k, i)*at(k, j);
								}
								at(i, j) /= aii;
							}
						}
						return true;
					}

					std::ostream& print(std::ostream& out, MatrixIndex cx, MatrixIndex cy) const noexcept
					{
						for (int i = 0; i < HEIGHT; i++) {
							for (int j = 0; j < WIDTH; j++) {
								if (i == cx & j == cy) {
									out << "[" << std::setprecision(5) << std::setw(5) << at(i, j) << "] ";
								}
								else {
									out << std::setprecision(5) << std::setw(5) << at(i, j) << " ";
								}
							}
							out << std::endl;
						}
						return out;
					}
				private:
					TNumber & at(MatrixIndex i, MatrixIndex j) noexcept { return Items[(j*HEIGHT) + i]; }
					TNumber at(MatrixIndex i, MatrixIndex j) const noexcept { return Items[(j*HEIGHT) + i]; }
					MatrixIndex HEIGHT;
					MatrixIndex WIDTH;
					TNumber* Items;
				};

				template <typename TNumber, NaturalNumber HEIGHT>
				class Matrix<TNumber, HEIGHT, HEIGHT, IDENTITY>
				{
				public:
					Matrix()
					{
					}

					void map(std::function<void(MatrixIndex i, MatrixIndex j, TNumber)> f) const noexcept
					{
						for (int i = 0; i < HEIGHT; ++i)
						{
							for (int j = 0; j < HEIGHT; ++j)
							{
								if (i == j) f(i, j, (TNumber)1);
								else f(i, j, (TNumber)0);
							}
						}
					}

					template<NaturalNumber RWIDTH, typename ANYTAG>
					void zip(
						const Matrix<TNumber, HEIGHT, RWIDTH, ANYTAG>& m,
						std::function<void(MatrixIndex i, MatrixIndex j, TNumber, TNumber)> f) const noexcept
					{
						for (int i = 0; i < HEIGHT; ++i)
						{
							for (int j = 0; j < WIDTH; ++j)
							{
								if (i == j) f(i, j, (TNumber)1, m.at(i, j));
								else f(i, j, (TNumber)0, m.at(i, j));
							}
						}
					}

					template<typename RTNumber, typename RTAG, NaturalNumber RWIDTH>
					Matrix<RTNumber, HEIGHT, RWIDTH, RTAG> operator * (const Matrix<RTNumber, HEIGHT, RWIDTH, RTAG>& r) const noexcept
					{
						return r;
					}
				};

				template <typename TNumber, NaturalNumber HEIGHT>
				class Matrix<TNumber, HEIGHT, HEIGHT, UPPERTRIANGULAR>
				{
				public:
					Matrix() {}
					Matrix(const TNumber* l)
					{
						std::copy_n(l, Size(), std::begin(Items));
					}

					template <typename RTAG>
					void zip(
						const Matrix<TNumber, HEIGHT, HEIGHT, RTAG>& m,
						std::function<void(MatrixIndex i, MatrixIndex j, TNumber, TNumber)> f) const noexcept
					{
						for (int i = 0; i < HEIGHT; ++i)
						{
							for (int j = 0; j < HEIGHT; ++j)
							{
								if (i == j) f(i, j, at(i, j), m.at(i, j));
								else f(i, j, at(i, j), m.at(i, j));
							}
						}
					}

					Matrix<TNumber, HEIGHT, HEIGHT, LOWERTRIANGULAR>
						transpose()
					{
						return Matrix<TNumber, HEIGHT, HEIGHT, LOWERTRIANGULAR>::From(this->Items);
					}

					std::ostream& print(std::ostream& out, MatrixIndex cx, MatrixIndex cy) const noexcept
					{
						for (int i = 0; i < HEIGHT; i++) {
							for (int j = 0; j < HEIGHT; j++) {
								if (i == cx & j == cy) {
									out << "[" << std::setprecision(5) << std::setw(5) << at(i, j) << "] ";
								}
								else {
									out << std::setprecision(5) << std::setw(5) << at(i, j) << " ";
								}
							}
							out << std::endl;
						}
						return out;
					}

					template <typename BTAG>
					std::tuple<bool, Vector<TNumber, HEIGHT, NONE>>
						solve(Vector<TNumber, HEIGHT, BTAG>& b) const noexcept
					{
						auto newA = Matrix<TNumber, HEIGHT, HEIGHT, UPPERTRIANGULAR>(*this);
						auto result = Vector<TNumber, HEIGHT, BTAG>(b);
						auto r = row_backward_substitution<TNumber, HEIGHT>(
							UpperTriangularColumnOrientedArray<TNumber, HEIGHT>(newA.Items),
							ColumnOrientedArray<TNumber, HEIGHT>(result.Items));
						return std::make_tuple(r, result);
					}
				private:
					static unsigned int constexpr Size() { return (((HEIGHT*HEIGHT) - HEIGHT) / 2.0) + HEIGHT; }
					TNumber at(MatrixIndex i, MatrixIndex j) const noexcept
					{
						//upper triangular
						if (i > j) return (TNumber)0;
						else
						{
							//todo why?
							int k = i + (j + 1)*j / 2;
							return Items[k];
						}
					}
					TNumber& at(MatrixIndex i, MatrixIndex j)
					{
						//only allow upper part
						assert(i <= j);

						//todo why?
						int k = i + (j + 1)*j / 2;
						return Items[k];
					}
					TNumber Items[Size()];
				};

				template <typename TNumber, NaturalNumber HEIGHT>
				class Matrix<TNumber, HEIGHT, HEIGHT, LOWERTRIANGULAR>
				{
				public:
					static Matrix<TNumber, HEIGHT, HEIGHT, LOWERTRIANGULAR> From(const TNumber* f)
					{
						Matrix<TNumber, HEIGHT, HEIGHT, LOWERTRIANGULAR> result;
						std::copy_n(f, Size(), result.Items);
						return result;
					}

					template <typename RTAG>
					void zip(
						const Matrix<TNumber, HEIGHT, HEIGHT, RTAG>& m,
						std::function<void(MatrixIndex i, MatrixIndex j, TNumber, TNumber)> f) const noexcept
					{
						for (int i = 0; i < HEIGHT; ++i)
						{
							for (int j = 0; j < HEIGHT; ++j)
							{
								if (i == j) f(i, j, at(i, j), m.at(i, j));
								else f(i, j, at(i, j), m.at(i, j));
							}
						}
					}

					std::ostream& print(std::ostream& out, MatrixIndex cx, MatrixIndex cy) const noexcept
					{
						for (int i = 0; i < HEIGHT; i++) {
							for (int j = 0; j < HEIGHT; j++) {
								if (i == cx & j == cy) {
									out << "[" << std::setprecision(5) << std::setw(5) << at(i, j) << "] ";
								}
								else {
									out << std::setprecision(5) << std::setw(5) << at(i, j) << " ";
								}
							}
							out << std::endl;
						}
						return out;
					}

					template <typename TAG>
					std::tuple<bool, Vector<TNumber, HEIGHT, NONE>>
						solve(const Vector<TNumber, HEIGHT, TAG>& b) const noexcept
					{
						auto newA = Matrix<TNumber, HEIGHT, HEIGHT, LOWERTRIANGULAR>(*this);
						auto result = Vector<TNumber, HEIGHT, TAG>(b);
						auto r = row_foward_substitution<TNumber, HEIGHT>(
							LowerTriangularColumnOrientedArray<TNumber, HEIGHT>(newA.Items),
							ColumnOrientedArray<TNumber, HEIGHT>(result.Items));
						return std::make_tuple(r, result);
					}
				private:
					static unsigned int constexpr Size() { return (((HEIGHT*HEIGHT) - HEIGHT) / 2.0) + HEIGHT; }
					TNumber at(MatrixIndex i, MatrixIndex j) const noexcept
					{
						//lower triangular
						if (i < j) return (TNumber)0;
						else
						{
							//todo why?
							int k = j + (i + 1)*i / 2;
							return Items[k];
						}
					}
					TNumber& at(MatrixIndex i, MatrixIndex j)
					{
						//only allow lower part and diagonal
						assert(i >= j);

						//todo why?
						int k = j + (i + 1)*i / 2;
						return Items[k];
					}
					TNumber Items[Size()];
				};

				template <typename T, NaturalNumber HEIGHT>
				class ColumnOrientedArray
				{
				public:
					ColumnOrientedArray(T* array) : Data(array) {}
					T& at(MatrixIndex i, MatrixIndex j) noexcept { return Data[(j*HEIGHT) + i]; }
					T at(MatrixIndex i, MatrixIndex j) const noexcept { return Data[(j*HEIGHT) + i]; }
				private:
					T * Data;
				};

				template <typename T>
				struct DONOTHING {
					static void print(MatrixIndex cx, MatrixIndex cy, std::function<T(MatrixIndex i, MatrixIndex j)> at) noexcept
					{
					}
				};
				template <typename T, NaturalNumber HEIGHT>
				struct TOSTDCOUT {
					static void print(MatrixIndex cx, MatrixIndex cy, std::function<T(MatrixIndex i, MatrixIndex j)> at) noexcept
					{
						std::cout << "----------------------------" << std::endl;
						for (int i = 0; i < HEIGHT; i++) {
							for (int j = 0; j < HEIGHT; j++) {
								if (i == cx & j == cy) {
									std::cout << "[" << std::setprecision(5) << std::setw(5) << at(i, j) << "] ";
								}
								else {
									std::cout << std::setprecision(5) << std::setw(5) << at(i, j) << " ";
								}
							}
							std::cout << std::endl;
						}
						std::cout << "----------------------------" << std::endl;
					}
				};

				template <typename T, NaturalNumber HEIGHT, typename PRINT = DONOTHING<T>>
				class LowerTriangularColumnOrientedArray :
					private PRINT
				{
				public:
					static unsigned int constexpr getIndex(MatrixIndex i, MatrixIndex j) noexcept { return j + (i + 1)*i / 2; }

					LowerTriangularColumnOrientedArray(T* array) : Data(array) {}
					T at(MatrixIndex i, MatrixIndex j) const noexcept
					{
						PRINT::print(i, j, [&](auto i, auto j) { return Data[getIndex(i, j)]; });

						//lower triangular
						if (i < j) return (TNumber)0;
						else return Data[getK()];
					}
					T& at(MatrixIndex i, MatrixIndex j)
					{
						PRINT::print(i, j, [&](auto i, auto j) { return Data[getIndex(i, j)]; });

						//only allow lower part and diagonal
						assert(i >= j);

						//todo why?
						return Data[getIndex(i, j)];
					}
				private:
					T * Data;
				};

				template <typename T, NaturalNumber HEIGHT, typename PRINT = DONOTHING<T>>
				class UpperTriangularColumnOrientedArray :
					private PRINT
				{
				public:
					static unsigned int constexpr getIndex(MatrixIndex i, MatrixIndex j) noexcept { return i + (j + 1)*j / 2; }

					UpperTriangularColumnOrientedArray(T* array) : Data(array) {}
					T at(MatrixIndex i, MatrixIndex j) const noexcept
					{
						PRINT::print(i, j, [&](auto i, auto j) { return Data[getIndex(i, j)]; });

						//upper triangular
						if (i > j) return (TNumber)0;
						else return Data[getK()];
					}
					T& at(MatrixIndex i, MatrixIndex j)
					{
						PRINT::print(i, j, [&](auto i, auto j) { return Data[getIndex(i, j)]; });

						//only allow upper part and diagonal
						assert(i <= j);

						//todo why?
						return Data[getIndex(i, j)];
					}
				private:
					T * Data;
				};

				template <typename T, NaturalNumber HEIGHT, typename ARRAY1, typename ARRAY2>
				bool row_foward_substitution(ARRAY1& A, ARRAY2& b)
				{
					int i = 0;
					for (; i < HEIGHT; ++i)
					{
						if (b.at(i, 0) != 0) break;
					}
					for (; i < HEIGHT; ++i)
					{
						for (int j = 0; j < i; ++j)
						{
							auto x = A.at(i, j)*b.at(j, 0);
							b.at(i, 0) -= x;
						}

						if (A.at(i, i) == 0) return false;
						b.at(i, 0) /= A.at(i, i);
					}

					return true;
				}

				template <typename T, NaturalNumber HEIGHT>
				bool column_foward_substitution(
					ColumnOrientedArray<T, HEIGHT> A,
					ColumnOrientedArray<T, HEIGHT> b)
				{
					int j = 0;
					for (; j < HEIGHT; ++j)
					{
						if (b.at(j, 0) != 0) break;
					}
					for (; j < HEIGHT; ++j)
					{
						if (A.at(j, j) == 0) return false;
						b.at(j, 0) /= A.at(j, j);
						for (int i = j + 1; i < HEIGHT; ++i)
						{
							b.at(i, 0) -= A.at(i, j)*b.at(j, 0);
						}
					}

					return true;
				}

				template <typename T, NaturalNumber HEIGHT, typename ARRAY1, typename ARRAY2>
				bool row_backward_substitution(ARRAY1& A, ARRAY2& b)
				{
					int i = HEIGHT - 1;
					for (; i >= 0; --i)
					{
						if (b.at(i, 0) != 0) break;
					}
					for (; i >= 0; --i)
					{
						for (int j = HEIGHT - 1; j > i; --j)
						{
							auto x = A.at(i, j)*b.at(j, 0);
							b.at(i, 0) -= x;
						}

						if (A.at(i, i) == 0) return false;
						b.at(i, 0) /= A.at(i, i);
					}

					return true;
				}

				//A*x=b
				//A = R^T*R;
				//R^T*R*x=b;
				//R*x=y;
				//R^T*y=b; <- R^T is Lower Triangular Matrix (Foward Substitution)
				//R*x = y; <- R is Upper Triangular Matrix (Backward Substitution)
				template <typename T, NaturalNumber HEIGHT, typename ATAG, typename BTAG>
				std::tuple<bool, Vector<T, HEIGHT, NONE>>
					solve_cholesky(
						const Matrix<T, HEIGHT, HEIGHT, ATAG>& A,
						const Vector<T, HEIGHT, BTAG>& b)
				{
					auto[r1, R] = A.factorCholeskyErasure();
					if (!r1) return std::make_tuple(false, Vector<T, HEIGHT, NONE>::NonInitialized());

					auto RT = R.transpose();
					auto[r2, y] = RT.solve(b);
					if (!r2) return std::make_tuple(false, Vector<T, HEIGHT, NONE>::NonInitialized());

					auto[r3, x] = R.solve(y);
					if (!r3) return std::make_tuple(false, Vector<T, HEIGHT, NONE>::NonInitialized());

					return std::make_tuple(true, x);
				}
			}
		}
	}
}