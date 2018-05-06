#include <functional>
#include <initializer_list>
#include <utility>
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
				class CHOLESKYFACTOR {};
				template <typename TNumber, NaturalNumber HEIGHT, NaturalNumber WIDTH, typename TAG = NONE> class Matrix;
				template <NaturalNumber HEIGHT, NaturalNumber WIDTH, typename TAG = NONE> using Matrixf = Matrix<float, HEIGHT, WIDTH, TAG>;
				template <typename TNumber, NaturalNumber HEIGHT> using MatrixIdentity = Matrix<TNumber, HEIGHT, HEIGHT, IDENTITY>;
				template <NaturalNumber HEIGHT> using MatrixIdentityf = Matrix<float, HEIGHT, HEIGHT, IDENTITY>;

				template <typename TNumber, NaturalNumber HEIGHT, typename TAG = NONE> using Vector = Matrix<TNumber, HEIGHT, 1, TAG>;
				template <NaturalNumber HEIGHT, typename TAG = NONE> using Vectorf = Matrix<float, HEIGHT, 1, TAG>;

				enum class SolveAlgorithm
				{
					row_oriented,
					column_oriented
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
					static MatrixIdentity<TNumber, HEIGHT> Identity()
					{
						return {};
					}

					static Matrix<TNumber, WIDTH, HEIGHT, TAG> ByRow(
						const std::initializer_list<TNumber>& l,
						ByRowTrait traits = ByRowTrait::None)
					{
						if (traits == ByRowTrait::None)
						{
							Matrix<TNumber, HEIGHT, WIDTH, TAG> result(l);
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

					static Matrix<TNumber, WIDTH, HEIGHT, TAG> UpperTriangular(
						float * l)
					{
						//if (traits == ByRow::None)
						//{
						//	Matrix<TNumber, HEIGHT, WIDTH, TAG> result(l);
						//	return result.transpose();
						//}
						//else
						//if (traits == ByRowTrait::UpperTriangular)
						//{
						//std::cout << "Upper Triangular" << std::endl;
						int n = HEIGHT * WIDTH;
						Matrix<TNumber, HEIGHT, WIDTH, NONE> result;
						for (int i = 0; i < HEIGHT; ++i)
						{
							for (int j = 0; j < WIDTH; ++j)
							{
								if (i > j) result.at(i, j) = (TNumber)0;
								else
								{
									int k = i + (j + 1)*j / 2;

									//std::cout << "k = " << k << std::endl;
									//std::cout << "----------------------------" << std::endl;
									//result.print(std::cout, i, j);
									//std::cout << "............................" << std::endl;

									result.at(i, j) = l[k];

									//result.print(std::cout, i, j);
									//std::cout << "----------------------------" << std::endl;
								}
							}
						}
						return result;
						//}
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
							std::cout << "----------------------------" << std::endl;
							print(std::cout, i, i);
							std::cout << "............................" << std::endl;
							aii = sqrt(aii); //rii
							print(std::cout, i, i);
							std::cout << "----------------------------" << std::endl;
							for (int j = 0; j < i; ++j) at(i, j) = 0.0f;
							for (int j = i + 1; j < HEIGHT; ++j)
							{
								for (int k = 0; k < i; ++k)
								{
									at(i, j) -= at(k, i)*at(k, j);
								}
								std::cout << "----------------------------" << std::endl;
								print(std::cout, i, j);
								std::cout << "............................" << std::endl;
								at(i, j) /= aii;
								print(std::cout, i, j);
								std::cout << "---------------------------" << std::endl;
							}
						}

						return true;
					}

					bool factorCholeskyErasure(float*& R) const noexcept
					{
						int n = HEIGHT;
						auto get = [&](int i, int j) {
							if (j >= i) {
								int k = i + (j + 1)*j / 2;
								return R[k];
							}
							else 
							return at(i, j);
						};
						int Rsize = (((HEIGHT*WIDTH) - HEIGHT) / 2) + HEIGHT;
						R = new float[Rsize];
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
							if (aii <= 0) return false;

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
					Matrix()
					{
					}

					TNumber& at(MatrixIndex i, MatrixIndex j) noexcept { return Items[(j*HEIGHT) + i]; }
					TNumber at(MatrixIndex i, MatrixIndex j) const noexcept { return Items[(j*HEIGHT) + i]; }
					TNumber Items[HEIGHT*WIDTH];

					template <typename TNumber2, NaturalNumber HEIGHT2, NaturalNumber WIDTH2, typename TAG2> friend class Matrix;
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




				template <typename T, NaturalNumber HEIGHT>
				class ColumnOrientedArray
				{
				public:
					ColumnOrientedArray(T* array) : Data(array)
					{
					}
					T& at(MatrixIndex i, MatrixIndex j) noexcept { return Data[(j*HEIGHT) + i]; }
					T at(MatrixIndex i, MatrixIndex j) const noexcept { return Data[(j*HEIGHT) + i]; }
				private:
					T * Data;
				};

				template <typename T, NaturalNumber HEIGHT>
				bool row_foward_substitution(
					ColumnOrientedArray<T, HEIGHT> A,
					ColumnOrientedArray<T, HEIGHT> b)
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
			}
		}
	}
}