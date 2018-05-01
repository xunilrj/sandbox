#include <functional>
#include <initializer_list>

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
				template <typename TNumber, NaturalNumber HEIGHT, NaturalNumber WIDTH, typename TAG = NONE> class Matrix;
				template <NaturalNumber HEIGHT, NaturalNumber WIDTH, typename TAG = NONE> using Matrixf = Matrix<float, HEIGHT, WIDTH, TAG>;
				template <typename TNumber, NaturalNumber HEIGHT> using MatrixIdentity = Matrix<TNumber, HEIGHT, HEIGHT, IDENTITY>;
				template <NaturalNumber HEIGHT> using MatrixIdentityf = Matrix<float, HEIGHT, HEIGHT, IDENTITY>;

				template <typename TNumber, NaturalNumber HEIGHT, typename TAG = NONE> using Vector = Matrix<TNumber, HEIGHT, 1, TAG>;
				template <NaturalNumber HEIGHT, typename TAG = NONE> using Vectorf = Matrix<float, HEIGHT, 1, TAG>;

				template <typename TNumber, NaturalNumber HEIGHT, NaturalNumber WIDTH, typename TAG>
				class Matrix
				{
				public:
					static MatrixIdentity<TNumber, HEIGHT> Identity()
					{
						return {};
					}
					
					static Matrix<TNumber, WIDTH, HEIGHT, TAG> ByRow(const std::initializer_list<TNumber>& l)
					{
						Matrix<TNumber, HEIGHT, WIDTH, TAG> result(l);
						return result.transpose();
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
					bool solve(Vector<TNumber, HEIGHT>& b)
					{
						for (int i = 0; i < HEIGHT; ++i)
						{
							for (int j = 0; j < i; ++j)
							{
								auto x = at(i, j)*b.at(j, 0);
								b.at(i, 0) -= x;
							}

							if (at(i, i) == 0) return false;
							b.at(i, 0) /= at(i, i);
						}

						return true;
					}

					Matrix<TNumber, WIDTH, HEIGHT, TAG> transpose() const noexcept
					{
						Matrix<TNumber, WIDTH, HEIGHT, TAG> result;
						map([&](auto i, auto j, auto x) {
							result.at(j, i) = x;
						});
						return result;
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


			}
		}
	}
}