#include <vector>
#include <functional>
#include <initializer_list>
#include <utility>
#include <tuple>
#include <iostream>
#include <type_traits>

#define TUPLE2(A,B) template <std::size_t N> \
decltype(auto) get() const \
{ \
if constexpr (N == 0) return A; \
else if constexpr (N == 1) return B; \
}

#define LAMBDA(i) [&](auto i) -> auto

namespace ma
{
	template<typename T>
	class task
	{
	public:
		task() : failed(true), Value() {}
		task(const T& value) : failed(false), Value(value) {}
		task(bool failed, const T& value) : failed(failed), Value(value) {}

		TUPLE2(failed, Value);
	private:
		bool failed;
		T Value;
	};
}
namespace std {
	template<typename T> struct tuple_size<ma::task<T>> : std::integral_constant<std::size_t, 2> {};
	template<std::size_t N, typename T>
	struct tuple_element<N, ma::task<T>> { using type = decltype(std::declval<ma::task<T>>().get<N>()); };
}
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

				// SPECIAL MATRIX TRAIT
				class NONE {};
				class IDENTITY {};
				class UPPER {};
				class UPPERBANDED {};
				class LOWER {};

				// PRINT TRAITS
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

				//STORAGE
				template <typename T>
				class IdentityStorage
				{
				public:
					IdentityStorage() {}
					T at(MatrixIndex i, MatrixIndex j) const noexcept { return i == j ? (T)1 : (T)0; }
					//TODO HOW TO REMOVE THIS
					T& at(MatrixIndex i, MatrixIndex j) noexcept { T r; return r; }
				};

				template <typename T, NaturalNumber HEIGHT, NaturalNumber WIDTH>
				class ColumnOrientedArray
				{
				public:
					static int constexpr Size() noexcept { return HEIGHT * WIDTH; }
					static ColumnOrientedArray<T, HEIGHT, WIDTH> Zeros()
					{
						ColumnOrientedArray<T, HEIGHT, WIDTH> array;
						array.set(0);
						return array;
					}

					ColumnOrientedArray() {}
					ColumnOrientedArray(const std::initializer_list<T>& l)
					{
						std::copy_n(l.begin(), Size(), Data);
					}
					ColumnOrientedArray(T* array)
					{
						std::copy_n(array, Size(), Data);
					}
					ColumnOrientedArray(const std::vector<T>& array)
					{
						std::copy_n(array.begin(), Size(), Data);
					}
					template <typename TARRAY>
					ColumnOrientedArray(const TARRAY& array)
					{
						array.map([&](auto i, auto j, auto value) {
							at(i, j) = value;
						});
					}

					void map(std::function<void(MatrixIndex i, MatrixIndex j, T)> f) const noexcept
					{
						for (int i = 0; i < H; ++i)
						{
							for (int j = 0; j < W; ++j)
							{
								f(i, j, at(i, j));
							}
						}
					}
					void mapUpper(std::function<void(MatrixIndex i, MatrixIndex j, T, bool)> f) const noexcept
					{
						bool first;
						for (int j = 0; j < WIDTH; ++j)
						{
							first = true;
							for (int i = 0; i < HEIGHT; ++i)
							{
								if (j >= i)
								{
									f(i, j, at(i, j), first);
									first = false;
								}
							}
						}
					}

					void set(T value)
					{
						std::fill_n(Data, Size(), value);
					}
					void setDiagonal(int d, T value) noexcept
					{
						int i = 0, j = 0;
						if (d > 0) {
							i = 0; j = d;
						}
						else {
							i = -d, j = 0;
						}

						while (true)
						{
							if (i >= HEIGHT) return;
							if (j >= WIDTH) return;

							at(i, j) = value;

							++i; ++j;
						}
					}
					void setLower(T value)
					{
						for (int j = 0; j < WIDTH; ++j)
							for (int i = j + 1; i < HEIGHT; ++i)
								at(i, j) = value;
					}
					void mapCholeskyK(MatrixIndex i, std::function<void(MatrixIndex)> f) const noexcept
					{
						for (MatrixIndex k = 0; k < i; ++k)
						{
							f(k);
						}
					}
					void mapCholeskyJ(MatrixIndex i, std::function<void(MatrixIndex)> f) const noexcept
					{
						for (int j = i + 1; j < HEIGHT; ++j)
						{
							f(j);
						}
					}

					T& at(MatrixIndex i, MatrixIndex j) noexcept { return Data[(j*HEIGHT) + i]; }
					T at(MatrixIndex i, MatrixIndex j) const noexcept { return Data[(j*HEIGHT) + i]; }

					NaturalNumber height() const noexcept { return HEIGHT; }
					NaturalNumber width() const noexcept { return WIDTH; }
				protected:
					T Data[Size()];
				};

				template <typename T, NaturalNumber HEIGHT, NaturalNumber WIDTH>
				class RowOrientedArray
				{
				public:
					RowOrientedArray() {}
					RowOrientedArray(const std::initializer_list<T>& l)
					{
						std::copy_n(l.begin(), Size(), Data);
					}
					RowOrientedArray(T* array)
					{
						std::copy_n(array, Size(), Data);
					}

					ColumnOrientedArray<T, WIDTH, HEIGHT>
						transpose() const noexcept
					{
						ColumnOrientedArray<T, WIDTH, HEIGHT> result;
						map([&](auto i, auto j, auto value) {
							result.at(i, j) = value;
						});
						return result;
					}

					void map(std::function<void(MatrixIndex i, MatrixIndex j, T)> f) const noexcept
					{
						for (int j = 0; j < WIDTH; ++j)
						{
							for (int i = 0; i < HEIGHT; ++i)
							{
								f(i, j, at(i, j));
							}
						}
					}
					void mapUpper(std::function<void(MatrixIndex i, MatrixIndex j, T, bool)> f) const noexcept
					{
						bool first;
						for (int j = 0; j < WIDTH; ++j)
						{
							first = true;
							for (int i = 0; i < HEIGHT; ++i)
							{
								if (j >= i)
								{
									f(i, j, at(i, j), first);
									first = false;
								}
							}
						}
					}

					T& at(MatrixIndex i, MatrixIndex j) noexcept { return Data[(i*WIDTH) + j]; }
					T at(MatrixIndex i, MatrixIndex j) const noexcept { return Data[(i*WIDTH) + j]; }
					static int constexpr Size() noexcept { return HEIGHT * WIDTH; }
				protected:
					T Data[Size()];
				};

				template <typename T, NaturalNumber HEIGHT, typename PRINT = DONOTHING<T>>
				class LowerTriangularColumnOrientedArray :
					private PRINT
				{
				public:
					static unsigned int constexpr Size() { return (((HEIGHT*HEIGHT) - HEIGHT) / 2) + HEIGHT; }
					static unsigned int constexpr getIndex(MatrixIndex i, MatrixIndex j) noexcept { return j + (i + 1)*i / 2; }

					LowerTriangularColumnOrientedArray(const T* array)
					{
						std::copy_n(array, Size(), Data);
					}
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
					T Data[Size()];
				};

				template <typename T, NaturalNumber HEIGHT, typename PRINT = DONOTHING<T>>
				class UpperTriangularColumnOrientedArray :
					private PRINT
				{
				public:
					static unsigned int constexpr Size() { return (((HEIGHT*HEIGHT) - HEIGHT) / 2) + HEIGHT; }
					static unsigned int constexpr getIndex(MatrixIndex i, MatrixIndex j) noexcept { return i + (j + 1)*j / 2; }

					UpperTriangularColumnOrientedArray()
					{
					}
					UpperTriangularColumnOrientedArray(const T* array)
					{
						std::copy_n(array, Size(), Data);
					}
					template<typename TArray>
					UpperTriangularColumnOrientedArray(const TArray& array)
					{
						array.mapUpper([&](auto i, auto j, auto v, bool first) {
							at(i, j) = v;
						});
					}

					void map(std::function<void(MatrixIndex, MatrixIndex, T)> f) const noexcept
					{
						for (int i = 0; i < HEIGHT; ++i)
						{
							for (int j = 0; j < HEIGHT; ++j)
							{
								if (i >= j) f(i, j, 0);
								else f(i, j, at(i, j));
							}
						}
					}
					void mapUpper(std::function<void(MatrixIndex, MatrixIndex, T, bool)> f) const noexcept
					{
						for (int j = 0; j < HEIGHT; ++j)
						{
							bool newCol = true;
							for (int i = 0; i < HEIGHT; ++i)
							{
								if (i <= j)
								{
									f(i, j, at(i, j), newCol);
									newCol = false;
								}
							}
						}
					}

					void setLower(T value) { assert(value == 0); }
					LowerTriangularColumnOrientedArray<T, HEIGHT, PRINT> toLower() const noexcept
					{
						return { Data };
					}

					T at(MatrixIndex i, MatrixIndex j) const noexcept
					{
						PRINT::print(i, j, [&](auto i, auto j) { return Data[getIndex(i, j)]; });

						//upper triangular
						if (i > j) return (T)0;
						else return Data[getIndex(i, j)];
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
					T Data[Size()];
				};

				//As seen in:
				//Computer Solution of Large Sparse Positive Definite
				//https://dl.acm.org/citation.cfm?id=578296
				//NOT TRULY BANDED YET. JUST UPPER PART
				template <typename T>
				class BandedSchemeArray
				{
				public:
					BandedSchemeArray(const std::initializer_list<T>& list) :
						BandedSchemeArray(DRowOrientedArray<T>(list))
					{
					}

					BandedSchemeArray(NaturalNumber h,
						NaturalNumber w,
						const std::initializer_list<T>& list) :
						BandedSchemeArray(DRowOrientedArray<T>{h, w, list})
					{
					}

					template<typename TARRAY>
					BandedSchemeArray(const TARRAY& array)
					{
						std::vector<T> data;
						std::vector<size_t> indices;
						indices.push_back(0);

						Height = array.height();
						Width = array.width();
						Diagonal = new float[array.height()];

						array.mapUpper([&](auto i, auto j, auto value, auto newCol) {
							if (i == j)
							{
								indices.push_back(data.size());
								Diagonal[i] = value;
								return;
							}
							if (value == 0) return;

							data.push_back(value);
						});

						Envelope = new T[data.size()];
						std::copy_n(data.begin(), data.size(), Envelope);
						Indices = new size_t[indices.size()];
						std::copy_n(indices.begin(), indices.size(), Indices);
					}

					MatrixIndex getLowestI(MatrixIndex j) const noexcept
					{
						return (MatrixIndex)(j - (Indices[j + 1] - Indices[j]));
					}
					size_t getIndex(MatrixIndex i, MatrixIndex j) const noexcept
					{
						if (i >= j) return -1;

						auto ij = Indices[j];
						auto jsize = Indices[j + 1] - ij;
						auto lowesti = j - jsize;

						if (i < lowesti) return -1;

						return ij + (i - lowesti);
					}

					void map(std::function<void(MatrixIndex i, MatrixIndex j, T)> f) const noexcept
					{
						for (NaturalNumber i = 0; i < Height; ++i)
						{
							for (NaturalNumber j = 0; j < Width; ++j)
							{
								f(i, j, at(i, j));
							}
						}
					}
					void mapUpper(std::function<void(MatrixIndex i, MatrixIndex j, T, bool)> f) const noexcept
					{
						bool first;
						for (MatrixIndex i = 0; i < Height; ++i)
						{
							first = true;
							for (MatrixIndex j = 0; j < Width; ++j)
							{
								if (i == j)
								{
									f(i, j, Diagonal[i], first);
									first = false;
								}
								else if (j >= i)
								{
									auto k = getIndex(i, j);
									if (k == -1) f(i, j, (T)0, first);
									else f(i, j, Envelope[k], first);

									first = false;
								}
							}
						}
					}

					void setLower(T value) noexcept { }
					void mapCholeskyK(MatrixIndex i, std::function<void(MatrixIndex)> f) const noexcept
					{
						for (MatrixIndex k = getLowestI(i); k < i; ++k)
						{
							f(k);
						}
					}
					void mapCholeskyJ(MatrixIndex i, std::function<void(MatrixIndex)> f) const noexcept
					{
						for (MatrixIndex j = i + 1; j < Height; ++j)
						{
							f(j);
						}
					}

					//TODO HOW TO REMOVE THIS
					T& at(MatrixIndex i, MatrixIndex j) noexcept
					{
						assert(i <= j);
						if (i == j) return Diagonal[j];
						auto index = getIndex(i, j);
						assert(index >= 0);
						return Envelope[index];
					}
					T at(MatrixIndex i, MatrixIndex j) const noexcept
					{
						if (i > j) return 0;
						if (i == j) return Diagonal[j];

						auto index = getIndex(i, j);
						if (index == -1) return 0;
						return Envelope[index];
					}

					MatrixIndex height() const noexcept { return Height; }
					MatrixIndex width() const noexcept { return Width; }
				private:
					MatrixIndex Height;
					MatrixIndex Width;
					T * Diagonal;
					T * Envelope;
					size_t * Indices;
				};

				template<typename T,
					typename LOWER,
					typename DIAGONAL,
					typename UPPER>
					class PassThroughArray
				{
				public:
					PassThroughArray(LOWER* lower, DIAGONAL* diagonal, UPPER* upper) :
						Lower(lower),
						Diagonal(diagonal),
						Upper(upper)
					{
					}

					T at(MatrixIndex i, MatrixIndex j) const noexcept
					{
						if (i == j) return Diagonal->at(i, j);
						else if (i > j) return Lower->at(i, j);
						else return Upper->at(i, j);
					}

					T& at(MatrixIndex i, MatrixIndex j) noexcept
					{
						if (i == j) return Diagonal->at(i, j);
						else if (i > j) return Lower->at(i, j);
						else return Upper->at(i, j);
					}
				private:
					LOWER * Lower;
					DIAGONAL* Diagonal;
					UPPER* Upper;
				};

				template<typename T, typename L, typename D, typename U>
				PassThroughArray<T, L, D, U> make_passthrough(L& l, D& d, U& u)
				{
					return { &l, &d, &u };
				}

				template <typename T>
				class DColumnOrientedArray
				{
				public:
					static DColumnOrientedArray<T> Zeros(NaturalNumber height, NaturalNumber width)
					{
						DColumnOrientedArray<T> array(height, width);
						array.set(0);
						return array;
					}

					DColumnOrientedArray() {}

					template <typename TSOURCE>
					DColumnOrientedArray(NaturalNumber height,
						NaturalNumber width,
						const std::initializer_list<T>& data = nullptr)
						: Height(height), Width(width), Data(Size())
					{
						if (data != nullptr)
						{
							std::copy_n(std::begin(data), Size(), Data.begin());
						}
					}

					DColumnOrientedArray(NaturalNumber height,
						NaturalNumber width,
						const T* data = nullptr)
						: Height(height), Width(width), Data(Size())
					{
						if (data != nullptr)
						{
							std::copy_n(data, Size(), Data.begin());
						}
					}

					template <typename TARRAY>
					DColumnOrientedArray(const TARRAY& array)
						: Height(array.height()), Width(array.width()), Data(Size())
					{
						array.map([&](auto i, auto j, auto value) {
							at(i, j) = value;
						});
					}

					void map(std::function<void(MatrixIndex i, MatrixIndex j, T)> f) const noexcept
					{
						for (NaturalNumber i = 0; i < Height; ++i)
						{
							for (NaturalNumber j = 0; j < Width; ++j)
							{
								f(i, j, at(i, j));
							}
						}
					}
					void mapUpper(std::function<void(MatrixIndex i, MatrixIndex j, T, bool)> f) const noexcept
					{
						bool first;
						for (MatrixIndex j = 0; j < Width; ++j)
						{
							first = true;
							for (MatrixIndex i = 0; i < Height; ++i)
							{
								if (j >= i)
								{
									f(i, j, at(i, j), first);
									first = false;
								}
							}
						}
					}

					void set(T value)
					{
						std::fill_n(Data.begin(), Size(), value);
					}
					void setDiagonal(int d, T value) noexcept
					{
						MatrixIndex i = 0, j = 0;
						if (d > 0) {
							i = 0; j = d;
						}
						else {
							i = -d, j = 0;
						}

						while (true)
						{
							if (i >= Height) return;
							if (j >= Width) return;

							at(i, j) = value;

							++i; ++j;
						}
					}
					void setLower(T value)
					{
						for (MatrixIndex j = 0; j < Width; ++j)
							for (MatrixIndex i = j + 1; i < Height; ++i)
								at(i, j) = value;
					}
					void mapCholeskyK(MatrixIndex i, std::function<void(MatrixIndex)> f) const noexcept
					{
						for (MatrixIndex k = 0; k < i; ++k)
						{
							f(k);
						}
					}
					void mapCholeskyJ(MatrixIndex i, std::function<void(MatrixIndex)> f) const noexcept
					{
						for (MatrixIndex j = i + 1; j < Height; ++j)
						{
							f(j);
						}
					}

					T& at(MatrixIndex i, MatrixIndex j) noexcept { return Data[(j*Height) + i]; }
					T at(MatrixIndex i, MatrixIndex j) const noexcept { return Data[(j*Height) + i]; }

					int Size() noexcept { return Height * Width; }
					MatrixIndex height() const noexcept { return Height; }
					MatrixIndex width() const noexcept { return Width; }
				protected:
					MatrixIndex Height;
					MatrixIndex Width;
					std::vector<T> Data;
				};

				template <typename T>
				class DRowOrientedArray
				{
				public:
					static DRowOrientedArray<T> Zeros(NaturalNumber height, NaturalNumber width)
					{
						DRowOrientedArray<T> array(height, width);
						array.set(0);
						return array;
					}

					DRowOrientedArray() {}

					DRowOrientedArray(const std::initializer_list<T>& data)
					{
						std::copy(std::begin(data), std::end(data), std::back_inserter(Data));
						//TODO check is exactly square
						Height = Width = (NaturalNumber)sqrt(Data.size());
					}

					template <typename TSOURCE>
					DRowOrientedArray(NaturalNumber height,
						NaturalNumber width,
						const TSOURCE& data = nullptr)
						: Height(height), Width(width), Data(Size())
					{
						std::copy_n(std::begin(data), Size(), Data.begin());
					}

					DRowOrientedArray(NaturalNumber height, NaturalNumber width, const T* data = nullptr)
						: Height(height), Width(width), Data(Size())
					{
						if (data != nullptr)
						{
							std::copy_n(std::begin(data), Size(), Data.begin());
						}
					}

					template <typename TARRAY>
					DRowOrientedArray(const TARRAY& array)
						: Data(Size(array.height(), array.width()))
					{
						array.map([&](auto i, auto j, auto value) {
							at(i, j) = value;
						});
					}

					void map(std::function<void(MatrixIndex i, MatrixIndex j, T)> f) const noexcept
					{
						for (int i = 0; i < Height; ++i)
						{
							for (int j = 0; j < Width; ++j)
							{
								f(i, j, at(i, j));
							}
						}
					}
					void mapUpper(std::function<void(MatrixIndex i, MatrixIndex j, T, bool)> f) const noexcept
					{
						bool first;
						for (NaturalNumber j = 0; j < Width; ++j)
						{
							first = true;
							for (NaturalNumber i = 0; i < Height; ++i)
							{
								if (j >= i)
								{
									f(i, j, at(i, j), first);
									first = false;
								}
							}
						}
					}

					void set(T value)
					{
						std::fill_n(Data.begin(), Size(), value);
					}
					void setDiagonal(int d, T value) noexcept
					{
						int i = 0, j = 0;
						if (d > 0) {
							i = 0; j = d;
						}
						else {
							i = -d, j = 0;
						}

						while (true)
						{
							if (i >= Height) return;
							if (j >= Width) return;

							at(i, j) = value;

							++i; ++j;
						}
					}
					void setLower(T value)
					{
						for (int j = 0; j < Width; ++j)
							for (int i = j + 1; i < Height; ++i)
								at(i, j) = value;
					}
					void mapCholeskyK(MatrixIndex i, std::function<void(MatrixIndex)> f) const noexcept
					{
						for (int k = 0; k < i; ++k)
						{
							f(k);
						}
					}
					void mapCholeskyJ(MatrixIndex i, std::function<void(MatrixIndex)> f) const noexcept
					{
						for (int j = i + 1; j < Height; ++j)
						{
							f(j);
						}
					}

					T& at(MatrixIndex i, MatrixIndex j) noexcept { return Data[(i*Height) + j]; }
					T at(MatrixIndex i, MatrixIndex j) const noexcept { return Data[(i*Height) + j]; }

					int Size() noexcept { return Height * Width; }
					NaturalNumber height() const noexcept { return Height; }
					NaturalNumber width() const noexcept { return Width; }
				protected:
					NaturalNumber Height;
					NaturalNumber Width;
					std::vector<T> Data;
				};

				// ALIASES
				template <typename TNumber, NaturalNumber HEIGHT, NaturalNumber WIDTH, typename TAG = NONE>
				class Matrix;
				template <NaturalNumber HEIGHT, NaturalNumber WIDTH, typename TAG = NONE>
				using Matrixf = Matrix<float, HEIGHT, WIDTH, TAG>;
				template <typename TNumber, NaturalNumber SIZE>
				using MatrixIdentity = Matrix<TNumber, SIZE, SIZE, IDENTITY>;
				template <NaturalNumber SIZE>
				using MatrixIdentityf = Matrix<float, SIZE, SIZE, IDENTITY>;
				template <typename TNumber, NaturalNumber SIZE, typename TAG = NONE>
				using Vector = Matrix<TNumber, SIZE, 1, TAG>;
				template <NaturalNumber SIZE, typename TAG = NONE>
				using Vectorf = Matrix<float, SIZE, 1, TAG>;
				template <typename TNumber, NaturalNumber HEIGHT>
				using LowerMatrix = Matrix<TNumber, HEIGHT, HEIGHT, LOWER>;
				template <typename TNumber, NaturalNumber HEIGHT>
				using UpperMatrix = Matrix<TNumber, HEIGHT, HEIGHT, UPPER>;
				template <typename TNumber, NaturalNumber HEIGHT>
				using UpperBandedMatrix = Matrix<TNumber, HEIGHT, HEIGHT, UPPERBANDED>;

				enum class SolveAlgorithm
				{
					forward_row_oriented,
					forward_column_oriented,
					backward_row_substitution,
					cholesky,
				};

				enum class InverseAlgorithm
				{
					cholesky
				};

				enum class ByRowTrait
				{
					None,
					UpperTriangular
				};

				template <typename T,
					NaturalNumber H,
					NaturalNumber W,
					typename STORAGE>
					class BaseMatrix
				{
				public:
					using TSTORAGE = STORAGE;

					BaseMatrix() {}
					BaseMatrix(const std::initializer_list<T>& data) : Data(data) {}
					BaseMatrix(const T* data) : Data(data) {}
					BaseMatrix(const STORAGE& data) : Data(data) { }

					void map(std::function<void(MatrixIndex i, MatrixIndex j, T)> f) const noexcept
					{
						for (int i = 0; i < H; ++i)
						{
							for (int j = 0; j < W; ++j)
							{
								f(i, j, at(i, j));
							}
						}
					}

					template <typename RSTORAGE>
					void zip(
						const BaseMatrix<T, H, W, RSTORAGE>& m,
						std::function<void(MatrixIndex i, MatrixIndex j, T, T)> f) const noexcept
					{
						for (int i = 0; i < H; ++i)
						{
							for (int j = 0; j < W; ++j)
							{
								if (i == j) f(i, j, at(i, j), m.at(i, j));
								else f(i, j, at(i, j), m.at(i, j));
							}
						}
					}

					std::ostream& print(std::ostream& out, MatrixIndex cx, MatrixIndex cy) const noexcept
					{
						for (int i = 0; i < H; i++) {
							for (int j = 0; j < W; j++) {
								if (i == cx && j == cy) {
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
				protected:
					STORAGE Data;
					T& at(MatrixIndex i, MatrixIndex j) noexcept { return Data.at(i, j); }
					T at(MatrixIndex i, MatrixIndex j) const noexcept { return Data.at(i, j); }

					template <typename TNumber2, NaturalNumber HEIGHT2, NaturalNumber WIDTH2, typename STORAGE2>
					friend class BaseMatrix;
				};

				//ALGORITHMS
				struct GeneralCholeskyFactor;

				//TRAITS
				template <typename T> class MatrixTrait {};
				template <>
				struct MatrixTrait<NONE>
				{
					template <typename T, NaturalNumber H, NaturalNumber W>
					using Storage = typename ColumnOrientedArray<T, H, W>;
					using InPlace = std::true_type;
				};

				template <>
				struct MatrixTrait<IDENTITY>
				{
					template <typename T, NaturalNumber H, NaturalNumber W>
					using Storage = typename IdentityStorage<T>;
					using InPlace = std::false_type;
				};

				template <>
				struct MatrixTrait<UPPERBANDED>
				{
					template <typename T, NaturalNumber H, NaturalNumber W>
					using Storage = typename BandedSchemeArray<T>;
					using InPlace = std::false_type;
				};

				template <>
				struct MatrixTrait<UPPER>
				{
					template <typename T, NaturalNumber H, NaturalNumber W>
					using Storage = typename UpperTriangularColumnOrientedArray<T, H>;
					using InPlace = std::false_type;
				};;

				template <typename TAG,
					typename T,
					NaturalNumber H,
					NaturalNumber W>
					using MatrixStorage = typename MatrixTrait<TAG>::template Storage<T, H, W>;
				template <typename TAG>
				using MatrixInPlace = typename MatrixTrait<TAG>::InPlace;

				//MATRICES
				template <typename TNumber,
					NaturalNumber HEIGHT,
					NaturalNumber WIDTH,
					typename TAG>
					class Matrix :
					public BaseMatrix <TNumber, HEIGHT, WIDTH, MatrixStorage<TAG, TNumber, HEIGHT, WIDTH>>
				{
				public:
					using TSTORAGE = MatrixStorage<TAG, TNumber, HEIGHT, WIDTH>;
					using TINPLACE = MatrixInPlace<TAG>;

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
							return result.getTranspose();
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

					//TODO only if square
					UpperMatrix<TNumber, HEIGHT> getUpper() const noexcept
					{
						return { Data };
					}

					Matrix() : BaseMatrix() { }
					Matrix(const MatrixStorage<TAG, TNumber, HEIGHT, WIDTH>& data) : BaseMatrix(data) { }
					Matrix(const std::initializer_list<TNumber>& l) : BaseMatrix(l) { }

					//Fast multiplication by Identity
					Matrix<TNumber, HEIGHT, WIDTH, TAG> operator * (
						const MatrixIdentity<TNumber, WIDTH>&) const noexcept
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
					template <typename RTAG>
					Matrix<TNumber, HEIGHT, 1, TAG> operator * (
						const Vector<TNumber, WIDTH, RTAG>& vector) const noexcept
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
					template<typename = std::enable_if<TINPLACE::value, bool>>
					bool solve(
						Vector<TNumber, HEIGHT>& b,
						SolveAlgorithm algo = SolveAlgorithm::forward_row_oriented)
					{
						if (algo == SolveAlgorithm::forward_row_oriented)
						{
							return row_foward_substitution<TNumber, HEIGHT>(this->Data, b.Data);
						}
						else if (algo == SolveAlgorithm::forward_column_oriented)
						{
							return column_foward_substitution<TNumber, HEIGHT>(this->Data, b.Data);
						}
						else if (algo == SolveAlgorithm::cholesky)
						{
							//TODO it does not return the result
							//create a immutable and inplace version
							auto[r, x] = solve_cholesky(*this, b);
							b = x;
							return r;
						}
						else if (algo == SolveAlgorithm::backward_row_substitution)
						{
							return row_backward_substitution<TNumber, HEIGHT>(this->Data, b.Data);
						}
					}

					Matrix<TNumber, WIDTH, HEIGHT, TAG> getTranspose() const noexcept
					{
						Matrix<TNumber, WIDTH, HEIGHT, TAG> result;
						map([&](auto i, auto j, auto x) {
							result.at(j, i) = x;
						});
						return result;
					}

					template<typename = std::enable_if<TINPLACE::value, bool>>
					bool choleskyFactor() noexcept
					{
						return GeneralCholeskyFactor::run(Data, Data);
					}

					using TGETCHOLESKYRETURN = UpperMatrix<TNumber, HEIGHT>;
					task<TGETCHOLESKYRETURN> getCholeskyFactor() const noexcept
					{
						TGETCHOLESKYRETURN::TSTORAGE storage(Data);
						auto result = GeneralCholeskyFactor::run(Data, storage);
						return { result, TGETCHOLESKYRETURN(storage) };
					}

					bool isPositiveDefinite() const noexcept
					{
						auto[r, R] = factorCholeskyErasure();
						return r;
					}

					Matrix<float, WIDTH, 1, NONE> ei(MatrixIndex i) const 
					{
						auto e = ColumnOrientedArray<float, HEIGHT, 1>::Zeros();
						e.at(i, 0) = (TNumber)1;
						return { e };
					}

					Matrix<TNumber, HEIGHT, WIDTH, NONE> inverse(InverseAlgorithm algo = InverseAlgorithm::cholesky) const noexcept
					{
						if (algo == InverseAlgorithm::cholesky)
						{
							auto[r1, R] = this->getCholeskyFactor();
							if (!r1) return {};
							auto RT = R.getTranspose();

							std::vector<TNumber> A1;

							for (MatrixIndex j = 0; j < Data.width(); ++j)
							{
								auto ej = ei(j);

								auto[r2, y] = RT.solve(ej);
								if (!r2) return {};

								auto[r3, x] = R.solve(y);
								if (!r3) return {};

								x.map([&](auto i, auto j, auto value) {
									A1.push_back(value);
								});
							}

							return { ColumnOrientedArray<TNumber,HEIGHT,WIDTH>{ A1 } };
						}
					}
				private:
					template <typename TNumber2, NaturalNumber HEIGHT2, NaturalNumber WIDTH2, typename TAG2>
					friend class Matrix;
				};

				template <typename TNumber, NaturalNumber HEIGHT>
				class Matrix<TNumber, HEIGHT, HEIGHT, IDENTITY>
					: public BaseMatrix<TNumber, HEIGHT, HEIGHT, IdentityStorage<TNumber>>
				{
				public:
					Matrix() : BaseMatrix() { }

					template<typename RTNumber, typename RTAG, NaturalNumber RWIDTH>
					Matrix<RTNumber, HEIGHT, RWIDTH, RTAG> operator * (
						const Matrix<RTNumber, HEIGHT, RWIDTH, RTAG>& r
						) const noexcept
					{
						return r;
					}
				};

				template <typename TNumber, NaturalNumber HEIGHT>
				class Matrix<TNumber, HEIGHT, HEIGHT, UPPER>
					: public BaseMatrix<TNumber, HEIGHT, HEIGHT, MatrixStorage<UPPER, TNumber, HEIGHT, HEIGHT>>
				{
				public:
					using TSTORAGE = MatrixStorage<UPPER, TNumber, HEIGHT, HEIGHT>;

					Matrix() : BaseMatrix() {}
					Matrix(const TSTORAGE& data) : BaseMatrix(data) {}
					Matrix(const TNumber* data) : BaseMatrix(data) { }
					template<typename ANOTHERSTORAGE>
					Matrix(const ANOTHERSTORAGE& array) : BaseMatrix(TSTORAGE(array)) { }
					Matrix(const std::initializer_list<TNumber>& list) : BaseMatrix(list) { }

					LowerMatrix<TNumber, HEIGHT> getTranspose() const noexcept
					{
						return { this->Data.toLower() };
					}

					UpperBandedMatrix<TNumber, HEIGHT> asBanded() const noexcept
					{
						return { Data };
					}

					template <typename BTAG>
					std::tuple<bool, Vector<TNumber, HEIGHT, NONE>>
						solve(Vector<TNumber, HEIGHT, BTAG>& b) const noexcept
					{
						auto newA = UpperMatrix<TNumber, HEIGHT>(*this);
						auto result = Vector<TNumber, HEIGHT, BTAG>(b);
						auto r = row_backward_substitution<TNumber, HEIGHT>(
							newA.Data,
							result.Data);
						return std::make_tuple(r, result);
					}
				};

				template <typename TNumber, NaturalNumber HEIGHT>
				class Matrix<TNumber, HEIGHT, HEIGHT, LOWER >
					: BaseMatrix<TNumber, HEIGHT, HEIGHT, LowerTriangularColumnOrientedArray<TNumber, HEIGHT, DONOTHING<TNumber>> >
				{
				public:
					using STORAGE = LowerTriangularColumnOrientedArray<TNumber, HEIGHT, DONOTHING<TNumber>>;

					static LowerMatrix<TNumber, HEIGHT> From(const TNumber* f)
					{
						LowerMatrix<TNumber, HEIGHT> result;
						std::copy_n(f, Size(), result.Items);
						return result;
					}

					Matrix(LowerTriangularColumnOrientedArray<TNumber, HEIGHT, DONOTHING<TNumber>> array) : BaseMatrix(array)
					{
					}

					template <typename TAG>
					std::tuple<bool, Vector<TNumber, HEIGHT, NONE>>
						solve(const Vector<TNumber, HEIGHT, TAG>& b) const noexcept
					{
						auto newA = LowerMatrix<TNumber, HEIGHT>(*this);
						auto result = Vector<TNumber, HEIGHT, TAG>(b);
						auto r = row_foward_substitution<TNumber, HEIGHT>(
							newA.Data,
							result.Data);
						return std::make_tuple(r, result);
					}
				};

				template <typename TNumber, typename TAG>
				class DMatrix
				{
				public:
					using TSTORAGE = DColumnOrientedArray<TNumber>;

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

					DMatrix() { }
					DMatrix(TSTORAGE data) : Data(data) {}
					DMatrix(MatrixIndex h, MatrixIndex w, const TNumber* data) : Data(h, w, data)
					{
					}

					void map(std::function<void(MatrixIndex, MatrixIndex, TNumber)> f) const noexcept
					{
						Data.map(f);
					}
					template <typename RTAG>
					void zip(
						const DMatrix<TNumber, RTAG>& m,
						std::function<void(MatrixIndex i, MatrixIndex j, TNumber, TNumber)> f) const noexcept
					{
						for (MatrixIndex i = 0; i < Data.height(); ++i)
						{
							for (MatrixIndex j = 0; j < Data.width(); ++j)
							{
								if (i == j) f(i, j, Data.at(i, j), m.Data.at(i, j));
								else f(i, j, Data.at(i, j), m.Data.at(i, j));
							}
						}
					}

					DMatrix<TNumber, TAG> transpose() const noexcept
					{
						TSTORAGE result(Data.width(), Data.height(), nullptr);
						Data.map([&](auto i, auto j, auto x) {
							result.at(j, i) = x;
						});
						return { result };
					}

					template <typename RTAG>
					DMatrix<TNumber, NONE> operator * (
						const DMatrix<TNumber, RTAG>& right) const noexcept
					{
						DMatrix<TNumber, NONE> result(this->Data.height(), right.Data.width(), nullptr);

						for (NaturalNumber i = 0; i < this->Data.height(); ++i)
						{
							for (NaturalNumber j = 0; j < right.Data.width(); ++j)
							{
								auto& v = result.Data.at(i, j);
								v = 0;
								for (NaturalNumber k = 0; k < right.Data.width(); ++k)
								{
									v += Data.at(i, k) * right.Data.at(k, j);
								}
							}
						}

						return result;
					}

					bool factorCholesky() noexcept
					{
						int HEIGHT = Data.height();
						int WIDTH = Data.width();

						if (HEIGHT != WIDTH) return false;
						for (int i = 0; i < HEIGHT; ++i)
						{
							auto& aii = Data.at(i, i);
							for (int k = 0; k < i; ++k)
							{
								auto aki = Data.at(k, i);
								aii -= aki * aki;
							}
							if (aii <= 0) return false;
							aii = sqrt(aii);
							for (int j = 0; j < i; ++j) Data.at(i, j) = 0.0f;
							for (int j = i + 1; j < HEIGHT; ++j)
							{
								for (int k = 0; k < i; ++k)
								{
									Data.at(i, j) -= Data.at(k, i)*Data.at(k, j);
								}
								Data.at(i, j) /= aii;
							}
						}
						return true;
					}

					using TGETCHOLESKYRETURN = DMatrix<TNumber, NONE>;
					task<TGETCHOLESKYRETURN> getCholeskyFactor() const noexcept
					{
						TGETCHOLESKYRETURN::TSTORAGE storage(Data);
						auto result = GeneralCholeskyFactor::run(Data, storage);
						return { result, TGETCHOLESKYRETURN(storage) };
					}

					bool isPositiveDefinite() const noexcept
					{
						TGETCHOLESKYRETURN::TSTORAGE storage(Data);
						auto result = GeneralCholeskyFactor::run(Data, storage);
						return  result;
					}

					DMatrix<TNumber, TAG> getTranspose() const noexcept
					{
						DMatrix<TNumber, TAG> result;
						map([&](auto i, auto j, auto x) {
							result.at(j, i) = x;
						});
						return result;
					}





					DMatrix<TNumber, UPPERBANDED> asBanded() const noexcept
					{
						BandedSchemeArray<TNumber> data(Data);

						return { data };
					}

					std::ostream& print(std::ostream& out, MatrixIndex cx, MatrixIndex cy) const noexcept
					{
						for (MatrixIndex i = 0; i < Data.height(); i++) {
							for (MatrixIndex j = 0; j < Data.width(); j++) {
								if (i == cx && j == cy) {
									out << "[" << std::setprecision(5) << std::setw(5) << Data.at(i, j) << "] ";
								}
								else {
									out << std::setprecision(5) << std::setw(5) << Data.at(i, j) << " ";
								}
							}
							out << std::endl;
						}
						return out;
					}

					MatrixIndex height() const noexcept { return Data.height(); }
					MatrixIndex width() const noexcept { return Data.width(); }
				private:
					DColumnOrientedArray<TNumber> Data;
				};

				template <typename TNumber>
				class DMatrix<TNumber, UPPERBANDED>
				{
				public:
					using TSTORAGE = BandedSchemeArray<TNumber>;

					static DMatrix<TNumber, UPPERBANDED> ByRow(MatrixIndex h, MatrixIndex w,
						const TNumber* data,
						ByRowTrait traits = ByRowTrait::None)
					{
						if (traits == ByRowTrait::None)
						{
							DMatrix<TNumber, TAG> result(h, w, data);
							return result.transpose();
						}
					}

					DMatrix() { }
					DMatrix(TSTORAGE data) : Data(data) {}
					DMatrix(MatrixIndex h, MatrixIndex w, const TNumber* data) : Data(h, w, data)
					{
					}

					void map(std::function<void(MatrixIndex, MatrixIndex, TNumber)> f) const noexcept
					{
						Data.map(f);
					}
					template <typename RTAG>
					void zip(
						const DMatrix<TNumber, RTAG>& m,
						std::function<void(MatrixIndex i, MatrixIndex j, TNumber, TNumber)> f) const noexcept
					{
						for (MatrixIndex i = 0; i < Data.height(); ++i)
						{
							for (MatrixIndex j = 0; j < Data.width(); ++j)
							{
								if (i == j) f(i, j, Data.at(i, j), m.Data.at(i, j));
								else f(i, j, Data.at(i, j), m.Data.at(i, j));
							}
						}
					}

					DMatrix<TNumber, NONE> getTranspose() const noexcept
					{
						DMatrix<TNumber, NONE> result;
						map([&](auto i, auto j, auto x) {
							result.at(j, i) = x;
						});
						return result;
					}

					//DMatrix<TNumber, UPPERBANDED> transpose() const noexcept
					//{
					//	TSTORAGE result(Data.width(), Data.height(), nullptr);
					//	Data.map([&](auto i, auto j, auto x) {
					//		result.at(j, i) = x;
					//	});
					//	return { result };
					//}

					template <typename RTAG>
					DMatrix<TNumber, NONE> operator * (
						const DMatrix<TNumber, RTAG>& right) const noexcept
					{
						DMatrix<TNumber, NONE> result(this->Data.height(), right.Data.width(), nullptr);

						for (NaturalNumber i = 0; i < this->Data.height(); ++i)
						{
							for (NaturalNumber j = 0; j < right.Data.width(); ++j)
							{
								auto& v = result.Data.at(i, j);
								v = 0;
								for (NaturalNumber k = 0; k < right.Data.width(); ++k)
								{
									v += Data.at(i, k) * right.Data.at(k, j);
								}
							}
						}

						return result;
					}

					using TGETCHOLESKYRETURN = DMatrix<TNumber, NONE>;
					task<TGETCHOLESKYRETURN> getCholeskyFactor() const noexcept
					{
						TGETCHOLESKYRETURN::TSTORAGE storage(Data);
						auto result = GeneralCholeskyFactor::run(Data, storage);
						return { result, TGETCHOLESKYRETURN(storage) };
					}
					bool isPositiveDefinite() const noexcept
					{
						TGETCHOLESKYRETURN::TSTORAGE storage(Data);
						auto result = GeneralCholeskyFactor::run(Data, storage);
						return  result;
					}

					std::ostream& print(std::ostream& out, MatrixIndex cx, MatrixIndex cy) const noexcept
					{
						for (MatrixIndex i = 0; i < Data.height(); i++) {
							for (MatrixIndex j = 0; j < Data.width(); j++) {
								if (i == cx && j == cy) {
									out << "[" << std::setprecision(5) << std::setw(5) << Data.at(i, j) << "] ";
								}
								else {
									out << std::setprecision(5) << std::setw(5) << Data.at(i, j) << " ";
								}
							}
							out << std::endl;
						}
						return out;
					}

					MatrixIndex height() const noexcept { return Data.height(); }
					MatrixIndex width() const noexcept { return Data.width(); }
				private:
					TSTORAGE Data;
				};

				//constructors
				template <typename T,
					NaturalNumber H,
					NaturalNumber W>
					Matrix<T, H, W, NONE>
					make_col(const std::initializer_list<T>& l)
				{
					return { l };
				}

				Matrix<float, 2, 2, NONE>
					make_col2f(const std::initializer_list<float>& l) { return { l }; }

				template <NaturalNumber H, NaturalNumber W>
				Matrix<float, H, W, NONE>
					make_rowf(const std::initializer_list<float>& l)
				{
					return Matrix<float, H, W, NONE>::ByRow(l);
				}

				DMatrix<float, NONE>
					make_rowf(MatrixIndex h, MatrixIndex w, const std::initializer_list<float>& l)
				{
					DRowOrientedArray<float> data(h, w, l);
					return { data };
				}

				template <typename T,
					NaturalNumber H,
					NaturalNumber W>
					Matrix<T, H, W, UPPERBANDED>
					make_banded(const std::initializer_list<T>& l)
				{
					return { l };
				}

				Matrix<float, 2, 2, UPPERBANDED>
					make_banded2f(const std::initializer_list<float>& l)
				{
					return { l };
				}

				template <typename T, NaturalNumber N>
				Matrix<T, N, N, NONE>
					make_toeplitz(const std::initializer_list<T>& l)
				{
					auto data = ColumnOrientedArray<float, N, N>::Zeros();
					int i = 0;
					int dir = -1;
					std::for_each(l.begin(), l.end(), [&](auto value) {
						data.setDiagonal(i * dir, value);
						if (dir < 0) { ++i; }
						dir *= -1;
					});
					return { data };
				}

				template <NaturalNumber N>
				Matrix<float, N, N, NONE>
					make_toeplitzf(const std::initializer_list<float>& l)
				{
					return make_toeplitz<float, N>(l);
				}

				template <typename T>
				DMatrix<T, NONE>
					make_toeplitz(NaturalNumber size, const std::initializer_list<T>& l)
				{
					auto data = DColumnOrientedArray<float>::Zeros(size, size);
					int i = 0;
					int dir = -1;
					std::for_each(l.begin(), l.end(), [&](auto value) {
						data.setDiagonal(i * dir, value);
						if (dir < 0) { ++i; }
						dir *= -1;
					});
					return { data };
				}

				DMatrix<float, NONE>
					make_toeplitzf(NaturalNumber size, const std::initializer_list<float>& l)
				{
					return make_toeplitz(size, l);
				}

				struct GeneralCholeskyFactor
				{
					template <typename ARRAY1, typename ARRAY2>
					static bool run(const ARRAY1& source, ARRAY2& r)
					{
						NaturalNumber HEIGHT = source.height();
						NaturalNumber WIDTH = source.width();

						if (HEIGHT != WIDTH) return false;

						for (NaturalNumber i = 0; i < HEIGHT; ++i)
						{
							auto aii = r.at(i, i);
							source.mapCholeskyK(i, LAMBDA(k)
							{
								auto aki = r.at(k, i);
								aii -= aki * aki;
							});
							if (aii <= 0) return false;
							aii = sqrt(aii);
							r.at(i, i) = aii;

							r.setLower(0);

							source.mapCholeskyJ(i, LAMBDA(j)
							{
								auto aij = r.at(i, j);
								source.mapCholeskyK(i, LAMBDA(k)
								{
									aij -= r.at(k, i)*r.at(k, j);
								});
								r.at(i, j) = aij / aii;
							});
						}

						return true;
					}
				};

				template <typename T,
					NaturalNumber HEIGHT,
					typename ARRAY1,
					typename ARRAY2>
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

				template <typename T,
					NaturalNumber HEIGHT,
					typename ARRAY1,
					typename ARRAY2>
					bool column_foward_substitution(
						ARRAY1& A,
						ARRAY2& b)
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

				template <typename T,
					NaturalNumber HEIGHT,
					typename ARRAY1,
					typename ARRAY2>
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

				template <NaturalNumber HEIGHT,
					NaturalNumber WIDTH,
					bool CLEARLOWER = true,
					typename ARRAY2>
					bool choleskyFactor(ARRAY2& r)
				{
					return GeneralCholeskyFactor::run<HEIGHT, WIDTH>(r, r);
				}

				//A*x=b
				//A = R^T*R;
				//R^T*R*x=b;
				//R*x=y;
				//R^T*y=b; <- R^T is Lower Triangular Matrix (Foward Substitution)
				//R*x = y; <- R is Upper Triangular Matrix (Backward Substitution)
				template <typename T,
					NaturalNumber HEIGHT,
					typename ATAG,
					typename BTAG>
					task<Vector<T, HEIGHT, NONE>>
					solve_cholesky(
						const Matrix<T, HEIGHT, HEIGHT, ATAG>& A,
						const Vector<T, HEIGHT, BTAG>& b)
				{
					auto[r1, R] = A.getCholeskyFactor();
					if (!r1) return {};

					auto RT = R.getTranspose();
					auto[r2, y] = RT.solve(b);
					if (!r2) return {};

					auto[r3, x] = R.solve(y);
					if (!r3) return {};

					return { x };
				}
			}
		}
	}
}