#define CATCH_CONFIG_MAIN 
#include "../catch/catch.hpp"

struct Basis
{
	static constexpr size_t STANDARD_BASIS = 0;
	static constexpr size_t CUSTOM_BASIS = 100;
};

template <size_t N, typename T, size_t TBasis, typename... MIXINS>
class Vector : public MIXINS...
{
public:
	using value_type = Vector<N, T, TBasis>;
	using reference = value_type&;
	using const_reference = const reference&;
	template<typename... TArgs>
	Vector(TArgs...args) : data{ args... }
	{
	}

	value_type operator + (const_reference r) const
	{
		value_type result;
		for (size_t i = 0; i < N; ++i)
		{
			result.data[i] = this->data[i] + r.data[i];
		}
		return result;
	}

	
private:
	T data[N];
};


template <size_t W, size_t H, typename T>
class Matrix
{

};

template <size_t N, size_t TFrom, size_t TTo>
using change_basis_44f = Matrix<N, N, float>;

template <size_t N, typename T, size_t TBasis>
struct Vector_Matrix
{
	template <typename T2 = T>
	std::enable_if_t<
		TBasis != Basis::STANDARD_BASIS,
		Vector<N, T2, Basis::STANDARD_BASIS>
	> to_standard_basis(const change_basis_44f<N,N, TBasis, Basis::STANDARD_BASIS>& m) const
	{
		return Vector<N, T2, Basis::STANDARD_BASIS> {};
	}
};


using vec3f = Vector<3, float, Basis::STANDARD_BASIS,
	Vector_Matrix<3, float, Basis::STANDARD_BASIS>
>;
template <size_t OID>
using vec3fo = Vector<3, float, Basis::CUSTOM_BASIS + OID,
	Vector_Matrix<3, float, Basis::STANDARD_BASIS>
>;


struct Object
{
	vec3fo<0> v;
};

TEST_CASE("algebra_linear.vec3f.Simple sum", "[ok]")
{
	vec3f a = { 0.0f, 0.0f, 0.0f };
	vec3f b = { 1.0f, 0.0f, 0.0f };

	auto c = a + b;

	Object o;

	auto d = a + o.v.to_standard_basis();
	
}