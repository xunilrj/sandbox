#ifndef _MA_TUPLES_H_
#define _MA_TUPLES_H_


template<int I, typename T>
class tuple_item
{
	T Value;
public:
	tuple_item(const T& v) : Value(v) {
	}
};

template <typename... TArgs>
class tuple;

template <typename T0>
class tuple<T0>
	: tuple_item<0, T0>
{
};

template <typename T0, typename T1>
class tuple<T0, T1> :
	tuple_item<0, T0>,
	tuple_item<1, T1>
{
};

template <typename T0, typename T1, typename T2>
class tuple<T0, T1, T2> :
	tuple_item<0, T0>,
	tuple_item<1, T1>,
	tuple_item<2, T2>
{
public:
};

template <int I, typename T>
struct tuple_element{};

template <int I, typename T0>
struct tuple_element<I,tuple<T0>>
{
	using type = T0;
};

#endif