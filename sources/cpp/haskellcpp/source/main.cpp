#include <array>
#include <iostream>
#include <algorithm>
#include <type_traits>
#include <vector>
#include <functional>

#ifdef _DEBUG 
#include <crtdbg.h> 
#define ASSERT(expression) _ASSERTE(expression) 
#define VERIFY(expression) ASSERT(expression) 
#define VERIFY_(expected, expression) ASSERT(expected == expression) 
#else 
#define ASSERT(expression) ((void)0) 
#define VERIFY(expression) (expression) 
#define VERIFY_(expected, expression) (expression) 
#endif

// ENABLER

namespace detail
{
	enum class enabler {};
}

constexpr detail::enabler dummy = {};

template <typename Condition>
using EnableIf = typename std::enable_if<Condition::value, detail::enabler>::type;

template <typename Condition>
using DisableIf = typename std::enable_if<!Condition::value, detail::enabler>::type;

// IS FUNCTOR

template <template <typename> typename T, typename V>
struct is_functor
{
public:
	using pointer = T<V>(T<V>::*)(V(*)(V));
private:
	template <typename U> static auto test(U*p) -> decltype(pointer{ &U::fmap<V(*)(V)> }, std::true_type());
	template <typename U> static auto test(...) -> decltype(std::false_type());
public:
	static constexpr bool value = decltype(test<T<V>>(nullptr))::value;
};

template<class F>
struct is_callable
{
	template<class U> static auto test(U* p) -> decltype(&U::operator(), void(), std::true_type());
	template<class U> static auto test(...) -> decltype(std::false_type());
	static constexpr bool value = decltype(test<F>(nullptr))::value;
};

template<class F, class...Args>
struct is_exact_callable
{
	template<class U> static auto test(U* p) -> decltype((*p)(std::declval<Args>()...), void(), std::true_type());
	template<class U> static auto test(...) -> decltype(std::false_type());
	static constexpr bool value = decltype(test<F>(nullptr))::value;
};

template <typename T>
struct Context
{
	T Value;

	explicit Context(T value) noexcept
		: Value{ value }
	{
	}

	template <typename F>
	Context<T> fmap(F f) const noexcept
	{
		return Context<T>(f(Value));
	}
};

template <>
struct Context<void>
{
	template <typename F>
	Context<void> fmap(F f) const noexcept
	{
		return Context<void>();
	}

	bool operator == (Context<void>) const noexcept
	{
		return true;
	}
};

template<template <typename> typename Context, typename T, EnableIf<is_functor<Context, T>>>
Context<T> just(T value) noexcept
{
	return Context<T>{value};
}

template<typename T>
Context<T> just(T value) noexcept
{
	return Context<T>{value};
}

//template <typename T>
//struct Maybe
//{
//	T Value;
//	static Maybe<T>& None()
//	{
//		static auto *inst = new Maybe<T>();
//		return *inst;
//	}
//
//	Maybe() = default;
//	explicit Maybe(T value) noexcept
//		: Value{ value }
//	{
//	}
//
//	template <typename F>
//	Maybe<T> fmap(F f)
//	{
//		return Maybe<T>(f(Value));
//	}
//};

//template <>
//struct Maybe<void>
//{
//	Maybe() = default;
//	static Maybe<void>& None()
//	{
//		static auto *inst = new Maybe<void>();
//		return *inst;
//	}
//	template <typename F>
//	Maybe<void> fmap(F f)
//	{
//		return None();
//	}
//};

template<typename T>
Context<T> maybe(T value) noexcept
{
	return Context<T>(value);
}

Context<void> none() noexcept
{
	return Context<void>();
}

struct memberfunction_trait {};
struct array_trait {};
struct pushback_trait {};
struct func_trait {};

template<typename F, typename V> struct fmap_trait {};
template<typename F, typename V> struct fmap_trait<F, Context<V>> { using trait = memberfunction_trait; };
//template<typename F, typename V> struct fmap_trait<F, Maybe<V>> { using trait = memberfunction_trait; };
template<typename F, class _Ty, size_t _Size> struct fmap_trait<F, std::array<_Ty, _Size>> { using trait = array_trait; };
template<typename F, class T> struct fmap_trait<F, std::vector<T> > { using trait = pushback_trait; };
//template<typename A, typename B, typename C> struct fmap_trait<std::function<A(B)>, C(A)> { using trait = func_trait; };

template <typename F, template <typename> typename T, typename V>
T<V> fmap_impl(F f, T<V> v, memberfunction_trait)
{
	return v.fmap(f);
}

template <typename F, typename V>
V fmap_impl(F f, V v, array_trait)
{
	V result;

	int i = 0;
	std::for_each(begin(v), end(v), [&](auto x)
	{
		result[i] = f(x);
		++i;
	});

	return result;
}

template <typename F, typename V>
V fmap_impl(F f, V v, pushback_trait)
{
	V result;

	std::for_each(begin(v), end(v), [&](auto x)
	{
		result.push_back(f(x));
	});

	return result;
}

template <typename GA, typename GR, typename FR>
struct fmap_impl_func
{
	typedef GR(*inner)(GA);
	typedef FR(*outter)(GR);
	typedef FR(*result)(GA);
};

//template <typename GA, typename GR, typename FR>
//typename fmap_impl_func<GA, GR, FR>::result fmap_impl(typename fmap_impl_func<GA, GR, FR>::outter f, typename fmap_impl_func<GA, GR, FR>::inner g)
//{
//	return [](GA x) { return f(g(x)); };
//}

template <typename F, typename V, DisableIf<is_callable<V>> = dummy>
V fmap(F f, V v)
{
	return fmap_impl(f, v, fmap_trait<F, V>::trait());
}

////////////////////////////////////////////////////////////
// indices

template<std::size_t...> struct indices {};
template<std::size_t N, std::size_t... Ind> struct make_indices : make_indices<N - 1, N - 1, Ind...> {};
template<std::size_t... Ind> struct make_indices<0, Ind...> : indices<Ind...> {};

////////////////////////////////////////////////////////////
// indice ranges
//
//template<std::size_t C, std::size_t P, std::size_t... N>
//struct increasing_indices_range :
//	increasing_indices_range<C - 1, P + 1, N..., P>
//{};
//
//template<std::size_t C, std::size_t P, std::size_t... N>
//struct decreasing_indices_range :
//	decreasing_indices_range<C + 1, P - 1, N..., P>
//{};
//
//template<std::size_t P, std::size_t... N>
//struct increasing_indices_range<0, P, N...> :
//	indices<N...>
//{};
//
//template<std::size_t P, std::size_t... N>
//struct decreasing_indices_range<0, P, N...> :
//	indices<N...>
//{};
//
//template<std::size_t S, std::size_t E, bool Increasing = (S < E)>
//	struct indices_range;
//
//	template<std::size_t S, std::size_t E>
//	struct indices_range<S, E, true> :
//		increasing_indices_range<E - S, S>
//	{};
//
//	template<std::size_t S, std::size_t E>
//	struct indices_range<S, E, false> :
//		decreasing_indices_range<E - S, S>
//	{};

	////////////////////////////////////////////////////////////
	// Function traits

template<typename T> struct function_traits : function_traits<decltype(&T::operator())> {};

template<typename Ret, typename... Args>
struct function_traits<Ret(Args...)>
{
	/**
	* Number of arguments of the function.
	*/
	static constexpr std::size_t arity = sizeof...(Args);

	/**
	* Return type of the function.
	*/
	using result_type = Ret;

	/**
	* Type of the Nth argument of the function.
	*/
	template<std::size_t N>
	using argument_type = typename std::tuple_element<N, std::tuple<Args...>>::type;
};

template<typename Ret, typename... Args> struct function_traits<Ret(*)(Args...)> : function_traits<Ret(Args...)> {};
template<typename C, typename Ret, typename... Args> struct function_traits<Ret(C::*)(Args...) const> : function_traits<Ret(Args...)> {};
template<typename T> struct function_traits<T&> : function_traits<T> {};
template<typename T> struct function_traits<const T&> : function_traits<T> {};
template<typename T> struct function_traits<T&&> : function_traits<T> {};
template<typename T> struct function_traits<const T&&> : function_traits<T> {};

////////////////////////////////////////////////////////////
// curry

//template<typename Function, typename First, std::size_t... Ind>
//auto curry_impl(const Function& func, First&& first, indices<Ind...>)
//	-> std::function<
//	typename function_traits<Function>::result_type(
//		typename function_traits<Function>::template argument_type<Ind>...)>
//{
//	return [&](typename function_traits<Function>::template argument_type<Ind>&&... args)
//	{
//		return func(
//			std::forward<First>(first),
//			std::forward<typename function_traits<Function>::template argument_type<Ind>>(args)...
//		);
//	};
//}

//template<typename Function, typename First,
//	typename Indices = indices_range<1, function_traits<Function>::arity>>
//	auto curry(Function&& func, First first)
//	-> decltype(curry_impl(std::forward<Function>(func), std::forward<First>(first), Indices()))
//{
//	using FirstArg = typename function_traits<Function>::template argument_type<0>;
//	static_assert(std::is_convertible<First, FirstArg>::value,
//		"the value to be tied should be convertible to the type of the function's first parameter");
//	return curry_impl(std::forward<Function>(func), std::forward<First>(first), Indices());
//}

////////////////////////////////////////////////////////////
// compose

template<typename First, typename Second, std::size_t... Ind>
std::function<typename function_traits<First>::result_type(typename function_traits<Second>::template argument_type<Ind>...)> compose_impl(const First& first, const Second& second, indices<Ind...>)
{
	return [&](typename function_traits<Second>::template argument_type<Ind>&&... args)
	{
		return first(second(
			std::forward<typename function_traits<Second>::template argument_type<Ind>>(args)...
		));
	};
}


template<typename First, typename Second,
	EnableIf<is_callable<Second>> = dummy,
	typename Indices = make_indices<function_traits<Second>::arity>>
	auto fmap(First first, Second second)
	-> decltype(compose_impl(std::forward<First>(first), std::forward<Second>(second), Indices()))
{
	static_assert(function_traits<First>::arity == 1u,
		"all the functions passed to compose, except the last one, must take exactly one parameter");

	using Ret = typename function_traits<Second>::result_type;
	using FirstArg = typename function_traits<First>::template argument_type<0>;
	static_assert(std::is_convertible<Ret, FirstArg>::value,
		"incompatible return types in compose");

	return compose_impl(std::forward<First>(first), std::forward<Second>(second), Indices());
}


template <typename T>
struct iamnotafunctor
{
	template <typename F>
	T fmap(F f)
	{
		return T();
	}
};

template <typename A, typename B>
auto applicative(A a, B b);

template <typename T1, typename T2>
auto applicative(Context<T1> a, Context<T2> b) -> decltype(auto)
{
	return fmap(a.Value, b);
}

template <typename T1, typename T2, size_t Size>
auto applicative(Context<T1> a, std::array<T2, Size> b) -> decltype(auto)
{
	return fmap(a.Value, b);
}

template <typename T1, size_t Size1, typename T2, size_t Size2>
auto applicative(std::array<T1, Size1> a, std::array<T2, Size2> b) -> decltype(auto)
{
	//TODO the array type must be the result of a applied to b
	//TODO all applications must return the same type
	std::array<int, Size1 * Size2> result;

	int i = 0;
	std::for_each(begin(a), end(a), [&](auto x1)
	{
		std::for_each(begin(b), end(b), [&](auto x2)
		{
			result[i] = x1(x2);
			++i;
		});
	});

	return result;
}

template <typename TA1, typename TA2, typename TR>
auto applicative(std::function<TR(TA1, TA2)> fcomplete, Context<TA1> ta1) -> decltype(auto)
{
	std::function<TR(TA2)>  fbinded = [=](TA2 ta2) {return fcomplete(ta1.Value, ta2); };
	return just(fbinded);
}

template <typename TA1, typename TA2, typename TR>
auto liftA2(std::function<TR(TA1, TA2)> fcomplete, Context<TA1> ta1, Context<TA2> ta2) -> decltype(auto)
{
	return applicative(applicative(fcomplete, ta1), ta2);
}

template<template <typename> typename MonadA, typename A, template <typename> typename MonadB, typename B>
MonadB<B> bind(MonadA<A> m, std::function<MonadB<B>(A)> f)
{
	return f(m.Value);
}
//
//template<typename A, typename B>
//Maybe<B> bind(Maybe<A> m, std::function<Maybe<B>(A)> f)
//{
//	
//}
//
//template<typename A, typename B>
//Maybe<B> bind(Just<A> m, std::function<Maybe<B>(A)> f)
//{
//	return f(m.Value);
//}

//http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html
int main()
{
	auto value = 2;
	auto plus3 = [](int x) {return x + 3; };
	auto five = plus3(value);

	ASSERT(five == 5);

	auto just2 = just(2);
	auto just5 = fmap(plus3, just2);

	ASSERT(just5.Value == 5);

	////auto just_isfunctor = is_functor<Just, int>::value;
	////is_functor<Just, int>::pointer p{ &Just<int>::fmap<int(*)(int)> };
	////static_assert(is_functor<Just, int>::value, "How Just<int> is not a Functor! Someone changed the code!");
	//static_assert(is_functor<Maybe, int>::value, "How Maybe<int> is not a Functor! Someone changed the code!");
	//static_assert(is_functor<iamnotafunctor, int>::value == false, "Something strange has happened!!");

	auto maybe2 = maybe(2);
	auto maybe5 = fmap(plus3, maybe2);

	ASSERT(maybe5.Value == 5);

	auto maybeEmpty = fmap(plus3, none());
	ASSERT(maybeEmpty == none());

	std::array<int, 3> stdarrayvalues = { 2, 4, 6 };
	auto stdarraynewvalues = fmap(plus3, stdarrayvalues);

	ASSERT(stdarraynewvalues[0] == 5);
	ASSERT(stdarraynewvalues[1] == 7);
	ASSERT(stdarraynewvalues[2] == 9);

	std::vector<int> stdvectorvalues = { 2,4,6 };
	auto stdvectornewvalues = fmap(plus3, stdvectorvalues);

	ASSERT(stdvectornewvalues[0] == 5);
	ASSERT(stdvectornewvalues[1] == 7);
	ASSERT(stdvectornewvalues[2] == 9);

	auto plus2 = [](int x) {return x + 2; };
	auto plus5 = fmap(plus3, plus2);
	auto fifteen = plus5(10);

	ASSERT(fifteen == 15);

	auto justplus3 = just(plus3);
	just5 = applicative(justplus3, just2);

	ASSERT(just5.Value == 5);

	auto juststdarraynewvalues = applicative(justplus3, stdarrayvalues);

	ASSERT(juststdarraynewvalues[0] == 5);
	ASSERT(juststdarraynewvalues[1] == 7);
	ASSERT(juststdarraynewvalues[2] == 9);

	auto times2 = [](int x) {return x * 2; };
	std::array<std::function<int(int)>, 2> arrayfunctions = { times2, plus3 };
	std::array<int, 3> values123{ 1,2,3 };
	auto justnewvalues2 = applicative(arrayfunctions, values123);

	ASSERT(justnewvalues2[0] == 2);
	ASSERT(justnewvalues2[1] == 4);
	ASSERT(justnewvalues2[2] == 6);
	ASSERT(justnewvalues2[3] == 4);
	ASSERT(justnewvalues2[4] == 5);
	ASSERT(justnewvalues2[5] == 6);

	auto sum = (std::function<int(int,int)>) [](int x, int y) {return x + y; };
	//auto justplus5 = fmap(sum, just5); //TODO the right here is the fmap and not the applicative!
	//auto just3 = just(3);
	//auto just8 = applicative(justplus5, just3);

	//ASSERT(just8.Value == 8);

	//auto product = (std::function<int(int, int)>)[](int x, int y) {return x *y; };
	//auto just15 = liftA2(product, just5, just3);

	//auto half = (std::function<Maybe<int>(int)>)[](int x) {
	//	if (x % 2 == 0) return maybe(x / 2);
	//	else return Maybe<int>::None();
	//};

	//auto just4 = just(4);

	//auto nothing1 = bind(just3, half);
	//maybe2 = bind(just4, half);
	//auto nothing2 = bind(Maybe<int>::None(), half);

	//auto chain = bind(bind(bind(just(20), half), half), half);

	return 0;
}