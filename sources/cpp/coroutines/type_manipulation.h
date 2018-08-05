#ifndef _MA_TYPE_MANIPULATION_H
#define _MA_TYPE_MANIPULATION_H

#include "tuple.h"

namespace ma
{
	//FUNCTION TYPES

	// primary template.
	template<class T>
	struct function_traits {};

	// partial specialization for function type
	template<class R, class... Args>
	struct function_traits<R(Args...)> {
		using result_type = R;
		using argument_types = tuple<Args...>;
	};

	// partial specialization for function pointer
	template<class R, class... Args>
	struct function_traits<R(*)(Args...)> {
		using result_type = R;
		using argument_types = tuple<Args...>;
	};

	// partial specialization for std::function
	//template<class R, class... Args>
	//struct function_traits<std::function<R(Args...)>> {
	//    using result_type = R;
	//    using argument_types = std::tuple<Args...>;
	//};

	// partial specialization for pointer-to-member-function (i.e., operator()'s)
	template<class T, class R, class... Args>
	struct function_traits<R(T::*)(Args...)> {
		using result_type = R;
		using argument_types = tuple<Args...>;
	};

	template<class T, class R, class... Args>
	struct function_traits<R(T::*)(Args...) const> {
		using result_type = R;
		using argument_types = tuple<Args...>;
	};

	template<class T>
	using result_of = typename function_traits<T>::result_type;

	template<class T>
	using first_argument_of =
		typename tuple_element<0, typename function_traits<T>::argument_types>::type;

	////////// POINTERS AND REFS

	template <typename T> struct remove_reference { using type = T; };
	template <typename T> struct remove_reference<T&> { using type = T; };
	template< typename T> struct remove_reference<T&&> { using type = T; };

	template <typename T> struct remove_qualifications { using type = T; };
	template <typename T> struct remove_qualifications<T&> { using type = T; };
	template< typename T> struct remove_qualifications<T&&> { typedef T type; };
	template< typename T> struct remove_qualifications<T*> { typedef T type; };
	template< typename T> struct remove_qualifications<T* const> { typedef T type; };
	template< typename T> struct remove_qualifications<T* volatile> { typedef T type; };
	template< typename T> struct remove_qualifications<T* const volatile> { typedef T type; };
	// ARRAY?
	template <typename T> using unqualified_type = typename remove_qualifications<T>::type;

	// TEMPLATE TYPES

	template<typename T> struct extract_value_type { using type = T; };
	template<template<typename> class X, typename T> struct extract_value_type <X<T>> { using type = T; };

	template<typename T>
	using first_template_parameter_of = typename extract_value_type<
		unqualified_type<T>
	>::type;

	/////

	template<class T>
	using unqualified_first_argument_of = unqualified_type<
		typename tuple_element<0, typename function_traits<T>::argument_types>::type
	>;
}

#endif
