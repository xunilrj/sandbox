#pragma once
#include <autocheck/autocheck.hpp>

struct NegativeOperator { template <typename TA> auto operator() (const TA& a) { return -a; } };

struct PlusOperator
{
	static constexpr char* NAME = "PlusOperator";
	template <typename TA, typename TB> auto operator()	(const TA& a, const TB& b)
	{
		return a + b;
	}
};
struct MinusOperator
{
	static constexpr char* NAME = "MinusOperator";
	template <typename TA, typename TB> auto operator() (const TA& a, const TB& b)
	{
		return a - b;
	}
};
struct TimesOperator { template <typename TA, typename TB> auto operator() (const TA& a, const TB& b) { return a * b; } };


template <typename T>
autocheck::generator<T> genenator() { return {}; }

template <typename T, typename... TArgs>
auto genenator(const TArgs...args) { return autocheck::cons<T>(args...); }

class catch_reporter : public autocheck::catch_reporter {
	std::string name;
public:
	catch_reporter(const std::string& name) : name{ name } {}

	virtual void success(size_t tests, size_t max_tests,
		size_t trivial, autocheck::distribution&& dist) const
	{
		autocheck::report_success(std::clog, tests, max_tests, trivial, std::move(dist));
		REQUIRE(true);
	}

	virtual void failure(size_t tests, const char* reason) const {
		std::ostringstream out;
		autocheck::report_failure(out, tests, reason);
		FAIL(name + "-" + out.str());
	}
};

template <typename TProp, typename TGen1>
void check(const std::string& name, const TGen1& gen1, const TProp& prop, size_t size = 100)
{
	catch_reporter rep{ name };
	auto arb = autocheck::make_arbitrary(gen1);
	autocheck::check<typename TGen1::result_type>(prop, size, arb, rep);
}

template <typename TProp, typename TGen1, typename TGen2>
void check(const std::string& name, const TGen1& gen1, const TGen2& gen2, const TProp& prop, size_t size = 100)
{
	catch_reporter rep{ name };
	auto arb = autocheck::make_arbitrary(gen1, gen2);
	autocheck::check<typename TGen1::result_type, typename TGen2::result_type>(prop, size, arb, rep);
}

template <typename TProp, typename TGen1, typename TGen2, typename TGen3>
void check(const std::string& name, const TGen1& gen1, const TGen2& gen2, const TGen3& gen3, const TProp& prop, size_t size = 100)
{
	catch_reporter rep{ name };
	auto arb = autocheck::make_arbitrary(gen1, gen2, gen3);
	autocheck::check<
		typename TGen1::result_type,
		typename TGen2::result_type,
		typename TGen3::result_type
	>(prop, size, arb, rep);
}

template <typename TProp, typename TGen1, typename TGen2, typename TGen3, typename TGen4>
void check(const std::string& name, const TGen1& gen1, const TGen2& gen2, const TGen3& gen3, const TGen4& gen4, const TProp& prop, size_t size = 100)
{
	catch_reporter rep{ name };
	auto arb = autocheck::make_arbitrary(gen1, gen2, gen3, gen4);
	autocheck::check<
		typename TGen1::result_type,
		typename TGen2::result_type,
		typename TGen3::result_type,
		typename TGen4::result_type
	>(prop, size, arb, rep);
}

auto any_int() { return genenator<int>(); }
auto any_float() { return genenator<float>(); }
auto any_f32() { return genenator<math::f32>(genenator<float>()); }

template <size_t D>
constexpr auto any_vec()
{
	if constexpr (D == 2) return genenator<math::vec2>(any_f32(), any_f32());
	else if constexpr (D == 3) return genenator<math::vec3>(any_f32(), any_f32(), any_f32());
	else if constexpr (D == 4) return genenator<math::vec4>(any_f32(), any_f32(), any_f32(), any_f32());
	else return genenator<math::vector<D, math::f32>>();
}

auto any_mat4()
{
	return genenator<math::mat4>(
		any_f32(), any_f32(), any_f32(), any_f32(),
		any_f32(), any_f32(), any_f32(), any_f32(),
		any_f32(), any_f32(), any_f32(), any_f32(),
		any_f32(), any_f32(), any_f32(), any_f32()
	);
}

auto any_quat()
{
	auto f = any_f32();
	return genenator<math::quat>(f, f, f, f);
}

auto any_unitquat()
{
	using namespace math;
	auto f = any_f32();
	return autocheck::map([](auto& gens, size_t size)
		{
			auto [a, b, c, d] = gens;
			auto q = quat(a, b, c, d);
			if (q.norm() == 0) return quat(1, 0, 0, 0);
			return q.normalized();
		}, genenator<std::tuple<f32,f32,f32,f32>>(f, f, f, f));
}

auto diagonal_mat4()
{
	using namespace math;
	return autocheck::map([](auto& a, size_t size)
		{
			return mat4::diagonal(
				std::get<0>(a),
				std::get<1>(a),
				std::get<2>(a),
				std::get<3>(a)
			);
		}, genenator<std::tuple<f32,f32,f32,f32>>(any_f32(), any_f32(), any_f32(), any_f32()));
}

/*
					   Magma
					 /       \
	   divisibility /         \ Associativity
				   /           \
			Quasigroup      SemiGroup
				 |               |
		identity |               | Identity
				 |               |
			   Loop           Monoid
				  \             /
	 associativity \           / Invertibility
					\         /
					   Group
						 |
						 | Commutativity
						 |
				   Abelian Group
*/


/*
Closure: For any x, y in M, we have  a (OP) b in M
https://www.encyclopediaofmath.org/index.php/Associativity
*/
template <typename T, typename TOperator>
void check_closure()
{
	using TR = decltype(std::declval<TOperator>()(std::declval<T>(), std::declval<T>()));
	static_assert(std::is_same<T, TR>::value, "Closure not found on this operator");
}

/*
Associativity: For all x, y, z in S, we have [a (OP) b] (OP) c = a (OP) [b (OP) c]
https://www.encyclopediaofmath.org/index.php/Associativity
*/
template <typename TOperator, typename TGen>
void check_associativity(const std::string& name, const TGen& g)
{
	check(name + ".check_associativity." + TOperator::NAME, g, g, g, [](auto&& a, auto&& b, auto&& c)
		{
			auto op = TOperator{};
			return op(op(a, b), c) == op(a, op(b, c));
		});
}

/*
Identity: There exists an I in G such that I (OP) x = x (OP) I = x. We say that I is an identity element of G.
https://www.encyclopediaofmath.org/index.php/Identity_element
*/
template <typename TOperator, typename TGen, typename TIdentity>
void check_identity(const std::string& name, const TGen& g, const TIdentity& I)
{
	check(name + ".check_identity", g, g, [&](auto&& a, auto&& b)
		{
			auto op = TOperator{};
			auto l = op(I, a);
			auto r = op(a, I);
			return (l == a) && (r == a) && (a == l) && (a == r);
		});
}

/*
Invertibility: For all a in G, there exists b in G as inv(a), such that a (OP) inv(a) = inv(a) (OP) a = I, where I is the identity element.
https://www.encyclopediaofmath.org/index.php/Invertible_element
*/
template <typename TOperator, typename TInverse, typename TGen, typename TIdentity>
void check_invertibility(const std::string& name, const TGen& g, const TIdentity& I)
{
	check(name + ".check_invertibility", g, [&](auto&& a)
		{
			auto op = TOperator{};
			auto inv = TInverse{};

			auto Ia = inv(I, a);
			//a (OP) inv(a) = I
			//inv(a) = I (IOP) a
			return op(a, Ia) == I && op(Ia, a) == I;
		});
}

/*
Commutativity: For all a, b in C, a (OP) b = b (OP) a.
https://www.encyclopediaofmath.org/index.php/Commutativity
*/
template <typename TOperator, typename TGen>
void check_commutativity(const std::string& name, const TGen& g)
{
	check(name + ".check_commutativity", g, g, [&](auto&& a, auto&& b)
		{
			auto op = TOperator{};
			return op(a, b) == op(b, a);
		});
}

/*
https://www.encyclopediaofmath.org/index.php/Magma
*/
template <typename T, typename TOperator>
void is_magma()
{
	check_closure<T, TOperator>();
}

/*
https://www.encyclopediaofmath.org/index.php/Semi-group
*/
template <typename T, typename TOperator, typename TGen>
void is_semigroup(const std::string& name, const TGen& g)
{
	is_magma<T, TOperator>();
	check_associativity<TOperator>(name + ".is_semigroup", g);
}

/*
https://en.wikipedia.org/wiki/Quasigroup
*/
template <typename T, typename TSemigroupOperator, typename TLeftQuasigroupOperator, typename TRightQuasigroupOperator, typename TGen>
void is_quasigroup(const std::string& name, const TGen& g)
{
	is_magma<T, TSemigroupOperator>();
	is_magma<T, TLeftQuasigroupOperator>();
	is_magma<T, TRightQuasigroupOperator>();
	//a ∗ x = b,
	//y ∗ a = b
	check(name + ".is_quasigroup", g, g, [&](auto&& a, auto&& b)
		{
			auto s = TSemigroupOperator{};
			auto l = TLeftQuasigroupOperator{};
			auto r = TRightQuasigroupOperator{};
			return b == s(a, l(a, b))	//y = x ∗(x \ y)
				&& b == l(a, s(a, b))	//y = x \ (x ∗ y)
				&& b == s(r(b, a), a)	//y = (y / x) ∗ x
				&& b == r(s(b, a), a);	//y = (y ∗ x) / x
		});
}

/*
https://www.encyclopediaofmath.org/index.php/Monoid
*/
template <typename T, typename TOperator, typename TGen, typename TId>
void is_monoid(const std::string& name, const TGen& g, const TId& id)
{
	is_semigroup<typename TGen::result_type, TOperator>(name + ".is_monoid", g);
	check_identity<TOperator>(name + ".is_monoid", g, id);
}

/*
https://www.encyclopediaofmath.org/index.php/Loop
*/
template <typename T, typename TSemigroupOperator, typename TLeftQuasigroupOperator, typename TRightQuasigroupOperator, typename TGen, typename TIdentity>
void is_loop(const std::string& name, const TGen& g, const TIdentity& I)
{
	is_quasigroup<T, TSemigroupOperator, TLeftQuasigroupOperator, TRightQuasigroupOperator>("is_loop");
	check_identity<TLeftQuasigroupOperator>(name + ".is_loop", g, I);
	check_identity<TRightQuasigroupOperator>(name + ".is_loop", g, I);
}

/*
https://www.encyclopediaofmath.org/index.php/Group
*/
template <typename TSemigroupOperator, typename TLeftQuasigroupOperator, typename TRightQuasigroupOperator, typename TGen, typename TId>
void is_group(const std::string& name, const TGen& g, const TId& id)
{
	is_monoid<typename TGen::result_type, TSemigroupOperator>(name + ".is_group", g, id);
	check_invertibility<TSemigroupOperator, TLeftQuasigroupOperator>(name + ".is_group", g, id);
	check_invertibility<TSemigroupOperator, TRightQuasigroupOperator>(name + ".is_group", g, id);
}

/*
https://www.encyclopediaofmath.org/index.php/Abelian_group
*/
template <typename TSemigroupOperator, typename TLeftQuasigroupOperator, typename TRightQuasigroupOperator, typename TGen, typename TId>
void is_abelian_group(const std::string& name, const TGen& g, const TId& id)
{
	is_group<PlusOperator, MinusOperator, MinusOperator>(name + "is_abelian_group", g, id);
	check_commutativity<TSemigroupOperator>(name + ".is_group", g);
}

/*
https://www.encyclopediaofmath.org/index.php/Distributivity
*/
template <typename TOutsideOperator, typename TInsideOperator, typename TGenOuside, typename TGenInside>
void check_distributivity(const std::string& name, const TGenOuside& outside, const TGenInside& inside)
{
	check(name + ".check_distributivity", outside, inside, inside, [&](auto&& f, auto&& a, auto&& b)
		{
			auto in = TInsideOperator{};
			auto out = TOutsideOperator{};
			return out(f, in(a, b)) == in(out(f, a), out(f, b));
		});
}
