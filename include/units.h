#pragma once

#include <ostream>
#include <cmath>
#include <type_traits>

#pragma push_macro("TEMPLATE_QA_QB")
#define TEMPLATE_QA_QB template <class A, class B, class UA, class UB>
#pragma push_macro("TEMPLATE_A_QB")
#define TEMPLATE_A_QB template <class A, class B, class UB, class = details::if_arithmetic_t<A>>
#pragma push_macro("ASSERT_SAME_UNIT")
#define ASSERT_SAME_UNIT static_assert(std::is_same_v<UA, UB>, "Operation requires equal units")
#pragma push_macro("QA")
#define QA Quantity<A, UA>
#pragma push_macro("QB")
#define QB Quantity<B, UB>

namespace units
{
	namespace details
	{
		template <class T>
		static constexpr bool always_false_v = false;

		inline constexpr bool all() { return true; }
		template <class... Rest>
		inline constexpr bool all(bool first, Rest... rest) { return first && all(rest...); }
	}

	template <int m, int kg, int s, int A, int K, int mol, int cd>
	struct Unit { };

	using Ratio = Unit<0, 0, 0, 0, 0, 0, 0>;

	template <int... AN, int... BN>
	constexpr Unit<(AN + BN)...> operator*(Unit<AN...>, Unit<BN...>) { return {}; }
	template <int... AN, int... BN>
	constexpr Unit<(AN - BN)...> operator/(Unit<AN...>, Unit<BN...>) { return {}; }

	template <int... AN>
	constexpr Unit<(AN/2)...> sqrt(Unit<AN...>)
	{
		static_assert(details::all((AN%2 == 0)...), "Cannot take square root of odd-dimensioned Unit");
		return {};
	}
	template <int... AN>
	constexpr Unit<(AN * 2)...> square(Unit<AN...>) { return {}; }
	template <int... AN>
	constexpr Unit<(AN * 3)...> cube(Unit<AN...>) { return {}; }

	static constexpr Unit<1, 0, 0, 0, 0, 0, 0> meter;
	static constexpr Unit<0, 1, 0, 0, 0, 0, 0> kilogram;
	static constexpr Unit<0, 0, 1, 0, 0, 0, 0> second;
	static constexpr Unit<0, 0, 0, 1, 0, 0, 0> Ampere;
	static constexpr Unit<0, 0, 0, 0, 1, 0, 0> Kelvin;

	static constexpr Unit<0, 0, -1, 0, 0, 0, 0> Hertz;

#pragma warning(push)
#pragma warning(disable: 4127)
	template <int m, int kg, int s, int A, int K, int mol, int cd>
	inline std::ostream& operator<<(std::ostream& out, Unit<m, kg, s, A, K, mol, cd>)
	{
		auto print_unit = [&](int p, const char* unit) { if (p < 1) return; out << unit; if (p > 1) out << '^' << p; };
		print_unit(m, "m");
		print_unit(kg, "kg");
		print_unit(s, "s");
		print_unit(A, "A");
		print_unit(K, "K");
		print_unit(mol, "mol");
		print_unit(cd, "cd");
		if (m < 0 || kg < 0 || s < 0 || A < 0 || K < 0 || mol < 0 || cd < 0)
			out << '/' << Unit<-m, -kg, -s, -A, -K, -mol, -cd>{};
		return out;
	}
#pragma warning(pop)


	static constexpr auto foo = sqrt(square(meter));

	namespace details
	{
		template <class U>
		struct is_unit : public std::false_type { };
		template <int... UN>
		struct is_unit<Unit<UN...>> : public std::true_type { };
		template <class U>
		static constexpr bool is_unit_v = is_unit<U>::value;

		template <class T, class R = void>
		struct if_arithmetic : public std::enable_if<std::is_arithmetic_v<T>> { };
		template <class T>
		using if_arithmetic_t = typename if_arithmetic<T>::type;
	}

	template <class T, class U>
	struct Quantity
	{
		static_assert(std::is_arithmetic_v<T>, "T must be an arithmetic type");
		static_assert( std::is_same_v<Ratio, decltype(U{} / U{})> , "U must be a Unit");
		static_assert(!std::is_same_v<Ratio, U>, "Ratio unit should always result in raw arithmetic type");

		T value;

		Quantity() { }

		template <class S, class = details::if_arithmetic_t<S>>
		explicit constexpr Quantity(S value) : value(T(value)) { }

		      T& operator*()       { return value; }
		const T& operator*() const { return value; }

		constexpr Quantity& operator+=(const Quantity& other) { value += other.value; return *this; }
		constexpr Quantity& operator-=(const Quantity& other) { value -= other.value; return *this; }

		constexpr auto operator-() const { return Quantity{ -value }; }

		explicit constexpr operator double() const { return double(value); }
	};


	namespace details
	{
		template <class U>
		struct maker
		{
			template <class T>
			static constexpr auto make(T value) { return Quantity<T, std::remove_const_t<U>>{ value }; }
		};
		template <>
		struct maker<Ratio>
		{
			template <class T>
			static constexpr T make(T value) { return value; }
		};
	}

	template <typename T, class U>
	std::ostream& operator<<(std::ostream& out, const Quantity<T, U>& q) { return out << q.value << U{}; }


	template <class A, class UA>
	constexpr auto abs(Quantity<A, UA> v) { return Quantity<A, UA>{ std::abs(v.value) }; }

	TEMPLATE_QA_QB constexpr auto operator+(QA a, QB b) { ASSERT_SAME_UNIT; return details::maker<UA>::make(a.value + b.value); }
	TEMPLATE_QA_QB constexpr auto operator-(QA a, QB b) { ASSERT_SAME_UNIT; return details::maker<UA>::make(a.value - b.value); }
	TEMPLATE_QA_QB constexpr auto operator*(QA a, QB b) { return details::maker<decltype(UA{} * UB{})>::make(a.value * b.value); }
	TEMPLATE_QA_QB constexpr auto operator/(QA a, QB b) { return details::maker<decltype(UA{} / UB{})>::make(a.value / b.value); }

	TEMPLATE_QA_QB constexpr bool operator!=(QA a, QB b) { ASSERT_SAME_UNIT; return a.value != b.value; }
	TEMPLATE_QA_QB constexpr bool operator< (QA a, QB b) { ASSERT_SAME_UNIT; return a.value <  b.value; }
	TEMPLATE_QA_QB constexpr bool operator<=(QA a, QB b) { ASSERT_SAME_UNIT; return a.value <= b.value; }
	TEMPLATE_QA_QB constexpr bool operator==(QA a, QB b) { ASSERT_SAME_UNIT; return a.value == b.value; }
	TEMPLATE_QA_QB constexpr bool operator>=(QA a, QB b) { ASSERT_SAME_UNIT; return a.value >= b.value; }
	TEMPLATE_QA_QB constexpr bool operator> (QA a, QB b) { ASSERT_SAME_UNIT; return a.value >  b.value; }


	template <class V> using Distance     = Quantity<V, std::remove_const_t<decltype(meter)>>;
	template <class V> using Time         = Quantity<V, std::remove_const_t<decltype(second)>>;
	template <class V> using Mass         = Quantity<V, std::remove_const_t<decltype(kilogram)>>;
	template <class V> using Area         = Quantity<V, std::remove_const_t<decltype(square(meter))>>;
	template <class V> using Volume       = Quantity<V, std::remove_const_t<decltype(cube(meter))>>;
	template <class V> using Speed        = Quantity<V, std::remove_const_t<decltype(meter / second)>>;
	template <class V> using Density      = Quantity<V, std::remove_const_t<decltype(kilogram / cube(meter))>>;
	template <class V> using Frequency    = Quantity<V, std::remove_const_t<decltype(Hertz)>>;

	template <class T, int... UN, class = details::if_arithmetic_t<T>>
	inline constexpr auto operator*(T value, Unit<UN...>) { return details::maker<Unit<UN...>>::make(value); }
	template <class T, int... UN, class = details::if_arithmetic_t<T>>
	inline constexpr auto operator/(T value, Unit<UN...>) { return details::maker<Unit<-UN...>>::make(value); }
	template <class A, class UA, int... BN>
	inline constexpr auto operator*(QA a, Unit<BN...>) { return details::maker<decltype(UA{} * Unit<BN...>{})> ::make(a.value); }
	template <class A, class UA, int... BN>
	inline constexpr auto operator/(QA a, Unit<BN...>) { return details::maker<decltype(UA{} / Unit<BN...>{})> ::make(a.value); }

	TEMPLATE_A_QB constexpr auto operator+(A a, QB b) { static_assert(details::always_false_v<A>, "Addition requires equal units"); }
	TEMPLATE_A_QB constexpr auto operator+(QB b, A a) { static_assert(details::always_false_v<A>, "Addition requires equal units"); }
	TEMPLATE_A_QB constexpr auto operator-(A a, QB b) { static_assert(details::always_false_v<A>, "Substraction requires equal units"); }
	TEMPLATE_A_QB constexpr auto operator-(QB b, A a) { static_assert(details::always_false_v<A>, "Substraction requires equal units"); }

	TEMPLATE_A_QB constexpr auto operator*(A a, QB b) { return details::maker<UB>::make(a * b.value); }
	TEMPLATE_A_QB constexpr auto operator*(QB b, A a) { return details::maker<UB>::make(b.value * a); }
	TEMPLATE_A_QB constexpr auto operator/(A a, QB b) { return details::maker<decltype(Ratio{} / UB{})> ::make(a / b.value); }
	TEMPLATE_A_QB constexpr auto operator/(QB b, A a) { return details::maker<UB>::make(b.value / a); }

	template <class A, class UA> constexpr auto sqrt(QA a) { return details::maker<decltype(sqrt(UA{}))>::make(std::sqrt(a.value)); }

	namespace float_literals
	{
		inline constexpr auto operator"" _m    (long double v) { return static_cast<float>(v)*meter; }
		inline constexpr auto operator"" _m2   (long double v) { return static_cast<float>(v)*square(meter); }
		inline constexpr auto operator"" _m3   (long double v) { return static_cast<float>(v)*cube(meter); }
		inline constexpr auto operator"" _kg   (long double v) { return static_cast<float>(v)*kilogram; }
		inline constexpr auto operator"" _s    (long double v) { return static_cast<float>(v)*second; }
		inline constexpr auto operator"" _mps  (long double v) { return static_cast<float>(v)*meter/second; }
		inline constexpr auto operator"" _kgpm3(long double v) { return static_cast<float>(v)*kilogram/(meter*meter*meter); }

		inline constexpr auto operator"" _mm  (long double v) { return static_cast<float>(v / 1000)*meter; }
		inline constexpr auto operator"" _g   (long double v) { return static_cast<float>(v / 1000)*kilogram; }
		inline constexpr auto operator"" _kgpl(long double v) { return static_cast<float>(v * 1000)*kilogram/(meter*meter*meter); }

		static constexpr float micro = 1e-6f;
		static constexpr float milli = 1e-3f;
		static constexpr float kilo = 1e3f;
		static constexpr float mega = 1e6f;
	}
}

#pragma pop_macro("TEMPLATE_QA_QB")
#pragma pop_macro("TEMPLATE_A_QB")
#pragma pop_macro("ASSERT_SAME_UNIT")
#pragma pop_macro("QA")
#pragma pop_macro("QB")

namespace uv
{
	template <class T>
	struct is_scalar;

	template <int... UN>
	struct is_scalar<units::Unit<UN...>> : public std::true_type { };

	template <class T, class U>
	struct is_scalar<units::Quantity<T, U>> : public std::true_type { };
}
