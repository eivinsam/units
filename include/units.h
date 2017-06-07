#pragma once

#include <ostream>
#include <cmath>
#include <type_traits>

#pragma push_macro("TEMPLATE_QA_QB")
#define TEMPLATE_QA_QB template <class A, class B, Unit UA, Unit UB>
#pragma push_macro("TEMPLATE_A_QB")
#define TEMPLATE_A_QB template <class A, class B, Unit UB, class = details::if_arithmetic_t<A>>
#pragma push_macro("QA")
#define QA quantity<A, UA>
#pragma push_macro("QB")
#define QB quantity<B, UB>

namespace units
{
	enum class Unit : unsigned char { None, Distance, Time, Mass, Area, Volume, Speed, Density, Frequency, Invalid };

	namespace details
	{
		template <class T, class R = void>
		struct if_arithmetic : public std::enable_if<std::is_arithmetic_v<T>> { };
		template <class T>
		using if_arithmetic_t = typename if_arithmetic<T>::type;

		inline constexpr unsigned code(Unit a) { return static_cast<unsigned>(a); }

		// Set code, ordered
		inline constexpr unsigned co(Unit a, Unit b) { return code(a)*code(Unit::Invalid) + code(b); }
		// Set code, unordered
		inline constexpr unsigned cu(Unit a, Unit b) { return a < b ? cu(b, a) : (code(a)*(code(a) + 1)) / 2 + code(b); }
	}

	inline std::ostream& operator<<(std::ostream& out, Unit u)
	{
		switch (u)
		{
		case Unit::Distance: return out << "m";
		case Unit::Time: return out << "s";
		case Unit::Mass: return out << "kg";
		case Unit::Area: return out << "m^2";
		case Unit::Volume: return out << "m^3";
		case Unit::Speed: return out << "m/s";
		case Unit::Density: return out << "kg/m^3";
		case Unit::Frequency: return out << "Hz";
		default:
			return out << "{invalid unit}";
		}
	}

	template <Unit A, Unit B>
	struct equal_test { static_assert(A == B, "operation requres equal units"); };

	template <Unit A, Unit B>
	constexpr equal_test<A, B> require_equal = {};


	constexpr inline Unit operator*(Unit a, Unit b)
	{
		using namespace details;
		if (a == Unit::Invalid || b == Unit::Invalid)
			return Unit::Invalid;

		if (a == Unit::None) return b;
		if (b == Unit::None) return a;

		switch (cu(a, b))
		{
		case cu(Unit::Distance, Unit::Distance): return Unit::Area;
		case cu(Unit::Distance, Unit::Area): return Unit::Volume;
		case cu(Unit::Speed, Unit::Time): return Unit::Distance;
		case cu(Unit::Distance, Unit::Frequency): return Unit::Speed;
		case cu(Unit::Time, Unit::Frequency): return Unit::None;
		default:
			return Unit::Invalid;
		}
	}
	constexpr inline Unit operator/(Unit a, Unit b)
	{
		using namespace details;
		if (a == Unit::Invalid || b == Unit::Invalid)
			return Unit::Invalid;

		if (a == b) return Unit::None;

		if (b == Unit::None) return a;
		if (a == Unit::None) switch (b)
		{
		case Unit::Time: return Unit::Frequency;
		case Unit::Frequency: return Unit::Time;
		default:
			return Unit::Invalid;
		}

		switch (co(a, b))
		{
		case co(Unit::Distance, Unit::Time): return Unit::Speed;
		case co(Unit::Speed, Unit::Distance): return Unit::Frequency;
		case co(Unit::Distance, Unit::Speed): return Unit::Time;
		case co(Unit::Volume, Unit::Area): return Unit::Distance;
		case co(Unit::Volume, Unit::Distance): return Unit::Area;
		case co(Unit::Area, Unit::Distance): return Unit::Distance;
		case co(Unit::Mass, Unit::Volume): return Unit::Density;
		default:
			return Unit::Invalid;
		}
	}

	template <Unit U>
	struct Unity { static_assert(U != Unit::Invalid, "Invalid unit"); };

	static constexpr Unity<Unit::Distance> meter;
	static constexpr Unity<Unit::Time> second;
	static constexpr Unity<Unit::Mass> kilogram;
	static constexpr Unity<Unit::Frequency> hertz;

	template <Unit UA, Unit UB>
	inline constexpr Unity<UA * UB> operator*(Unity<UA>, Unity<UB>) { return {}; }
	template <Unit UA, Unit UB>
	inline constexpr Unity<UA / UB> operator/(Unity<UA>, Unity<UB>) { return {}; }

	template <typename T, Unit U>
	class quantity
	{
	public:
		static_assert(std::is_arithmetic_v<T>, "T must be an arithmetic type");
		static_assert(U != Unit::Invalid, "Invalid unit");
		static_assert(U != Unit::None, "Logic error: Units::None should always result in raw arithmetic type");

		T value;

		quantity() { }

		template <class S, class = details::if_arithmetic_t<S>>
		explicit constexpr quantity(S value) : value(T(value)) { }

		T& operator*() { return value; }
		const T& operator*() const { return value; }
		T* operator->() { return &value; }
		const T* operator->() const { return &value; }

		constexpr quantity& operator+=(const quantity& other) { value += other.value; return *this; }
		constexpr quantity& operator-=(const quantity& other) { value -= other.value; return *this; }

		constexpr auto operator-() const { return quantity{ -value }; }

		explicit constexpr operator double() const { return double(value); }
	};

	namespace details
	{
		template <Unit U>
		struct maker
		{
			template <class T>
			static constexpr auto make(T value) { return quantity<T, U>{ value }; }
		};
		template <>
		struct maker<Unit::None>
		{
			template <class T>
			static constexpr T make(T value) { return value; }
		};
	}

	template <typename T, Unit U>
	std::ostream& operator<<(std::ostream& out, const quantity<T, U>& q) { return out << q.value << U; }


	template <class A, Unit UA>
	constexpr auto abs(quantity<A, UA> v) { return quantity<A, UA>{ std::abs(v.value) }; }

	TEMPLATE_QA_QB constexpr auto operator+(QA a, QB b) { require_equal<UA, UB>; return details::maker<UA>::make(a.value + b.value); }
	TEMPLATE_QA_QB constexpr auto operator-(QA a, QB b) { require_equal<UA, UB>; return details::maker<UA>::make(a.value - b.value); }
	TEMPLATE_QA_QB constexpr auto operator*(QA a, QB b) { return details::maker<UA * UB>::make(a.value * b.value); }
	TEMPLATE_QA_QB constexpr auto operator/(QA a, QB b) { return details::maker<UA / UB>::make(a.value / b.value); }

	TEMPLATE_QA_QB constexpr bool operator!=(QA a, QB b) { require_equal<UA, UB>; return a.value != b.value; }
	TEMPLATE_QA_QB constexpr bool operator< (QA a, QB b) { require_equal<UA, UB>; return a.value <  b.value; }
	TEMPLATE_QA_QB constexpr bool operator<=(QA a, QB b) { require_equal<UA, UB>; return a.value <= b.value; }
	TEMPLATE_QA_QB constexpr bool operator==(QA a, QB b) { require_equal<UA, UB>; return a.value == b.value; }
	TEMPLATE_QA_QB constexpr bool operator>=(QA a, QB b) { require_equal<UA, UB>; return a.value >= b.value; }
	TEMPLATE_QA_QB constexpr bool operator> (QA a, QB b) { require_equal<UA, UB>; return a.value >  b.value; }


	template <class V> using Distance     = quantity<V, Unit::Distance>;
	template <class V> using Time         = quantity<V, Unit::Time>;
	template <class V> using Mass         = quantity<V, Unit::Mass>;
	template <class V> using Area         = quantity<V, Unit::Area>;
	template <class V> using Volume       = quantity<V, Unit::Volume>;
	template <class V> using Speed        = quantity<V, Unit::Speed>;
	template <class V> using Density      = quantity<V, Unit::Density>;
	template <class V> using Frequency    = quantity<V, Unit::Frequency>;

	template <class T, Unit U, class = details::if_arithmetic_t<T>>
	inline constexpr auto operator*(T value, Unity<U>) { return details::maker<U>::make(value); }
	template <class T, Unit U, class = details::if_arithmetic_t<T>>
	inline constexpr auto operator/(T value, Unity<U>) { return details::maker<Unit::None / U>::make(value); }
	template <class A, Unit UA, Unit UB>
	inline constexpr auto operator*(quantity<A, UA> a, Unity<UB>) { return details::maker<UA * UB>::make(a.value); }
	template <class A, Unit UA, Unit UB>
	inline constexpr auto operator/(quantity<A, UA> a, Unity<UB>) { return details::maker<UA / UB>::make(a.value); }

	TEMPLATE_A_QB constexpr auto operator*(A a, QB b) { return details::maker<UB>::make(a * b.value); }
	TEMPLATE_A_QB constexpr auto operator*(QB b, A a) { return details::maker<UB>::make(b.value * a); }
	TEMPLATE_A_QB constexpr auto operator/(A a, QB b) { return details::maker<Unit::None / UB>::make(a / b.value); }
	TEMPLATE_A_QB constexpr auto operator/(QB b, A a) { return details::maker<UB>::make(b.value / a); }

	namespace float_literals
	{
		inline constexpr auto operator"" _m    (long double v) { return static_cast<float>(v)*meter; }
		inline constexpr auto operator"" _m2   (long double v) { return static_cast<float>(v)*meter*meter; }
		inline constexpr auto operator"" _m3   (long double v) { return static_cast<float>(v)*meter*meter*meter; }
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

	template <class V>
	auto sqrt(Area<V> a) { return Distance<V>(std::sqrt(a.value)); }
}

#pragma pop_macro("TEMPLATE_QA_QB")
#pragma pop_macro("TEMPLATE_A_QB")
#pragma pop_macro("QA")
#pragma pop_macro("QB")

namespace uv
{
	template <class T>
	struct is_scalar;

	template <units::Unit U>
	struct is_scalar<units::Unity<U>> : public std::true_type { };

	template <class T, units::Unit U>
	struct is_scalar<units::quantity<T, U>> : public std::true_type { };
}
