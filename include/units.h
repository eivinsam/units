#pragma once

#include <ostream>

#define TEMPLATE_QA_QB template <class A, class B, Unit UA, Unit UB>
#define QA quantity<A, UA>
#define QB quantity<B, UB>

namespace units
{
	enum class Unit : unsigned char { None, Distance, Time, Mass, Area, Volume, Speed, Density, Angle, AngularSpeed, Invalid };

	inline std::ostream& operator<<(std::ostream& out, Unit u)
	{
		switch (u)
		{
		case Unit::None: return out;
		case Unit::Distance: return out << "m";
		case Unit::Time: return out << "s";
		case Unit::Mass: return out << "kg";
		case Unit::Area: return out << "m^2";
		case Unit::Volume: return out << "m^3";
		case Unit::Speed: return out << "m/s";
		case Unit::Density: return out << "kg/m^3";
		case Unit::Angle: return out << "rad";
		case Unit::AngularSpeed: return out << "rad/s";
		default: 
			return out << "{invalid unit}";
		}
	}

	template <Unit A, Unit B>
	struct equal_test { static_assert(A == B, "operation requres equal units"); };

	template <Unit A, Unit B>
	constexpr equal_test<A, B> require_equal = {};

	constexpr inline unsigned operator|(Unit a, Unit b)
	{ 
		return (static_cast<unsigned>(a)*static_cast<unsigned>(Unit::Invalid)) + static_cast<unsigned>(b);
	}

	constexpr inline Unit operator*(Unit a, Unit b)
	{
		if (a == Unit::Invalid || b == Unit::Invalid)
			return Unit::Invalid;
		if (a == Unit::None) return b;
		if (b == Unit::None) return a;

		switch (a | b)
		{
		case Unit::Distance | Unit::Distance: return Unit::Area;
		case Unit::Distance | Unit::Area: return Unit::Volume;
		case Unit::Area | Unit::Distance: return Unit::Volume;
		case Unit::Speed | Unit::Time: return Unit::Distance;
		case Unit::Time | Unit::Speed: return Unit::Distance;
		case Unit::AngularSpeed | Unit::Time: return Unit::Angle;
		case Unit::Time | Unit::AngularSpeed: return Unit::Angle;
			//case Unit::
		default:
			return Unit::Invalid;
		}
	}
	constexpr inline Unit operator/(Unit a, Unit b)
	{
		if (a == Unit::Invalid || b == Unit::Invalid)
			return Unit::Invalid;
		if (b == Unit::None) return a;
		if (a == b) return Unit::None;

		switch (a | b)
		{
		case Unit::Distance | Unit::Time: return Unit::Speed;
		case Unit::Angle | Unit::Time: return Unit::AngularSpeed;
		case Unit::Volume | Unit::Area: return Unit::Distance;
		case Unit::Volume | Unit::Distance: return Unit::Area;
		case Unit::Area | Unit::Distance: return Unit::Distance;
		case Unit::Mass | Unit::Volume: return Unit::Density;
		default: 
			return Unit::Invalid;
		}
	}

	template <typename T, Unit U>
	class quantity
	{
	public:
		static_assert(std::is_arithmetic_v<T>, "T must be an arithmetic type");
		static_assert(U != Unit::Invalid, "invalid unit");

		T value;

		quantity() { }

		template <class S, class = std::enable_if<std::is_arithmetic_v<S>>>
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

	template <typename T, Unit U>
	std::ostream& operator<<(std::ostream& out, const quantity<T, U>& q) { return out << q.value << U; }


	template <class A, Unit UA>
	constexpr auto abs(quantity<A, UA> v) { return quantity<A, UA>{ std::abs(v.value) }; }

	namespace details
	{
		template <class A, class B>
		using add_type = decltype(std::declval<A>() + std::declval<B>());
		template <class A, class B>
		using sub_type = decltype(std::declval<A>() - std::declval<B>());
		template <class A, class B>
		using mul_type = decltype(std::declval<A>() * std::declval<B>());
		template <class A, class B>
		using div_type = decltype(std::declval<A>() / std::declval<B>());

		template <class T>
		using if_arithmetic_t = std::enable_if_t<std::is_arithmetic_v<T>>;
	}

	TEMPLATE_QA_QB constexpr auto operator+(QA a, QB b) { require_equal<UA, UB>; return quantity<details::add_type<A, B>, UA>{ a.value + b.value }; }
	TEMPLATE_QA_QB constexpr auto operator-(QA a, QB b) { require_equal<UA, UB>; return quantity<details::sub_type<A, B>, UA>{ a.value - b.value }; }
	TEMPLATE_QA_QB constexpr auto operator*(QA a, QB b) { return quantity<details::mul_type<A, B>, UA * UB>{ a.value * b.value }; }
	TEMPLATE_QA_QB constexpr auto operator/(QA a, QB b) { return quantity<details::div_type<A, B>, UA / UB>{ a.value / b.value }; }

	TEMPLATE_QA_QB constexpr bool operator!=(QA a, QB b) { require_equal<UA, UB>; return a.value != b.value; }
	TEMPLATE_QA_QB constexpr bool operator< (QA a, QB b) { require_equal<UA, UB>; return a.value <  b.value; }
	TEMPLATE_QA_QB constexpr bool operator<=(QA a, QB b) { require_equal<UA, UB>; return a.value <= b.value; }
	TEMPLATE_QA_QB constexpr bool operator==(QA a, QB b) { require_equal<UA, UB>; return a.value == b.value; }
	TEMPLATE_QA_QB constexpr bool operator>=(QA a, QB b) { require_equal<UA, UB>; return a.value >= b.value; }
	TEMPLATE_QA_QB constexpr bool operator> (QA a, QB b) { require_equal<UA, UB>; return a.value >  b.value; }


	template <class V> using NoUnit       = quantity<V, Unit::None>;
	template <class V> using Distance     = quantity<V, Unit::Distance>;
	template <class V> using Time         = quantity<V, Unit::Time>;
	template <class V> using Mass         = quantity<V, Unit::Mass>;
	template <class V> using Area         = quantity<V, Unit::Area>;
	template <class V> using Volume       = quantity<V, Unit::Volume>;
	template <class V> using Speed        = quantity<V, Unit::Speed>;
	template <class V> using Density      = quantity<V, Unit::Density>;
	template <class V> using Angle        = quantity<V, Unit::Angle>;
	template <class V> using AngularSpeed = quantity<V, Unit::AngularSpeed>;


	namespace unit
	{
		template <class V> constexpr auto         none(V value) { return       NoUnit<V>{ value }; }
		template <class V> constexpr auto     distance(V value) { return     Distance<V>{ value }; }
		template <class V> constexpr auto         time(V value) { return         Time<V>{ value }; }
		template <class V> constexpr auto         mass(V value) { return         Mass<V>{ value }; }
		template <class V> constexpr auto         area(V value) { return         Area<V>{ value }; }
		template <class V> constexpr auto       volume(V value) { return       Volume<V>{ value }; }
		template <class V> constexpr auto        speed(V value) { return        Speed<V>{ value }; }
		template <class V> constexpr auto      density(V value) { return      Density<V>{ value }; }
		template <class V> constexpr auto        angle(V value) { return        Angle<V>{ value }; }
		template <class V> constexpr auto angularSpeed(V value) { return AngularSpeed<V>{ value }; }
	}

	template <class A, class B, Unit U, class = details::if_arithmetic_t<A>> constexpr auto operator*(A a, quantity<B, U> b) { return unit::none(a) * b; }
	template <class A, class B, Unit U, class = details::if_arithmetic_t<B>> constexpr auto operator*(quantity<A, U> a, B b) { return a * unit::none(b); }
	template <class A, class B, Unit U, class = details::if_arithmetic_t<A>> constexpr auto operator/(A a, quantity<B, U> b) { return unit::none(a) / b; }
	template <class A, class B, Unit U, class = details::if_arithmetic_t<B>> constexpr auto operator/(quantity<A, U> a, B b) { return a / unit::none(b); }

	namespace float_literals
	{
		inline constexpr auto operator"" _m(long double v) { return unit::distance(static_cast<float>(v)); }
		inline constexpr auto operator"" _m2(long double v) { return unit::area(static_cast<float>(v)); }
		inline constexpr auto operator"" _m3(long double v) { return unit::volume(static_cast<float>(v)); }
		inline constexpr auto operator"" _kg(long double v) { return unit::mass(static_cast<float>(v)); }
		inline constexpr auto operator"" _s(long double v) { return unit::time(static_cast<float>(v)); }
		inline constexpr auto operator"" _mps(long double v) { return unit::speed(static_cast<float>(v)); }
		inline constexpr auto operator"" _kgpm3(long double v) { return unit::density(static_cast<float>(v)); }
		inline constexpr auto operator"" _rad(long double v) { return unit::angle(static_cast<float>(v)); }
		inline constexpr auto operator"" _radps(long double v) { return unit::angularSpeed(static_cast<float>(v)); }

		inline constexpr auto operator"" _mm(long double v) { return unit::distance(static_cast<float>(v / 1000)); }
		inline constexpr auto operator"" _g(long double v) { return unit::distance(static_cast<float>(v / 1000)); }
		inline constexpr auto operator"" _kgpl(long double v) { return unit::distance(static_cast<float>(v * 1000)); }

		static constexpr auto meter  = 1.0_m;
		static constexpr auto second = 1.0_s;
		static constexpr auto gram   = 0.001_kg;

		static constexpr float micro = 1e-6f;
		static constexpr float milli = 1e-3f;
		static constexpr float kilo = 1e3f;
		static constexpr float mega = 1e6f;
	}

	using std::sqrt;
	template <class V>
	auto sqrt(Area<V> a) { return Distance<V>(sqrt(a.value)); }
	template <class V>
	auto sqrt(NoUnit<V> a) { return NoUnit<V>(sqrt(a.value)); }
}

#undef TEMPLATE_QA_QB
#undef QA
#undef QB
