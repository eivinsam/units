#pragma once

#include <ostream>
#include <istream>
#include <cmath>
#include <type_traits>
#include <numeric>

#pragma push_macro("TEMPLATE_QA_QB")
#define TEMPLATE_QA_QB template <class A, class B, class UA, class UB>
#pragma push_macro("QA")
#define QA Quantity<A, UA>
#pragma push_macro("QB")
#define QB Quantity<B, UB>
#pragma push_macro("FLOAT_LITERAL") 
#define FLOAT_LITERAL(tag, type) \
	inline constexpr auto operator"" tag(long double v) { return static_cast<float>(v)*(type); } \
	inline constexpr auto operator"" tag(unsigned long long v) { return static_cast<float>(v)*(type); }

namespace units
{
	template <size_t P, size_t Q = 1>
	struct Frac
	{
		static constexpr size_t p = P;
		static constexpr size_t q = Q;
		using reduced = Frac<P/std::gcd(P, Q), Q/std::gcd(P, Q)>;

		static constexpr double factor = double(P)/Q;
	};

	template <size_t AP, size_t AQ, size_t BP, size_t BQ> 
	constexpr typename Frac<AP*BP, AQ*BQ>::reduced operator*(Frac<AP, AQ>, Frac<BP, BQ>) { return { }; }
	template <size_t AP, size_t AQ, size_t BP, size_t BQ>
	constexpr typename Frac<AP*BQ, AQ*BP>::reduced operator/(Frac<AP, AQ>, Frac<BP, BQ>) { return { }; }

	inline constexpr size_t square(size_t n) { return n*n; }
	inline constexpr size_t cube(size_t n) { return n*n*n; }

	inline constexpr size_t isqrt(size_t n)
	{
		size_t result = 0;
		for (size_t p = sizeof(size_t)*4; p > 0; --p)
		{
			const size_t high = result | 1<<(p-1);
			if (high*high <= n)
				result = high;
		}
		return result;
	}
	inline constexpr size_t icbrt(size_t n)
	{
		size_t result = 0;
		for (size_t p = (sizeof(size_t)*2666)/1000; p > 0; --p)
		{
			const size_t high = result | 1<<(p-1);
			if (high*high*high <= n)
				result = high;
		}
		return result;
	}

	template <size_t P, size_t Q> constexpr Frac<P*P,   Q*Q> square(Frac<P, Q>) { return {}; }
	template <size_t P, size_t Q> constexpr Frac<P*P*P, Q*Q*Q> cube(Frac<P, Q>) { return {}; }

	template <size_t P, size_t Q>
	constexpr auto sqrt(Frac<P, Q>)
	{
		static_assert(square(isqrt(P)) == P && square(isqrt(Q)) == Q, "P and Q must be squares");
		return Frac<isqrt(P), isqrt(Q)>{};
	}
	template <size_t P, size_t Q>
	constexpr auto cbrt(Frac<P, Q>)
	{
		static_assert(cube(icbrt(P)) == P && cube(icbrt(Q)) == Q, "P and Q must be cubes");
		return Frac<icbrt(P), icbrt(Q)>{};
	}

	static constexpr Frac<1, 10> deci = {};
	static constexpr Frac<1, 100> centi = {};
	static constexpr Frac<1, 1000> milli = {};
	static constexpr Frac<1, 1000000> micro = {};

	static constexpr Frac<10, 1> deka = {};
	static constexpr Frac<100, 1> hecto = {};
	static constexpr Frac<1000, 1> kilo = {};
	static constexpr Frac<1000000, 1> mega = {};

	namespace details
	{
		template <class T>
		static constexpr bool always_false_v = false;

		inline constexpr bool all() { return true; }
		template <class... Rest>
		inline constexpr bool all(bool first, Rest... rest) { return first && all(rest...); }
	}



	struct Zero
	{ 
		constexpr operator int() const { return 0; }
		constexpr operator float() const { return 0; }
		constexpr operator double() const { return 0; }
	};
	static constexpr Zero zero = {};

	// Represents a unit with composed of powers of SI units
	template <class F, int m, int kg, int s, int A, int K, int mol, int cd>
	struct Unit
	{
		static_assert(details::always_false_v<F>, "F must be Frac<P,Q>");
	};
	template <size_t P, size_t Q, int... N>
	struct Unit<Frac<P, Q>, N...>
	{
		static constexpr Frac<P, Q> frac = {};
	};

	using Ratio = Unit<Frac<1>, 0, 0, 0, 0, 0, 0, 0>;

	template <class AF, class BF, int... AN, int... BN> constexpr Unit<decltype(AF{}*BF{}), (AN + BN)...> operator*(Unit<AF, AN...>, Unit<BF, BN...>) { return {}; }
	template <class AF, class BF, int... AN, int... BN> constexpr Unit<decltype(AF{}/BF{}), (AN - BN)...> operator/(Unit<AF, AN...>, Unit<BF, BN...>) { return {}; }

	template <class F, size_t P, size_t Q, int... N> constexpr Unit<decltype(F{}*Frac<P, Q>{}), N...> operator*(Unit<F, N...>, Frac<P, Q>) { return {}; }
	template <class F, size_t P, size_t Q, int... N> constexpr Unit<decltype(F{}/Frac<P, Q>{}), N...> operator/(Unit<F, N...>, Frac<P, Q>) { return {}; }
	template <class F, size_t P, size_t Q, int... N> constexpr Unit<decltype(Frac<P, Q>{}*F{}),  N...> operator*(Frac<P, Q>, Unit<F, N...>) { return {}; }
	template <class F, size_t P, size_t Q, int... N> constexpr Unit<decltype(Frac<P, Q>{}/F{}), -N...> operator/(Frac<P, Q>, Unit<F, N...>) { return {}; }

	template <class AF, int... AN> constexpr auto square(Unit<AF, AN...> u) { return u*u; }
	template <class AF, int... AN> constexpr auto cube(Unit<AF, AN...> u) { return u*u*u; }



#pragma warning(push)
#pragma warning(disable: 4127)
	template <size_t FP, size_t FQ, int m, int kg, int s, int A, int K, int mol, int cd>
	inline std::ostream& operator<<(std::ostream& out, Unit<Frac<FP, FQ>, m, kg, s, A, K, mol, cd>)
	{
		auto print_unit = [&](int p, const char* unit) { if (p < 1) return; out << unit; if (p > 1) out << '^' << p; };
		if (FP != 1 || FQ != 1) 
			out << FP;
		if (FQ != 1)
			out << '/' << FQ;
		print_unit(+m, "m"); print_unit(+kg, "kg"); print_unit(+s, "s"); print_unit(+A, "A"); print_unit(+K, "K"); print_unit(+mol, "mol"); print_unit(+cd, "cd"); 
		print_unit(-m, "m"); print_unit(-kg, "kg"); print_unit(-s, "s"); print_unit(-A, "A"); print_unit(-K, "K"); print_unit(-mol, "mol"); print_unit(-cd, "cd");
		return out;
	}
#pragma warning(pop)


	namespace details
	{
		//template <class U>
		//struct is_unit : public std::false_type { };
		//template <int... UN>
		//struct is_unit<Unit<UN...>> : public std::true_type { };
		//template <class U>
		//static constexpr bool is_unit_v = is_unit<U>::value;

		template <class T>
		using if_arithmetic_t = std::enable_if_t<std::is_arithmetic<T>::value>;
	}



	// A quantity represented by a value of type T with a unit described by type U
	template <class T, class U>
	struct Quantity
	{
		static_assert(details::always_false_v<U>, "U must be a unit");
		static constexpr U unit = {};

		T value;
	};

	namespace details
	{
		template <class U>
		struct maker
		{
			template <class T>
			static constexpr auto make(T value) { return Quantity<T, U>{ value }; }
		};
		template <class U> struct maker<U&> : maker<U> { };
		template <class U> struct maker<const U> : maker<U> { };

		template <class F>
		struct maker<Unit<F, 0,0,0,0,0,0,0>>
		{
			template <class T>
			static constexpr T make(T value) { return (value*F::p)/F::q; }
		};
	}

	template <class T, class F, int... N>
	struct Quantity<T, Unit<F, N...>>
	{
		using U = Unit<F, N...>;
		static constexpr U unit = {};

		static_assert(std::is_arithmetic_v<T>, "T must be an arithmetic type");
		static_assert(!std::is_same_v<Ratio, U>, "Ratio unit should always result in raw arithmetic type");

		T value;

		Quantity() { }
		constexpr Quantity(Zero) : value(0) { }
		constexpr Quantity(const Quantity& b) : value(b.value) { }
		template <class S, class G>
		constexpr Quantity(const Quantity<S, Unit<G, N...>>& b) : value( (b.value*G::p*F::q) / (G::q*F::p) ) { }

		template <class S, class = details::if_arithmetic_t<S>>
		explicit constexpr Quantity(S value) : value(T(value)) { }

		constexpr Quantity& operator+=(const Quantity& other) { value += other.value; return *this; }
		constexpr Quantity& operator-=(const Quantity& other) { value -= other.value; return *this; }

		template <class S, class = details::if_arithmetic_t<S>> constexpr Quantity& operator*=(S c) { value *= c; return *this; }
		template <class S, class = details::if_arithmetic_t<S>> constexpr Quantity& operator/=(S c) { value /= c; return *this; }

		constexpr auto operator-() const { return Quantity{ -value }; }
		constexpr auto operator+() const { return Quantity{ +value }; }

		friend constexpr auto operator*(Quantity q, T c) { return details::maker<U>::make(q.value * c); }
		friend constexpr auto operator/(Quantity q, T c) { return details::maker<U>::make(q.value / c); }
		friend constexpr auto operator*(T c, Quantity q) { return details::maker<U>::make(q.value * c); }
		friend constexpr auto operator/(T c, Quantity q) { return details::maker<decltype(Ratio{} / U{})> ::make(q.value / c); }

		template <class S, class = details::if_arithmetic_t<S>> friend constexpr auto operator*(Quantity q, S c) { return details::maker<U>::make(q.value * c); }
		template <class S, class = details::if_arithmetic_t<S>> friend constexpr auto operator/(Quantity q, S c) { return details::maker<U>::make(q.value / c); }
		template <class S, class = details::if_arithmetic_t<S>> friend constexpr auto operator*(S c, Quantity q) { return details::maker<U>::make(c * q.value); }
		template <class S, class = details::if_arithmetic_t<S>> friend constexpr auto operator/(S c, Quantity q) { return details::maker<decltype(Ratio{} / U{})>::make(c / q.value); }

		template <class S, class G> constexpr auto operator+(Quantity<S, Unit<G, N...>> b) const
		{
			static constexpr size_t cq = std::gcd(unit.frac.q, b.unit.frac.q);
			static constexpr size_t pa =   unit.frac.p*(b.unit.frac.q/cq);
			static constexpr size_t pb = b.unit.frac.p*(  unit.frac.q/cq);
			static constexpr size_t P = std::gcd(pa, pb);
			static constexpr size_t Q = unit.frac.q*(b.unit.frac.q/cq);

			return details::maker<Unit<Frac<P, Q>, N...>>::make((pa/P)*value + (pb/P)*b.value);
		}
		template <class S, class G> constexpr auto operator-(Quantity<S, Unit<G, N...>> b) const { return *this + (-b); }

		friend constexpr bool operator==(Quantity q, Zero) { return q.value == 0; }
		friend constexpr bool operator!=(Quantity q, Zero) { return q.value != 0; }
		friend constexpr bool operator< (Quantity q, Zero) { return q.value <  0; }
		friend constexpr bool operator<=(Quantity q, Zero) { return q.value <= 0; }
		friend constexpr bool operator>=(Quantity q, Zero) { return q.value >= 0; }
		friend constexpr bool operator> (Quantity q, Zero) { return q.value >  0; }

		friend constexpr bool operator==(Zero, Quantity q) { return 0 == q.value; }
		friend constexpr bool operator!=(Zero, Quantity q) { return 0 != q.value; }
		friend constexpr bool operator< (Zero, Quantity q) { return 0 <  q.value; }
		friend constexpr bool operator<=(Zero, Quantity q) { return 0 <= q.value; }
		friend constexpr bool operator>=(Zero, Quantity q) { return 0 >= q.value; }
		friend constexpr bool operator> (Zero, Quantity q) { return 0 >  q.value; }

		template <class S, class G> constexpr bool operator==(Quantity<S, Unit<G, N...>> b) const { return value*b.unit.frac.q == b.value*unit.frac.q; }
		template <class S, class G> constexpr bool operator!=(Quantity<S, Unit<G, N...>> b) const { return value*b.unit.frac.q != b.value*unit.frac.q; }
		template <class S, class G> constexpr bool operator< (Quantity<S, Unit<G, N...>> b) const { return value*b.unit.frac.q <  b.value*unit.frac.q; }
		template <class S, class G> constexpr bool operator<=(Quantity<S, Unit<G, N...>> b) const { return value*b.unit.frac.q <= b.value*unit.frac.q; }
		template <class S, class G> constexpr bool operator>=(Quantity<S, Unit<G, N...>> b) const { return value*b.unit.frac.q >= b.value*unit.frac.q; }
		template <class S, class G> constexpr bool operator> (Quantity<S, Unit<G, N...>> b) const { return value*b.unit.frac.q >  b.value*unit.frac.q; }

		explicit constexpr operator double() const { return double(value); }
	};


	template <typename T, class U>
	std::ostream& operator<<(std::ostream& out, const Quantity<T, U>& q)
	{
		return out << q.value << ' ' << U{};
	}
	template <typename T, class U>
	std::istream& operator>>(std::istream& in, Quantity<T, U>& q)
	{
		return in >> q.value;
	}

	template <class T, size_t P, size_t Q, class = std::enable_if_t<std::is_arithmetic_v<T>>>
	auto operator*(T value, Frac<P, Q>) { return Quantity<T, Unit<Frac<P, Q>, 0, 0, 0, 0, 0, 0, 0>>{ value }; }

	TEMPLATE_QA_QB constexpr auto operator*(QA a, QB b) { return details::maker<decltype(UA{} * UB{})>::make(a.value * b.value); }
	TEMPLATE_QA_QB constexpr auto operator/(QA a, QB b) { return details::maker<decltype(UA{} / UB{})>::make(a.value / b.value); }


	template <class T, class F, int... UN, class = details::if_arithmetic_t<T>>
	inline constexpr auto operator*(T value, Unit<F, UN...>) { return details::maker<Unit<F, UN...>>::make(value); }
	template <class T, class F, int... UN, class = details::if_arithmetic_t<T>>
	inline constexpr auto operator*(Unit<F, UN...>, T value) { return details::maker<Unit<F, UN...>>::make(value); }
	template <class T, size_t P, size_t Q, int... UN, class = details::if_arithmetic_t<T>>
	inline constexpr auto operator/(T value, Unit<Frac<P, Q>, UN...>) { return details::maker<Unit<Frac<Q, P>, -UN...>>::make(value); }
	template <class T, class F, int... UN, class = details::if_arithmetic_t<T>>
	inline constexpr auto operator/(Unit<F, UN...>, T value) { return details::maker<Unit<F, UN...>>::make(1/value); }

	template <class A, class UA, class BF, int... BN>
	inline constexpr auto operator*(QA a, Unit<BF, BN...>) { return details::maker<decltype(UA{} * Unit<BF, BN...>{})>::make(BF::q*a.value/BF::p); }
	template <class A, class UA, class BF, int... BN>
	inline constexpr auto operator*(Unit<BF, BN...>, QA a) { return details::maker<decltype(Unit<BF, BN...>{} * UA{})>::make(BF::q*a.value/BF::p); }
	template <class A, class UA, class BF, int... BN>
	inline constexpr auto operator/(QA a, Unit<BF, BN...>) { return details::maker<decltype(UA{} / Unit<BF, BN...>{})>::make(BF::p*a.value/BF::q); }
	template <class A, class UA, class BF, int... BN>
	inline constexpr auto operator/(Unit<BF, BN...>, QA a) { return details::maker<decltype(Unit<BF, BN...>{} / UA{})>::make(BF::q/(a.value*BF::p)); }


	template <class T, class U, size_t P, size_t Q>
	inline constexpr auto operator*(Quantity<T, U> q, Frac<P, Q> f) { return details::maker<decltype(U{}*f)>::make(Q*q.value/P); }
	template <class T, class U, size_t P, size_t Q>
	inline constexpr auto operator*(Frac<P, Q> f, Quantity<T, U> q) { return details::maker<decltype(f*U{})>::make(Q*q.value/P); }
	template <class T, class U, size_t P, size_t Q>
	inline constexpr auto operator/(Quantity<T, U> q, Frac<P, Q> f) { return details::maker<decltype(U{}/f)>::make(P*q.value/Q); }
	template <class T, class U, size_t P, size_t Q>
	inline constexpr auto operator/(Frac<P, Q> f, Quantity<T, U> q) { return details::maker<decltype(f/U{})>::make(Q/(q.value*P)); }


	template <class A, class B, class U> constexpr auto min(Quantity<A, U> a, Quantity<B, U> b) { using R = decltype(a+b); return a.value < b.value ? R(a) : R(b); }
	template <class A, class B, class U> constexpr auto max(Quantity<A, U> a, Quantity<B, U> b) { using R = decltype(a+b); return a.value < b.value ? R(b) : R(a); }


	template <class A, class UA>
	constexpr auto abs(QA v) { return QA{ std::abs(v.value) }; }

	template <class F, int... AN>
	constexpr auto sqrt(Unit<F, AN...>)
	{
		static_assert(details::all((AN%2 == 0)...), "Cannot take square root of odd-dimensioned Unit");
		constexpr auto frt = sqrt(F{});
		return Unit<std::decay_t<decltype(frt)>, (AN/2)...>{};
	}
	template <class F, int... AN>
	constexpr auto cbrt(Unit<F, AN...>)
	{
		static_assert(details::all((AN%3 == 0)...), "Cannot take cube root of non-multiple-of-three-dimensioned Unit");
		constexpr auto frt = cbrt(F{});
		return Unit<std::decay_t<decltype(frt)>, AN...>{};
	}

	template <class A, class UA> constexpr auto sqrt(QA a) { constexpr auto urt = sqrt(UA{}); return details::maker<decltype(urt)>::make(std::sqrt(a.value)); }
	template <class A, class UA> constexpr auto cbrt(QA a) { constexpr auto urt = cbrt(UA{}); return details::maker<decltype(urt)>::make(std::cbrt(a.value)); }

	template <class A, class UA> constexpr bool isfinite(QA a) { return std::isfinite(a.value); }

	template <class A, class UA> auto atan2(QA a, QA b) { return std::atan2(a.value, b.value); }


	static constexpr Unit<Frac<1>, 1, 0, 0, 0, 0, 0, 0> meter = {};
	static constexpr Unit<Frac<1>, 0, 1, 0, 0, 0, 0, 0> kilogram = {};
	static constexpr Unit<Frac<1>, 0, 0, 1, 0, 0, 0, 0> second = {};
	static constexpr Unit<Frac<1>, 0, 0, 0, 1, 0, 0, 0> Ampere = {};
	static constexpr Unit<Frac<1>, 0, 0, 0, 0, 1, 0, 0> Kelvin = {};

	static constexpr Unit<Frac<1>, 0, 0, -1, 0, 0, 0, 0> Hertz = {};
	static constexpr Unit<Frac<1>, 1, 1, -2, 0, 0, 0, 0> Newton = {};
	static constexpr auto litre = cube(deci*meter);

	template <class V> using Distance  = Quantity<V, std::decay_t<decltype(meter)>>;
	template <class V> using Time      = Quantity<V, std::decay_t<decltype(second)>>;
	template <class V> using Mass      = Quantity<V, std::decay_t<decltype(kilogram)>>;
	template <class V> using Area      = Quantity<V, std::decay_t<decltype(square(meter))>>;
	template <class V> using Volume    = Quantity<V, std::decay_t<decltype(cube(meter))>>;
	template <class V> using Speed     = Quantity<V, std::decay_t<decltype(meter / second)>>;
	template <class V> using Density   = Quantity<V, std::decay_t<decltype(kilogram / cube(meter))>>;
	template <class V> using Frequency = Quantity<V, std::decay_t<decltype(Hertz)>>;
	template <class V> using Force     = Quantity<V, std::decay_t<decltype(Newton)>>;
	template <class V> using Torque    = Quantity<V, std::decay_t<decltype(Newton*meter)>>;


	namespace float_literals
	{
		FLOAT_LITERAL(_m, meter);
		FLOAT_LITERAL(_m2, square(meter));
		FLOAT_LITERAL(_m3, cube(meter));
		FLOAT_LITERAL(_kg, kilogram);
		FLOAT_LITERAL(_s, second);
		FLOAT_LITERAL(_mps, meter/second);
		FLOAT_LITERAL(_kgpm3, kilogram/cube(meter));

		FLOAT_LITERAL(_cm, centi*meter);
		FLOAT_LITERAL(_mm, milli*meter);
		FLOAT_LITERAL(_g, milli*kilogram);
		FLOAT_LITERAL(_l, litre);
		FLOAT_LITERAL(_kgpl, kilogram/litre);
	}

	namespace imperial
	{
		static constexpr auto inch = meter*Frac< 254, 10000>{};
		static constexpr auto foot = inch*Frac<12>{};
		static constexpr auto yard = foot*Frac<3>{};
		/// etc
	}

	namespace typography
	{
		static constexpr auto point = imperial::inch*Frac<1, 72>();

		namespace float_literals
		{
			FLOAT_LITERAL(_pt, point);
		}
	}

	static_assert((1000*(milli*meter))/meter == 1);
	static_assert((1000*(milli*meter))/milli == 1*meter);
}

namespace std
{
	template <class T, class U>
	struct numeric_limits<units::Quantity<T, U>> : numeric_limits<T>
	{
		using Quantity = units::Quantity<T, U>;

		static constexpr Quantity min()           { return Quantity{ numeric_limits<T>::min() }; }
		static constexpr Quantity lowest()        { return Quantity{ numeric_limits<T>::lowest() }; }
		static constexpr Quantity max()           { return Quantity{ numeric_limits<T>::max() }; }
		static constexpr Quantity epsilon()       { return Quantity{ numeric_limits<T>::epsilon() }; }
		static constexpr Quantity round_error()   { return Quantity{ numeric_limits<T>::round_error() }; }
		static constexpr Quantity infinity()      { return Quantity{ numeric_limits<T>::infinity() }; }
		static constexpr Quantity quiet_NaN()     { return Quantity{ numeric_limits<T>::quiet_NaN() }; }
		static constexpr Quantity signaling_NaN() { return Quantity{ numeric_limits<T>::signaling_NaN() }; }
		static constexpr Quantity denorm_min()    { return Quantity{ numeric_limits<T>::denorm_min() }; }
	};
}

#pragma pop_macro("TEMPLATE_QA_QB")
#pragma pop_macro("QA")
#pragma pop_macro("QB")
#pragma pop_macro("FLOAT_LITERAL")

namespace uv
{
	template <class T>
	struct is_scalar;

	template <>
	struct is_scalar<units::Zero> : std::true_type { };
	template <size_t P, size_t Q>
	struct is_scalar<units::Frac<P, Q>> : std::true_type { };
	template <class F, int... UN>
	struct is_scalar<units::Unit<F, UN...>> : std::true_type { };
	template <class T, class U>
	struct is_scalar<units::Quantity<T, U>> : std::true_type { };
}
