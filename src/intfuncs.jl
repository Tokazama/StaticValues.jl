Base.flipsign(x::SSigned, y::SFloat16{Y}) where Y = flipsign(x, SInt16{Base.bitcast(Int16, Y::Float16)}())
Base.flipsign(x::SSigned, y::SFloat32{Y}) where Y = flipsign(x, SInt32{Base.bitcast(Int32, Y::Float32)}())
Base.flipsign(x::SSigned, y::SFloat64{Y}) where Y = flipsign(x, SInt{Base.bitcast(Int64, Y::Float64)}())
#Base.flipsign(x::SSigned, y::Real) = flipsign(x, -oftype(x, signbit(y)))

Base.copysign(x::SSigned, y::SSigned)  = flipsign(x, x ⊻ y)
Base.copysign(x::SSigned, y::SFloat16) = copysign(x, SInt16{Base.bitcast(Int16, Y::Float16)}())
Base.copysign(x::SSigned, y::SFloat32) = copysign(x, SInt32{Base.bitcast(Int32, Y::Float32)}())
Base.copysign(x::SSigned, y::SFloat64) = copysign(x, SInt{Base.bitcast(Int64, Y::Float64)}())
#Base.copysign(x::SSigned, y::SReal)    = copysign(x, -oftype(x, signbit(y)))


Base.:(==)(x::SSigned,   y::SUnsigned) = (x >= 0) & (unsigned(x) == y)
Base.:(==)(x::SUnsigned, y::SSigned  ) = (y >= 0) & (x == unsigned(y))
Base.:(<)( x::SSigned,   y::SUnsigned) = (x <  0) | (unsigned(x) <  y)
Base.:(<)( x::SUnsigned, y::SSigned  ) = (y >= 0) & (x <  unsigned(y))
Base.:(<=)(x::SSigned,   y::SUnsigned) = (x <  0) | (unsigned(x) <= y)
Base.:(<=)(x::SUnsigned, y::SSigned ) = (y >= 0) & (x <= unsigned(y))
# unsigned shift counts always shift in the same direction

Base.rem2pi(::SInt32{X}, r::RoundingMode) where X = SFloat64{rem2pi(X::Int32, r)}()
Base.rem2pi(::SInt64{X}, r::RoundingMode) where X = SFloat64{rem2pi(X::Int64, r)}()


Base.invmod(::SInteger, ::SIntegerZeroType) = throw(DomainError(m, "`m` must not be 0."))

for (ST,BT) in SI2BI
    @eval begin
        Base.:(>>)(x::$ST{X}, y::SUnsigned{Y}) where {X,Y} = $ST{>>(X::$BT, Y)}()
        Base.:(<<)(x::$ST{X}, y::SUnsigned{Y}) where {X,Y} = $ST{<<(X::$BT, Y)}()
        Base.:(>>>)(x::$ST{X}, y::SUnsigned{Y}) where {X,Y} = $ST{>>>(X::$BT, Y)}()
        Base.:(^)(::$ST{X}, y::SUnsigned{Y}) where {X,Y} = $ST{^(X::$BT, Y::$BT)}()
        Base.lcm(a::$ST{A}, b::$ST{B}) where {A,B} = $ST{lcm(A::$BT,B::$BT)}()
        (/)(x::$ST, y::$ST) where {T<:Integer} = float(x) / float(y)
        function Base.invmod(n::$ST, m::$ST)
            g, x, y = gcdx(n, m)
            g != 1 && throw(DomainError((n, m), "Greatest common divisor is $g."))
            # Note that m might be negative here.
            # For unsigned T, x might be close to typemax; add m to force a wrap-around.
            r = mod(x + m, m)
            # The postcondition is: mod(r * n, m) == mod(T(1), m) && div(r, m) == 0
            r
        end
    end
end
Base.:(>>)(x::SInteger, y::SInt) =
    ifelse(0 <= y, x >> unsigned(y), x << unsigned(-y))
Base.:(<<)(x::SInteger, y::SInt) =
    ifelse(0 <= y, x << unsigned(y), x >> unsigned(-y))
Base.:(>>>)(x::SInteger, y::SInt) =
    ifelse(0 <= y, x >>> unsigned(y), x << unsigned(-y))

Base.:(<<)(x::SInteger, y::BaseInteger) = <<(values(x), y)

#
Base.:(>>)(x::SInteger, y::Int) = >>(values(x), y)
Base.:(>>)(x::BaseInteger, y::SInt) = >>(x, values(y))
Base.:(<<)(x::SInteger, y::Int) = <<(values(x), y)
Base.:(<<)(x::BaseInteger, y::SInt) = <<(x, values(y))
Base.:(>>>)(x::SInteger, y::Int) = >>(values(x), y)
Base.:(>>>)(x::BaseInteger, y::SInt) = >>(x, values(y))


#SIntegerNegOneType = Union{SInt8{-Int8(1)},SInt16{-Int16(1)},SInt32{-Int32(1)},SInt64{-1},SInt128{-Int128(1)}}


Base.div(x::SSigned, y::SUnsigned) = flipsign(signed(div(unsigned(abs(x)), y)), x)
Base.div(x::SUnsigned, y::SSigned) = unsigned(flipsign(signed(div(x, unsigned(abs(y)))), y))

Base.rem(x::SSigned, y::SUnsigned) = flipsign(signed(rem(unsigned(abs(x)), y)), x)
Base.rem(x::SUnsigned, y::SSigned) = rem(x, unsigned(abs(y)))

Base.fld(x::SSigned, y::SUnsigned) = div(x, y) - (signbit(x) & (rem(x, y) != 0))
Base.fld(x::SUnsigned, y::SSigned) = div(x, y) - (signbit(y) & (rem(x, y) != 0))

Base.bswap(x::SInt8) = x
Base.bswap(x::SUInt8) = x
# TODO: bswap other stuff (probably unnecessary, but why not)

@inline _gcdxrem(x::Tuple{A,B}) where {A<:SInteger,B<:SInteger} = (x[2], rem(x[1], x[2]))
@inline _gcdxsub(q::SInteger, x::Tuple{A,B}) where {A<:SInteger,B<:SInteger} = (x[2], x[1] - q*x[2])

@inline function _gcdxwhile(
    ab::Tuple{A,B},s::Tuple{S0,S1},t::Tuple{T0,T1}
   ) where {A<:SInteger,B<:SInteger,S0<:SInteger,S1<:SInteger,T0<:SInteger,T1<:SInteger}
    _gcdxwhile(_gcdxrem(ab),
               _gcdxsub(div(ab[1],ab[2]), s),
               _gcdxsub(div(ab[1],ab[2]), t))
end

@inline function _gcdxwhile(ab::Tuple{A,B},
                            s::Tuple{S0,S1},
                            t::Tuple{T0,T1}
   ) where {A<:SInteger,B<:SIntegerZeroType,
            S0<:SInteger,S1<:SInteger,
            T0<:SInteger,T1<:SInteger}
    if ab[1] < 0
        return (-ab[1], -s[1], -t[1])
    else
        return (ab[1], s[1], t[1])
    end
end
function Base.gcdx(a::SInteger, b::SInteger)
    _gcdxwhile((a, b), (oneunit(a), zero(b)), (zero(a), oneunit(b)))
end

# only alternating `A` and `B` so that they eventually get promoted
#Tuple{SInt64{_1} where _1,Union{SInt64{-1}, SInt64{1}},SInt64{0}}

"""
    gcd(::Union{Integer,SInteger}, Union{Integer,SInteger})

# Examples
```jldoctest
using StaticValues, Test

julia> gcd(SInt(6), SInt(9)) === SInt(3)
true

julia> gcd(SInt(6),SInt(-9)) === SInt(3)
true

julia> gcd(SInt(6), -9) === 3
true

julia> gcd(6,SInt(-9)) === 3
true

julia> @inferred(gcd(SInt(6), SInt(-9)))
3
```
"""
function Base.gcd(a::SInteger, b::SInteger)
    za = trailing_zeros(a)
    zb = trailing_zeros(b)
    k = min(za, zb)
    u, v = _gcdwhile(unsigned(abs(a >> za)), unsigned(abs(b >> zb)))
    r = u << k
    # T(r) would throw InexactError; we want OverflowError instead
    r > typemax(eltype(a)) && Base.__throw_gcd_overflow(a, b)
    r % typeof(a)
end

@inline _gcdwhile(u::U, v::U) where {U<:SInteger} = u, v
function _gcdwhile(u::SInteger, v::SInteger)
    if u > v
        _gcdwhile(v, u)
    end
    _gcdwhile(u, (v-u) >> trailing_zeros(v-u))
end

Base.gcd(a::SIntegerZeroType, b::SInteger) = abs(b)
Base.gcd(a::SIntegerZeroType, b::SIntegerZeroType) = abs(b)
Base.gcd(a::SInteger,         b::SIntegerZeroType) = abs(a)


"""
# StaticValues Examples
```jldoctest
julia> powermod(SInt(2), SInt(6), SInt(5)) === SInt(4)
true
```
"""
#=
Base.powermod(x::SInteger, p::SInteger, m::SInteger) =
    oftype(m, _powermod(one(m), p, m, prevpow(oftype(p, 2), p),oftype(m, mod(x,m))))

function _powermod(r::SInteger, p::SInteger, m::SInteger, t::SInteger, b::SInteger)
    if p >= t
        _powermod(mod(widemul(r,b), m), p - t, m, t, b)
    else
        _powermod2(r, p, m, t >>> SOne, b)
    end
end

Base.powermod(x::SInteger, p::SIntegerZeroType, m::SInteger) = mod(one(m), m)
Base.powermod(x::SInteger, p::SIntegerNegOneType, m::SInteger) = powermod(invmod(x, m), -p, m)
Base.powermod(x::SInteger, p::SInteger, m::SIntegerOneType) = zero(m)
Base.powermod(x::SInteger, p::SInteger, m::SIntegerNegOneType) = zero(m)


function _powermod2(r::SInteger, p::SInteger, m::SInteger, t::SInteger, b::SInteger)
    if t <= 0
        return r
    else
        return _powermod(mod(widemul(r, r), m), p, m, t, b)
    end
end

# there's not static bigint, so can't widen past 128
function _powermod2(r::SInt128, p::SInteger, m::SInteger, t::SInteger, b::SInteger)
    if t <= 0
        return r
    else
        return _powermod(mod(r*r, m), p, m, t, b)
    end
end

function _powermod(r::SInt128, p::SInteger, m::SInteger, t::SInteger, b::SInteger)
    if p >= t
        _powermod(mod(r * b, m), p - t, m, t, b)
    else
        _powermod2(r, p, m, t >>> SOne, b)
    end
end
=#
#_prevpow2(x::Unsigned) = one(x) << unsigned((sizeof(x)<<3)-leading_zeros(x)-SOne)
#_prevpow2(x::Integer) = reinterpret(typeof(x), x < 0 ? -_prevpow2(unsigned(-x)) : _prevpow2(unsigned(x)))
