Base.log(x::SReal{X}) where X = SFloat64{log(X)}()

"""
# StaticValues Examples
```jldoctest
julia> prevpow(2, 7)
4
julia> prevpow(2, 9)
8
julia> prevpow(5, 20)
5
julia> prevpow(4, 16)
16
```
SUInt(0x0000000000000004)
"""
function Base.prevpow(a::SReal, x::SReal)
    x < 1 && throw(DomainError(x, "`x` must be ≥ 1."))
    # See comment in nextpos() for a == special case.
    a == 2 && isa(x, Integer) && return _prevpow2(x)
    a <= 1 && throw(DomainError(a, "`a` must be greater than 1."))
    n = floor(Integer, log(a, x))
    p = a^(n+SOne)
    p <= x ? p : a^n
end

_prevpow2(x::Unsigned) = one(x) << unsigned((SInt(sizeof(eltype(x))) << SInt(3))-leading_zeros(x)-SOne)
_prevpow2(x::Integer) = convert(typeof(x), x < 0 ? -_prevpow2(unsigned(-x)) :
                                                        _prevpow2(unsigned(x)))
#=
    # only iterate over <:Integer
    for (ST2,BT2) in zip(static_integer, base_integer)
        @eval begin
            Base.round(::Type{$BT2}, ::$ST{V}) where V = $ST2{round($BT2, V::$BT)}()
        end
    end
end

Base.isfinite(s::SNumber) = isfinite(values(s))




#=
FIXME:
Weird inference error on gcdx

a = SInt(9)
b = SInt(-9)

julia> @inferred(gcdx(a,b))
(9, 0, -1)


a> a = SInt(4)
4

julia> b = SInt(-8)
-8

julia> @inferred(gcdx(a,b))
(4, 1, 0)

a = SInt(6)
b = SInt(-9)
julia> @inferred(gcdx(a,b))
ERROR: return type Tuple{SInt64{3},SInt64{-1},SInt64{-1}} does not match inferred return type Tuple{SInt64{_A} where _A,SInt64{_A} where _A,SInt64{_A} where _A}
=#

for (ST,BT) in zip(static_integer, base_integer)
    @eval begin
        function Base.invmod(n::$ST, m::$ST)
            g, x, y = gcdx(n, m)
            g != 1 && throw(DomainError((n, m), "Greatest common divisor is $g."))
            m == 0 && throw(DomainError(m, "`m` must not be 0."))
            # Note that m might be negative here.
            # For unsigned T, x might be close to typemax; add m to force a wrap-around.
            r = mod(x + m, m)
            # The postcondition is: mod(r * n, m) == mod(T(1), m) && div(r, m) == 0
            r
        end
    end
end

>>(x::SInteger, y::SInteger) = SInteger(>>(values(x), values(y)))

nbits16 = SInt(cld(precision(Float16), 2))
nbits32 = SInt(cld(precision(Float32), 2))
nbits64 = SInt(cld(precision(Float64), 2))

# lack of specificity in base requires that these be more verbosely written out
Base.nbitslen(::Type{Float16}, l::SReal{V1}, f::SReal{V2}) where {V1,V2} =
    min(nbits16, Base.nbitslen(l, f))
Base.nbitslen(::Type{Float32}, l::SReal{V1}, f::SReal{V2}) where {V1,V2} =
    min(nbits32, Base.nbitslen(l, f))
Base.nbitslen(::Type{Float64}, l::SReal{V1}, f::SReal{V2}) where {V1,V2} =
    min(nbits64, Base.nbitslen(l, f))

Base.nbitslen(::Type{<:SFloat16}, l::SReal{V1}, f::SReal{V2}) where {V1,V2} =
    min(nbits16, Base.nbitslen(l, f))
Base.nbitslen(::Type{<:SFloat32}, l::SReal{V1}, f::SReal{V2}) where {V1,V2} =
    min(nbits32, Base.nbitslen(l, f))
Base.nbitslen(::Type{<:SFloat64}, l::SReal{V1}, f::SReal{V2}) where {V1,V2} =
    min(nbits64, Base.nbitslen(l, f))


Base.AbstractFloat(x::SInteger) = SFloat64(x)

=#
# TODO
# - ispow2
# - isqrt
# - factorial
# - binomial
# - all string things
# - >> is type unstable if not same exact type
#
# we get the following for free
# - nextpow
# - prevpow
# - ndigits
