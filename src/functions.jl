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
SVal(val::SVal) = val

for (ST,BT) in zip(static_real, base_real)

    @eval begin
        Base.fma(::$ST{X}, ::$ST{Y}, ::$ST{Z}) where {X,Y,Z} =
            $ST{fma(X::$BT, Y::$BT, Z::$BT)}()

        #Base.muladd(::$ST{X}, ::$ST{Y}, ::$ST{Z}) where {X,Y,Z} = SVal(muladd(X::$BT, Y::$BT, Z::$BT))

        Base.div(::$ST{X}, ::$ST{Y}) where {X,Y} = $ST{div(X::$BT, Y::$BT)}()

        Base.fld(::$ST{X}, ::$ST{Y}) where {X,Y} = $ST{fld(X::$BT, Y::$BT)}()

        Base.cld(::$ST{X}, ::$ST{Y}) where {X,Y} = $ST{cld(X::$BT, Y::$BT)}()

        Base.rem(::$ST{X}, ::$ST{Y}) where {X,Y} = $ST{rem(X::$BT, Y::$BT)}()


        # TODO: figure out return type inference for these (if possible)
        (\)(::$ST{V1}, ::$ST{V2}) where {V1,V2} = SVal((\)(V1::$BT, V2::$BT))
        (^)(::$ST{V1}, ::$ST{V2}) where {V1,V2} = SVal((^)(V1::$BT, V2::$BT))
        Base.mod(::$ST{X}, ::$ST{Y}) where {X,Y} = SVal(mod(X::$BT, Y::$BT))
        Base.mod1(::$ST{X}, ::$ST{Y}) where {X,Y} = SVal(mod1(X::$BT, Y::$BT))
        Base.fld1(::$ST{X}, ::$ST{Y}) where {X,Y} = SVal(fld1(X::$BT, Y::$BT))

        seek_static_val(::Type{$BT}, val::Val{V}) where V = $ST{V}()
    end

    # f(static) --> Bool
    # f(static, static) --> static
    for (ST2,BT2) in zip(static_real, base_real)
        if BT == BT2
            @eval begin
                (::Type{<:$ST{<:Any}})(val::$ST2) = val
                (::Type{<:$ST{<:Any}})(val::$BT2) = $ST{val}()

                Base.promote_rule(::Type{<:$ST}, ::Type{$BT2}) = $BT
                Base.flipsign(::$ST{V1}, ::$ST2{V2}) where {V1,V2} = $ST{flipsign(V1::$BT,V2::$BT2)}()
                Base.copysign(::$ST{V1}, ::$ST2{V2}) where {V1,V2} = $ST{copysign(V1::$BT,V2::$BT2)}()

                # converts to the element type but does not change from static/non-static type
                ofeltype(::Type{$BT}, val::$ST) = val
                ofeltype(::Type{$BT}, val::$BT) = val
                ofeltype(::$BT, val::$ST) = val
                ofeltype(::$BT, val::$BT) = val

                ofeltype(::Type{<:$ST}, val::$ST) = val
                ofeltype(::Type{<:$ST}, val::$BT) = val
                ofeltype(::$ST, val::$ST) = val
                ofeltype(::$ST, val::$BT) = val

                (::Type{$BT2})(::$ST{V}) where V = V::$BT
            end
        else
            @eval begin
                ofeltype(::Type{$BT}, val::$ST2{V}) where V = $ST{$BT(V::$BT2)}()
                ofeltype(::Type{$BT}, val::$BT2) = $BT(val)
                ofeltype(::$BT, val::$ST2{V}) where V = $ST{$BT(V::$BT2)}()
                ofeltype(::$BT, val::$BT2) = $BT(val)

                ofeltype(::Type{$ST}, val::$ST2{V}) where V = $ST{$BT(V::$BT2)}()
                ofeltype(::Type{$ST}, val::$BT2) = $BT(val)
                ofeltype(::$ST, val::$ST2{V}) where V = $ST{$BT(V::$BT2)}()
                ofeltype(::$ST, val::$BT2) = $BT(val)

                (::Type{<:$ST{<:Any}})(::$ST2{V}) where V = $ST{$BT(V::$BT2)}()
                (::Type{<:$ST{<:Any}})(val::$BT2) = $ST{$BT(val)}()

                Base.promote_rule(::Type{<:$ST}, ::Type{$BT2}) = promote_type($BT, $BT2)
                Base.flipsign(::$ST{V1}, ::$ST2{V2}) where {V1,V2} = $ST{flipsign(V1::$BT,V2::$BT2)}()
                Base.copysign(::$ST{V1}, ::$ST2{V2}) where {V1,V2} = $ST{copysign(V1::$BT,V2::$BT2)}()


                (::Type{$BT2})(::$ST{V}) where V = $BT2(V::$BT)

                # Given Val of different type convert to SVal
            end
        end
    end

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
        (/)(::$ST{V1}, ::$ST{V2}) where {V1,V2} = SFloat64{(/)(V1::$BT, V2::$BT)}()
        Base.log2(::$ST{V}) where V = SFloat64{log(V::$BT)/log(2)}()
        Base.log10(::$ST{V}) where V = SFloat64{log(V::$BT)/log(10)}()
        Base.lcm(a::$ST{A}, b::$ST{B}) where {A,B} = $ST{lcm(A::$BT,B::$BT)}()

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


Base.promote_eltype(x::SVal, y::BaseNumber) = promote_type(eltype(x), eltype(y))
Base.promote_eltype(x::BaseNumber, y::SVal) = promote_type(eltype(x), eltype(y))
Base.promote_eltype(x::SVal, y::SVal) = promote_type(eltype(x), eltype(y))

Base.promote_eltype(x::Type{<:SVal}, y::Type{<:SVal}) = promote_type(eltype(x), eltype(y))

promote_toeltype(x, y) = promote_toeltype(promote_eltype(x, y), x, y)
promote_toeltype(::Type{T}, x, y) where T = ofeltype(T, x), ofeltype(T, y)

Base.trunc(::Type{T}, x::SVal) where T = SVal(trunc(T, values(x)))

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
