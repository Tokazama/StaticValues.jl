function Base.splitprec(::Type{F}, i::SInteger) where {F<:AbstractFloat}
    hi = Base.truncbits(F(i), cld(precision(F), F(2)))
    ihi = oftype(i, hi)
    hi, i - ihi
end

truncbits(x::F, nb) where {F<:Union{SFloat16, SFloat32, SFloat64}} =
    truncmask(x, typemax(Base.uinttype(F)) << nb)

function truncmask(x::F, mask) where {F<:IEEESFloat}
    reinterpret(F, mask & reinterpret(Base.uinttype(F), x))
end

truncmask(x, mask) = x

truncbits(x, nb) = x

"""
    HPSVal
"""
struct HPSVal{V<:Tuple{H where H,L where L},T}
    function HPSVal{Tuple{H,L},T}() where {H,L,T}
        if T === eltype(H) === eltype(L)
            return new{Tuple{H,L},T}()
        else
            error("high and low precision values must were typed as $T but got,
                  $(eltype(H)) and $(eltype(H)).")
        end
    end
end

Base.@pure gethi(::HPSVal{Tuple{H,L},T}) where {H,L,T} = H::T
Base.@pure gethi(::Type{<:HPSVal{Tuple{H,L},T}}) where {H,L,T} = H::T

Base.@pure getlo(::HPSVal{Tuple{H,L},T}) where {H,L,T} = L::T
Base.@pure getlo(::Type{<:HPSVal{Tuple{H,L},T}}) where {H,L,T} = L::T

Base.eltype(::HPSVal{Tuple{H,L},T}) where {H,L,T} = T
Base.values(::HPSVal{Tuple{H,L},T}) where {H,L,T} = TwicePrecision{T}(H::T,L::T)

HPSVal{T}(hi::SNumber) where {X,T} = HPSVal{T}(hi, zero(hi))

function HPSVal(::Type{T}, hi::SNumber{H}, lo::SNumber{L}) where {T,H,L}
    if T === eltype(hi) === eltype(lo)
        return HPSVal{Tuple{H,L},T}()
    else
        return HPSVal(T, convert_static_val(T, hi), convert_static_val(T, lo))
    end
end

HPSVal(hi::SNumber, lo::SNumber) = HPSVal(promote_type(eltype(hi), eltype(lo)), hi, lo)
HPSVal(x::TwicePrecision{T}) where T = HPSVal{Tuple{x.hi,x.lo},T}()

HPSVal(::Type{T}, ::Tuple{SInteger{N},SInteger{D}}) where {N,D,T<:Union{Float16,Float32}} =
    HPSVal(T, convert_static_val(T, T, Val(T(N/D))))

HPSVal(::Type{T}, ::Tuple{SNumber{N},SNumber{D}}) where {T,N,D} = HPSVal{T}(N) / D

function HPSVal(::Type{T}, ::SNumber{X}) where {X,T}
    xT = convert(T, X)
    HPSVal{Tuple{xT,T(X - xT)},T}()
end
#HPSVal{T}(x::SNumber) where T = HPSVal{T}(x, zero(x))

HPSVal(x::SNumber) = HPSVal(eltype(x), x)

HPSVal(::Type{T}, i::SInteger{X}) where {X,T<:AbstractFloat} =
    HPSVal(T, canonicalize2(splitprec(T, i)...)...)


"""
    zhi, zlo = add12(x, y)

A high-precision representation of `x + y` for floating-point
numbers. Mathematically, `zhi + zlo = x + y`, where `zhi` contains the
most significant bits and `zlo` the least significant.
Because of the way floating-point numbers are printed, `lo` may not
look the way you might expect from the standpoint of decimal
representation, even though it is exact from the standpoint of binary
representation.

# Example:
```jldoctest
julia> SFloat64(1.0) + SFloat64(1.0001e-15)
1.000000000000001

julia> big(SVal(1.0)) + big(SVal(1.0001e-15))
1.000000000000001000100000000000020165767380775934141445417482375879192346701529

julia> hi, lo = Base.add12(SVal(1.0), SVal(1.0001e-15))
(1.000000000000001, -1.1012302462515652e-16)

julia> big(hi) + big(lo)
1.000000000000001000100000000000020165767380775934141445417482375879192346701529
```
`lo` differs from 1.0e-19 because `hi` is not exactly equal to
the first 16 decimal digits of the answer.

"""
function Base.add12(x::SNumber{X,T}, y::SNumber{Y,T}) where {X,Y,T}
    x, y = ifelse(abs(y) > abs(x), (y, x), (x, y))
    canonicalize2(x, y)
end
Base.add12(x, y) = add12(promote(x, y)...)


"""
    zhi, zlo = mul12(x, y)
A high-precision representation of `x * y` for floating-point
numbers. Mathematically, `zhi + zlo = x * y`, where `zhi` contains the
most significant bits and `zlo` the least significant.
Example:

```jldoctest
julia> x = SFloat32(π)
3.1415927f0

julia> x * x
9.869605f0

julia> SFloat64(x) * SFloat64(x)
9.869604950382893

julia> hi, lo = StaticValues.mul12(x, x)
(9.869605f0, -1.140092f-7)

julia> SFloat64(hi) + SFloat64(lo)
9.869604950382893
```
"""
function mul12(x::SFloat{X,T}, y::SFloat{Y,T}) where {X,Y,T<:AbstractFloat}
    h = x * y
    ifelse(iszero(h) | !isfinite(h), (h, h), canonicalize2(h, fma(x, y, h)))
end
mul12(x::SNumber{X,T}, y::SNumber{Y,T}) where {T,X,Y} = (p = x * y; (p, zero(p)))
mul12(x, y) = mul12(promote(x, y)...)


"""
    zhi, zlo = div12(x, y)
A high-precision representation of `x / y` for floating-point
numbers. Mathematically, `zhi + zlo ≈ x / y`, where `zhi` contains the
most significant bits and `zlo` the least significant.
Example:
```julia
julia> x, y = SFloat32(π), SFloat(3.1f0)
(3.1415927f0, 3.1f0)

julia> x / y
1.013417f0

julia> SFloat64(x) / SFloat64(y)
1.0134170444063078

julia> hi, lo = Base.div12(x, y)
(1.013417f0, 3.8867366f-8)

julia> SFloat64(hi) + SFloat64(lo)
1.0134170444063066
"""
function div12(x::SFloat{X,T}, y::SFloat{Y,T}) where {X,Y,T<:AbstractFloat}
    # We lose precision if any intermediate calculation results in a subnormal.
    # To prevent this from happening, standardize the values.
    xs, xe = frexp(x)
    ys, ye = frexp(y)
    r = xs / ys
    rh, rl = canonicalize2(Val{r}(), Val{-fma(r, ys, -xs)/ys}())
    ifelse(iszero(r) | !isfinite(r), (r, r), (ldexp(get(rh), xe-ye), ldexp(get(rl), xe-ye)))
end
div12(x::SNumber{X,T}, y::SNumber{Y,T}) where {X,Y,T} = (p = x / y; (p, zero(p)))
div12(x, y) = div12(promote(x, y)...)

SNumber(x::HPSVal{Tuple{H,L},T}) where {H,L,T} = SNumber(Val(H::T + L::T))

#---
# Numerator/Denominator constructors
HPSVal{T}(nd::Tuple{<:SNumber,<:SNumber}, nb::SInteger) where T =
    twiceprecision(HPSVal{T}(nd), nb)

# Truncating constructors. Useful for generating values that can be
# exactly multiplied by small integers.
function twiceprecision(v::SNumber, nb::SInteger) 
    hi = Base.truncbits(v, nb)
    HPSVal(hi, v-hi)
end

function twiceprecision(
    val::HPSVal{Tuple{H,L},T},
    nb::SInteger{N,NT}) where {T<:Union{Float16, Float32, Float64},H,L,N,NT<:Integer}
    hi = Base.truncbits(H::T, N::NT)
    HPSVal{Tuple{T(hi), T((H::T - hi) + L::T)},T}()
end

#---conversion
(::Type{T})(x::HPSVal{Tuple{H,L},T2}) where {T<:Number,T2,H,L} = T(H::T2 + L::T2)::T

(::Type{<:HPSVal{<:Any,T}})(x::HPSVal{<:Any,T}) where T = x
(::Type{<:HPSVal{<:Any,T1}})(x::HPSVal{Tuple{H,L},T2}) where {T1,H,L,T2} =
    HPSVal{Tuple{T1(H::T2)::T1,T1(L::T2)::T1},T1}()

#=
Base.convert(::Type{T}, x::HPSVal) where {T<:Number} = T(x)
Base.convert(::Type{HPSVal{T}}, x::Number) where {T} = HPSVal{T}(x)
Base.convert(::Type{HPSVal{T}}, x::SVal) where {T} = HPSVal{T}(x)
=#


Base.float(x::HPSVal{Tuple{H,L},T}) where {H,L,T<:AbstractFloat} = x
Base.float(x::HPSVal{Tuple{H,L},T}) where {H,L,T} = HPSVal{Tuple{float(H::T),float(L::T)},float(T)}()

#Base.big(::HPSVal{T,H,L}) where {T,H,L} = big(H) + big(L)

-(::HPSVal{Tuple{H,L},T}) where {H,L,T} = HPSVal{Tuple{-H::T,-L::T},T}()

Base.zero(::Type{<:HPSVal{<:Any,T}}) where {T} = HPSVal{Tuple{T(0),T(0)},T}()
Base.one(::Type{<:HPSVal{<:Any,T}}) where {T} = HPSVal{Tuple{T(1),T(1)},T}()


# Arithmetic

@inline function +(::HPSVal{Tuple{H,L},T}, y::SNumber) where {H,L,T}
    s_hi, s_lo = add12(SNumber(H::T), y)
    hnew, hlow = canonicalize2(s_hi, s_lo+L)
    HPSVal(hnew, hlow)
end

+(x::SNumber, y::HPSVal) = y+x

@inline function +(x::HPSVal{Tuple{Hx,Lx},T}, y::HPSVal{Tuple{Hy,Ly},T}) where {Hx,Lx,Hy,Ly,T}
    r = Hx::T + Hy::T
    s = abs(Hx::T) > abs(Hy::T) ? (((Hx::T - r) + Hy::T) + Ly::T) + Lx::T : (((Hy::T - r) + Hx::T) + Lx::T) + Ly::T
    hnew, lnew = canonicalize2(SNumber(r), SNumber(s))
    HPSVal(hnew,lnew)
end
+(x::HPSVal{Tuple{Hx,Lx},Tx}, y::HPSVal{Tuple{Hy,Ly},Ty}) where {Hx,Lx,Tx,Hy,Ly,Ty} = +(promote(x, y)...)

-(x::HPSVal, y::HPSVal) = x + (-y)
-(x::BaseNumber, y::HPSVal) = x + (-y)
-(x::HPSVal, y::BaseNumber) = x + (-y)

*(x::HPSVal, v::BaseNumber) = Base.TwicePrecision(x) * v

# FIXME
function *(x::HPSVal{Tuple{H,L},T}, v::SNumber{V,Tv}) where {T,H,L,V,Tv<:BaseNumber}
    V::Tv == SZero && return HPSVal{Tuple{H::T*V::Tv,L::T*V::Tv},T}()
    x * HPSVal(oftype(H::T * V::Tv, V::Tv))
end

function *(x::HPSVal{Tuple{H,L},T}, v::SNumber{V,Tv}) where {T<:Union{Float16, Float32, Float64},H,L,V,Tv}
    V::Tv == 0 && return HPSVal(SNumber(H::T*V::Tv), SNumber(L::T*V::Tv))
    nb = ceil(Int, log2(abs(v)))
    u = Base.truncbits(H::T, nb)
    HPSVal(canonicalize2(SNumber(u*V::Tv), SNumber(((H::T-u) + L::T)*V::Tv))...)
end

function *(x::HPSVal{Tuple{H,L},T1}, s::SInteger{V,T2}) where {H,L,T1<:Union{Float16, Float32, Float64},V,T2<:Integer}
    V::T2 == 0 && return HPSVal(SNumber(H::T1*V::T2), SNumber(L::T1*V::T2))
    nb = ceil(Int, log2(abs(s)))
    u = Base.truncbits(H::T1, get(nb))
    HPSVal(canonicalize2(SNumber(u*V::T2), SNumber((H::T1-u) + L::T1*V::T2))...)
end

*(v::Number, x::HPSVal) = x*v

@inline function *(x::HPSVal{Tuple{Hx,Lx},T}, y::HPSVal{Tuple{Hy,Ly},T}) where {Hx,Lx,Hy,Ly,T}
    zh, zl = mul12(SNumber(Hx::T), SNumber(Hy::T))
    hnew, lnew = canonicalize2(zh, SNumber(Hx::T * Ly::T + Lx::T * Hy::T) + zl)
    ret = HPSVal{Tuple{T(hnew)::T,T(lnew)::T},T}()
    ifelse(iszero(zh) | !isfinite(zh), HPSVal{Tuple{T(zh),T(zh)},T}(), ret)
end

*(x::HPSVal, y::HPSVal) = *(promote(x, y)...)

/(x::HPSVal{Tuple{H,L},T}, v::Number) where {H,L,T} =
    x / HPSVal(oftype(SNumber(H::T)/v, v))

function /(x::HPSVal{Tuple{Hx,Lx},T}, y::HPSVal{Tuple{Hy,Ly},T}) where {Hx,Lx,Hy,Ly,T}
    hi = SNumber(Hx::T / Hy::T)
    uh, ul = mul12(hi, SNumber(Hy::T))
    lo = ((((Hx - uh) - ul) + Lx) - hi*Ly)/Hy
    ret = HPSVal(canonicalize2(hi, lo)...)
    ifelse(iszero(hi) | !isfinite(hi), HPSVal(hi, hi), ret)
end

#nbitslen(r::StaticRange{T,B,E,S,F,L}) where {T,B,E,S,F,L} = nbitslen(T, L, F)
#@inline ($f)(x::Float64) = nan_dom_err(ccall(($(string(f)), libm), Float64, (Float64,), x), x) 

nbitslen(::Type{T}, l::SNumber, f::SNumber) where {T<:Union{Float16, Float32, Float64}} =
    min(nbithelper(T), nbitslen(l, f))

nbithelper(::Type{T}) where T = cld(SNumber(precision(T)), SNumber(2))

# The +1 here is for safety, because the precision of the significand
# is 1 bit higher than the number that are explicitly stored.
function nbitslen(l::SNumber, f::SNumber)
    if l < 2
        return SZero
    else
        return ceil(Int, log2(max(f-SOne, l-f))) + SOne
    end
end

function _rat(x::SNumber, y::SNumber, m::SNumber, a::SNumber, b::SNumber, c::SNumber, d::SNumber)
    f = trunc(Int, y)
    ynew = y
    ynew -= f
    anew = f*a + c
    cnew = a
    bnew = f*b + d
    dnew = b
    if max(abs(anew), abs(bnew)) <= Base.convert(Int,m)
        return cnew, dnew
    elseif oftype(x, anew)/oftype(x, bnew) == x
        return anew, bnew
    elseif ynew <= m
        ynew = inv(ynew)
        _rat(x, ynew, m, anew, bnew, cnew, dnew)
    else
        return anew, bnew
    end
end

function rat(v::SNumber{V,T}) where {V,T<:BaseNumber}
    y = v
    a = d = SOne(T)
    b = c = SZero(T)
    m = SNumber(maxintfloat(Base.narrow(T), Int))
    _rat(v, v, m, a, b, c, d)
end

<(x::HPSVal, y::SNumber) = <(x, HPSVal(y))
<(x::SNumber, y::HPSVal) = <(HPSVal(x), y)


<(x::HPSVal{Tuple{Hx,Lx},T}, y::HPSVal{Tuple{Hy,Ly},T}) where {T,Hx,Lx,Hy,Ly} =
    Hx::T < Hy::T || ((Hx::T == Hy::T) & (Lx::T < Ly::T))

Base.show(io::IO, r::HPSVal) = showsval(io, r)
Base.show(io::IO, ::MIME"text/plain", r::HPSVal) = showsval(io, r)

showsval(io::IO, r::HPSVal{Tuple{H,L},T}) where {T,H,L} = print(io, "HPSVal{$T}($H, $L)")
