import Base: twiceprecision

function splitprec(::Type{F}, i::SInteger) where {F<:AbstractFloat}
    hi = truncbits(F(i), cld(precision(F), F(2)))
    ihi = oftype(i, hi)
    hi, i - ihi
end

truncmask(x::SFloat16, mask) = SFloat16{reinterpret(Float16, values(mask) & reinterpret(UInt16, values(x)))}()
truncmask(x::SFloat32, mask) = SFloat32{reinterpret(Float32, values(mask) & reinterpret(UInt32, values(x)))}()
truncmask(x::SFloat64, mask) = SFloat64{reinterpret(Float64, values(mask) & reinterpret(UInt64, values(x)))}()

truncbits(x::F, nb) where {F<:Union{SFloat16, SFloat32, SFloat64}} = truncmask(x, typemax(Base.uinttype(F)) << nb)

"""
    TPVal{Tuple{H,L},T}
"""
struct TPVal{H,L}
    function TPVal{H,L}() where {H,L}
        if eltype(H) == eltype(L)
            return new{H,L}()
        else
            error("high and low precision values must be of the same type but got,
                  $(eltype(H)) and $(eltype(L)).")
        end
    end
end

const AbstractTPI64 = Union{TPVal{<:SInt64,<:SInt64},TwicePrecision{Int64}}
const AbstractTPF64 = Union{TPVal{<:SFloat64,<:SFloat64},TwicePrecision{Float64}}
const AbstractTPF32 = Union{TPVal{<:SFloat32,<:SFloat32},TwicePrecision{Float32}}
const AbstractTPF16 = Union{TPVal{<:SFloat16,<:SFloat16},TwicePrecision{Float16}}

const AbstractTwicePrecision = Union{AbstractTPI64,AbstractTPF64,AbstractTPF32,AbstractTPF16}

"gethi - retrieve the the hi bits (most significant bits) from a twice precision number"
function gethi end

"getlo - retrieve the the low bits (least significant bits) from a twice precision number"
function getlo end


gethi(x::TwicePrecision) = x.hi
getlo(x::TwicePrecision) = x.lo

Base.@pure gethi(::TPVal{H,L}) where {H,L} = H()
Base.@pure gethi(::Type{<:TPVal{H,L}}) where {H,L} = H()

Base.@pure getlo(::TPVal{H,L}) where {H,L} = L()
Base.@pure getlo(::Type{<:TPVal{H,L}}) where {H,L} = L()

eltype(::TPVal{H,L}) where {H,L} = eltype(H)

Base.@pure values(::TPVal{H,L}) where {H,L} = H()::H + L()::L
Base.@pure values(::Type{<:TPVal{H,L}}) where {H,L} = H()::H + L()::L

TPVal(hi::T) where T = TPVal(hi, zero(T))

TPVal(hi::SReal, lo::SReal) = TPVal(promote(hi, lo)...)

TPVal(hi::SFloat64{H}, lo::SFloat64{L}) where {H,L} = TPVal{SFloat64{H},SFloat64{L}}()
TPVal(hi::SFloat32{H}, lo::SFloat32{L}) where {H,L} = TPVal{SFloat32{H},SFloat32{L}}()
TPVal(hi::SFloat16{H}, lo::SFloat16{L}) where {H,L} = TPVal{SFloat16{H},SFloat16{L}}()
TPVal(hi::SInt64{H}, lo::SInt64{L}) where {H,L} = TPVal{SInt64{H},SInt64{L}}()

TPVal(x::TwicePrecision{T}) where T = TPVal(x.hi, x.lo)

TPVal(::Type{T}, ::Tuple{N,D}) where {N<:SInteger,D<:SInteger,T<:Union{Float16,Float32}} =
    TPVal(ofeltype(T, N()/D()))

TPVal(::Type{T}, ::Tuple{N,D}) where {T,N<:SReal,D<:SReal} = TPVal(ofeltype(T, N())) / D()

TPVal(hi::T, lo::T) where T<:BaseNumber = TPVal(SVal(hi), SVal(lo))

function TPVal(::Type{T}, x::SReal{X}) where {X,T}
    xT = ofeltype(T, x)
    TPVal(xT, ofeltype(T, X - xT))
end
#TPVal{T}(x::SReal) where T = TPVal{T}(x, zero(x))

TPVal(x::SReal) = TPVal(eltype(x), x)

TPVal(::Type{T}, i::SInteger{X}) where {X,T<:AbstractFloat} =
    TPVal(T, canonicalize2(splitprec(T, i)...)...)

function TPVal(::Type{T}, hi::SReal{H}, lo::SReal{L}) where {T,H,L}
    if T === eltype(hi) === eltype(lo)
        return TPVal{Tuple{H,L},T}()
    else
        return TPVal(T, convert_static_val(T, hi), convert_static_val(T, lo))
    end
end

SReal(x::TPVal) where {H,L,T} = SReal(Val(H::T + L::T))

#---
# Handle ambiguities created by mixed static base types

"""
    tpval

constructs a twice precision value 
"""
tpval(::Type{T}, hi::Real) where T = tpval(T, hi, zero(hi))

# ↓ this accounts for mixed static/base types
tpval(::Type{T}, hi::Real, lo::Real) where T = tpval(T, promote(hi, lo)...)

tpval(::Type{T}, hi::T, lo::T) where T<:BaseReal = TwicePrecision{T}(hi, lo)

tpval(hi::SFloat64{H}, lo::SFloat64{L}) where {H,L} = TPVal{SFloat64{H},SFloat64{L}}()
tpval(hi::SFloat32{H}, lo::SFloat32{L}) where {H,L} = TPVal{SFloat32{H},SFloat32{L}}()
tpval(hi::SFloat16{H}, lo::SFloat16{L}) where {H,L} = TPVal{SFloat16{H},SFloat16{L}}()
tpval(::Type{T}, frac::Tuple{N,D}) where {T,N,D} = tpval(ofeltype(T, frac[1]::N/frac[2]::D))

tpval(nd::Tuple{N,D}, nb::Integer) where {T,N,D} = twiceprecision(tpval(nd), nb)

#---
# Numerator/Denominator constructors
TPVal(nd::Tuple{<:SReal,<:SReal}, nb::SInteger) where T =
    twiceprecision(TPVal(nd), nb)

# Truncating constructors. Useful for generating values that can be
# exactly multiplied by small integers.
function Base.twiceprecision(v::SReal, nb::SInteger) 
    hi = Base.truncbits(v, nb)
    TPVal(hi, v-hi)
end


function Base.twiceprecision(val::TPVal{Tuple{H,L},T}, b::SInteger{N}) where {T<:Union{Float16, Float32, Float64},H,L,N}
    hi = Base.truncbits(H::T, N)
    TPVal{Tuple{T(hi), T((H::T - hi) + L::T)},T}()
end



#---conversion
(::Type{T})(x::TPVal{Tuple{H,L},T2}) where {T<:Number,T2,H,L} = T(H::T2 + L::T2)::T

TwicePrecision(::TPVal{Tuple{H,L},T}) where {H,L,T} = TwicePrecision{T}(H::T,L::T)

(::Type{<:TPVal{<:Any,T}})(x::TPVal{<:Any,T}) where T = x
(::Type{<:TPVal{<:Any,T1}})(x::TPVal{Tuple{H,L},T2}) where {T1,H,L,T2} =
    TPVal{Tuple{T1(H::T2)::T1,T1(L::T2)::T1},T1}()

#=
Base.convert(::Type{T}, x::TPVal) where {T<:Number} = T(x)
Base.convert(::Type{TPVal{T}}, x::Number) where {T} = TPVal{T}(x)
Base.convert(::Type{TPVal{T}}, x::SVal) where {T} = TPVal{T}(x)
=#


Base.float(x::TPVal{Tuple{H,L},T}) where {H,L,T<:AbstractFloat} = x
Base.float(x::TPVal{Tuple{H,L},T}) where {H,L,T} = TPVal{Tuple{float(H::T),float(L::T)},float(T)}()

#Base.big(::TPVal{T,H,L}) where {T,H,L} = big(H) + big(L)

-(::TPVal{Tuple{H,L},T}) where {H,L,T} = TPVal{Tuple{-H::T,-L::T},T}()

Base.zero(::Type{<:TPVal{<:Any,T}}) where {T} = TPVal{Tuple{T(0),T(0)},T}()
Base.one(::Type{<:TPVal{<:Any,T}}) where {T} = TPVal{Tuple{T(1),T(1)},T}()


# Arithmetic

@inline function +(x::TPVal{H,L}, y::SReal) where {H,L,T}
    s_hi, s_lo = Base.add12(gethi(x)::H, y)
    hnew, hlow = Base.canonicalize2(s_hi, s_lo+getlo()::L)
    TPVal(hnew, hlow)
end

+(x::SReal, y::TPVal) = y+x

@inline function +(x::TPVal{Hx,Lx}, y::TPVal{Hy,Ly}) where {Hx,Lx,Hy,Ly}
    hx = gethi(x)::Hx
    lx = getlo(x)::Lx
    hy = gethi(y)::Hy
    ly = getlo(y)::Ly
    r = hx + hy
    s = abs(hx) > abs(hy) ? (((hx - r) + hy) + ly) + lx : (((hy - r) + hx) + lx) + ly
    hnew, lnew = canonicalize2(r, s)
    TPVal(hnew,lnew)
end


@inline function +(x::TPVal{H,L}, y::TwicePrecision) where {H,L}
    hx = gethi(x)::H
    lx = getlo(x)::L
    r = hx + y.hi
    s = abs(hx) > abs(y.hi) ? (((hx - r) + y.hi) + y.lo) + lx::T : (((y.hi - r) + hx) + lx) + y.lo
    hnew, lnew = canonicalize2(r, s)
    TwicePrecision(hnew, lnew)
end

@inline +(x::TwicePrecision, y::TPVal) = y + x


-(x::TPVal, y::TPVal) = x + (-y)
-(x::BaseReal, y::TPVal) = x + (-y)
-(x::TPVal, y::BaseReal) = x + (-y)


#
*(x::TPVal, v::BaseReal) = TwicePrecision(x) * v
*(x::TPVal, v::TwicePrecision) = TwicePrecision(x) * v
*(v::TwicePrecision, x::TPVal) = x * v
*(v::Number, x::TPVal) = x * v

function *(x::TPVal{H,L}, v::Number) where {H,L}
    v == 0 && return TPVal(H() * v, L() * v)
    x * TPVal(oftype(H() * v, v))
end

for T in (SFloat16, SFloat32, SFloat64)
    @eval begin
        function *(x::TPVal{H,L}, v::Integer) where {H<:$T,L<:$T}
            v == 0 && return TPVal(H() * v, L() * v)
            nb = ceil(Int, log2(abs(v)))
            u = Base.truncbits(H(), nb)
            tpval(canonicalize2(u * v, H() - u + L() * v)...)
        end
    end
end

@inline function *(x::TPVal{Hx,Lx}, y::TPVal{Hy,Ly}) where {Hx,Lx,Hy,Ly}
    zh, zl = Base.mul12(gethi(x), gethi(y))
    ifelse(iszero(zh) | !isfinite(zh), TPVal(zh, zh), TPVal(canonicalize2(zh, gethi(x) * getlo(y) + getlo(x) * gethi(y))...))
end

function *(x::TPVal{Tuple{H,L},T}, s::SInteger) where {H,L,T<:Union{Float16, Float32, Float64}}
    v == 0 && return TPVal(H::T1 * s, L::T1 * s)
    nb = ceil(Int, log2(abs(s)))
    u = Base.truncbits(H::T1, get(nb))
    TPVal(canonicalize2(SReal(u* s), SReal((H::T1-u) + L::T1*s))...)
end


# TODO: check this

#*(x::TPVal{V1,T1}, y::TPVal{V2,T2}) where {V1,V2,T1,T2} = *(promote(x, y)...)

/(x::TPVal, v::Real) = x / tpval(oftype(gethi(x)/v, v))

function /(x::TPVal{Hx,Lx}, y::TPVal{Hy,Ly}) where {Hx,Lx,Hy,Ly}
    hi = SReal(gethi(x) / gethi(y))
    uh, ul = Base.mul12(hi, gethi(y))
    lo = ((((gethi(x) - uh) - ul) + getlo(x)) - hi*getlo(y))/gethi(y)
    ret = TPVal(canonicalize2(hi, lo)...)
    ifelse(iszero(hi) | !isfinite(hi), TPVal(hi, hi), ret)
end

#nbitslen(r::StaticRange{T,B,E,S,F,L}) where {T,B,E,S,F,L} = nbitslen(T, L, F)
#@inline ($f)(x::Float64) = nan_dom_err(ccall(($(string(f)), libm), Float64, (Float64,), x), x) 

nbitslen(::Type{T}, l::SReal, f::SReal) where {T<:Union{Float16, Float32, Float64}} =
    min(nbithelper(T), nbitslen(l, f))

nbithelper(::Type{T}) where T = cld(SReal(precision(T)), SReal(2))

# The +1 here is for safety, because the precision of the significand
# is 1 bit higher than the number that are explicitly stored.
function nbitslen(l::SReal, f::SReal)
    if l < 2
        return SZero
    else
        return ceil(Int, log2(max(f-SOne, l-f))) + SOne
    end
end

function _rat(x::SReal, y::SReal, m::SReal, a::SReal, b::SReal, c::SReal, d::SReal)
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

Base.narrow(::Type{T}) where {T<:SFloat} = SFloat64
Base.narrow(::Type{<:SFloat64}) = SFloat32
Base.narrow(::Type{<:SFloat32}) = SFloat16
Base.narrow(::Type{<:SFloat16}) = SFloat16

function Base.rat(v::SReal)
    a = d = one(v)
    b = c = zero(v)
    m = SReal(maxintfloat(Base.narrow(typeof(v)), Int))
    _rat(v, v, m, a, b, c, d)
end

# this should be taken care of using promotion
#<(x::TPVal, y::SReal) = <(x, y)
#<(x::SReal, y::TPVal) = <(TPVal(x), y)


<(x::TPVal{Hx,Lx}, y::TPVal{Hy,Ly}) where {Hx,Lx,Hy,Ly} =
    gethi(x)::Hx < gethi(y)::Hy || ((gethi(x)::Hx == gethi(y)::Hy) & (getlo(x)::Lx < getlo(y)::Ly))

Base.show(io::IO, r::TPVal) = showsval(io, r)
Base.show(io::IO, ::MIME"text/plain", r::TPVal) = showsval(io, r)

showsval(io::IO, r::TPVal{H,L}) where {H,L} = print(io, "TPVal($H, $L)")
