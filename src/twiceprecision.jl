import Base: twiceprecision, rat, narrow, isbetween

function splitprec(::Type{F}, i::SInteger) where {F<:AbstractFloat}
    hi = truncbits(F(i), cld(precision(F), F(2)))
    ihi = oftype(i, hi)
    hi, i - ihi
end

truncmask(x::SFloat16, mask) = SFloat16{reinterpret(Float16, values(mask) & reinterpret(UInt16, values(x)))}()
truncmask(x::SFloat32, mask) = SFloat32{reinterpret(Float32, values(mask) & reinterpret(UInt32, values(x)))}()
truncmask(x::SFloat64, mask) = SFloat64{reinterpret(Float64, values(mask) & reinterpret(UInt64, values(x)))}()

truncbits(x::F, nb) where {F<:Union{SFloat16, SFloat32, SFloat64}} =
    truncmask(x, Base.typemax(Base.uinttype(F)) << nb)


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

Base.@pure gethi(::TPVal{H,L}) where {H,L} = H()::H
Base.@pure gethi(::Type{<:TPVal{H,L}}) where {H,L} = H()::H

Base.@pure getlo(::TPVal{H,L}) where {H,L} = L()::L
Base.@pure getlo(::Type{<:TPVal{H,L}}) where {H,L} = L()::L

eltype(::TPVal{H,L}) where {H,L} = eltype(H)

Base.@pure values(::TPVal{H,L}) where {H,L} = H()::H + L()::L
Base.@pure values(::Type{<:TPVal{H,L}}) where {H,L} = H()::H + L()::L

TPVal(hi::T) where T = TPVal(hi, zero(eltype(hi)))

TPVal(hi::SReal, lo::SReal) = TPVal(promote(hi, lo)...)

TPVal(hi::SFloat64{H}, lo::SFloat64{L}) where {H,L} = TPVal{SFloat64{H},SFloat64{L}}()
TPVal(hi::SFloat32{H}, lo::SFloat32{L}) where {H,L} = TPVal{SFloat32{H},SFloat32{L}}()
TPVal(hi::SFloat16{H}, lo::SFloat16{L}) where {H,L} = TPVal{SFloat16{H},SFloat16{L}}()
TPVal(hi::SInt64{H}, lo::SInt64{L}) where {H,L} = TPVal{SInt64{H},SInt64{L}}()

TPVal(x::TwicePrecision{T}) where T = TPVal(x.hi, x.lo)

TPVal(::Type{T}, ::Tuple{N,D}) where {N<:SInteger,D<:SInteger,T<:Union{Float16,Float32}} =
    TPVal(ofeltype(T, N()/D()))

TPVal(::Type{T}, ::Tuple{N,D}) where {T,N<:SReal,D<:SReal} = TPVal(ofeltype(T, N())) / D()
TPVal(::Type{Float64}, nd::Tuple{N,D}) where {T,N<:SReal,D<:SReal} =
    TPVal(ofeltype(Float64, nd[1]::N)) / nd[2]::D


TPVal(::Type{T}, nd::Tuple, nb::Integer) where {T} = Base.twiceprecision(TPVal(T, nd), nb)

TPVal(hi::T, lo::T) where T<:BaseNumber = TPVal(SVal(hi), SVal(lo))

function TPVal(::Type{T}, x::SReal{X}) where {X,T}
    xT = ofeltype(T, x)
    TPVal(xT, ofeltype(T, x - xT))
end
#TPVal{T}(x::SReal) where T = TPVal{T}(x, zero(x))

TPVal(x::SReal) = TPVal(eltype(x), x)

TPVal(::Type{T}, i::SInteger{X}) where {X,T<:AbstractFloat} =
    TPVal(T, canonicalize2(splitprec(T, i)...)...)

function TPVal(::Type{T}, hi::H, lo::L) where {T,H<:SReal,L<:SReal}
    if T === eltype(hi) === eltype(lo)
        return TPVal{H,L}()
    else
        return TPVal(T, ofeltype(T, hi), ofeltype(T, lo))
    end
end

SReal(x::TPVal) = gethi(x) + getlo(x)

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
tpval(::Type{T}, frac::Tuple{<:SReal,<:SReal}, n::SInteger) where T = TPVal(T, frac, n)
tpval(::Type{T}, frac::Tuple{<:BaseNumber,<:BaseNumber}, n::Number) where T = TwicePrecision{T}(frac, values(n))

tpval(hi::T, lo::T) where T= TwicePrecision(hi, lo)
# Numerator/Denominator constructors
function tpval(::Type{T}, nd::Tuple{BaseInteger,BaseInteger}) where  {T<:Union{Float16,Float32}}
    n, d = nd
    TwicePrecision{T}(n/d)
end

function tpval(::Type{T}, nd::Tuple{<:SInteger,BaseInteger}) where  {T<:Union{Float16,Float32}}
    n, d = nd
    TwicePrecision{T}(values(n)/d)
end

function tpval(::Type{T}, nd::Tuple{BaseInteger,<:SInteger}) where  {T<:Union{Float16,Float32}}
    n, d = nd
    TwicePrecision{T}(n/values(d))
end

function tpval(::Type{T}, nd::Tuple{<:SInteger,<:SInteger}) where  {T<:Union{Float16,Float32}}
    n, d = nd
    tpval(T, n/d)
end

function tpval(::Type{T}, nd::Tuple{<:SReal,<:SReal}) where {T}
    n, d = nd
    TPVal(ofeltype(T, n)) / d
end

function tpval(::Type{T}, nd::Tuple{<:SReal,BaseReal}) where {T}
    n, d = nd
    TwicePrecision{T}(values(n)) / d
end

function tpval(::Type{T}, nd::Tuple{BaseReal,<:SReal}) where {T}
    n, d = nd
    TwicePrecision{T}(n) / values(d)
end

function tpval(::Type{T}, nd::Tuple{BaseReal,BaseReal}) where {T}
    n, d = nd
    TwicePrecision{T}(n) / d
end


#=
for N in (SReal,BaseReal)
for D in (SReal,BaseReal)
    if N == BaseReal || D == BaseReal
        @eval begin
            tpval(::Type{T}, x::Tuple{$N,$D}) where T = TwicePrecision{T}(values(x[1])) / values(x[2])
        end
    else
        @eval begin
            tpval(::Type{T}, x::Tuple{$N,$D}) where T = TPVal(ofeltype(T, x[1])) / x[2]
        end
    end
for I in (SInteger,BaseInteger)
    if N == BaseReal || D == BaseReal || I == BaseInteger
        @eval begin
            tpval(::Type{T}, x::Tuple{$N,$D}, nb::$I) where T =
                Base.twiceprecision(TwicePrecision{T}(values(x[1]),values(x[2])), values(nb))
        end
    else
        @eval begin
            tpval(::Type{T}, x::Tuple{$N,$D}, nb::$I) where T = Base.twiceprecision(TPVal(T, x[1], x[2]), nb)
        end
    end
end
end
end
=#

tpval(hi::SFloat64{H}, lo::SFloat64{L}) where {H,L} = TPVal{SFloat64{H},SFloat64{L}}()
tpval(hi::SFloat32{H}, lo::SFloat32{L}) where {H,L} = TPVal{SFloat32{H},SFloat32{L}}()
tpval(hi::SFloat16{H}, lo::SFloat16{L}) where {H,L} = TPVal{SFloat16{H},SFloat16{L}}()
tpval(x::SReal) = TPVal(x)
tpval(x::BaseNumber) = TwicePrecision(x)

tpval(nd::Tuple{N,D}, nb::Integer) where {T,N,D} = Base.twiceprecision(tpval(nd), nb)

#---
# Numerator/Denominator constructors
TPVal(nd::Tuple{<:SReal,<:SReal}, nb::SInteger) where T =
    Base.twiceprecision(TPVal(nd), nb)

# Truncating constructors. Useful for generating values that can be
# exactly multiplied by small integers.
function Base.twiceprecision(v::SReal, nb::SInteger) 
    hi = Base.truncbits(v, nb)
    TPVal(hi, v-hi)
end


for T in (SFloat16,SFloat32,SFloat64)
    @eval begin
        function Base.twiceprecision(val::TPVal{<:$T,<:$T}, b::SInteger)
            hi = Base.truncbits(gethi(val), b)
            TPVal(gethi(val), (gethi(val) - hi) + getlo(val))
        end
    end
end



#---conversion
TwicePrecision(::TPVal{H,L}) where {H,L,T} = TwicePrecision(values(H),values(L))

(::Type{T})(x::TPVal{H,L}) where {T<:Number,H,L} = T(values(x))::T

for (ST1,BT1) in S2B
    for (ST2,BT2) in S2B
        if ST1 == ST2
            @eval begin
                (::Type{<:TPVal{<:$ST1,<:$ST1}})(val::TPVal{$ST2{H},$ST2{L}}) where {H,L} = val
            end
        else
            @eval begin
                (::Type{<:TPVal{<:$ST1,<:$ST1}})(val::TPVal{$ST2{H},$ST2{L}}) where {H,L} =
                    TPVal{$ST1{$BT1(H::$BT2)},$ST1{$BT1(L::$BT2)}}()
            end
        end
    end
end

Base.float(x::TPVal{Tuple{H,L},T}) where {H,L,T<:AbstractFloat} = x
Base.float(x::TPVal{Tuple{H,L},T}) where {H,L,T} = TPVal{Tuple{float(H::T),float(L::T)},float(T)}()

#Base.big(::TPVal{T,H,L}) where {T,H,L} = big(H) + big(L)

-(x::TPVal) = TPVal(-gethi(H), -getlo(L))

Base.zero(::Type{<:TPVal{T}}) where {T} = TPVal(zero(T), zero(T))
Base.one(::Type{<:TPVal{T}}) where {T} = TPVal(one(T), one(T))


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
    s = abs(hx) > abs(y.hi) ? (((hx - r) + y.hi) + y.lo) + lx : (((y.hi - r) + hx) + lx) + y.lo
    hnew, lnew = canonicalize2(r, s)
    TwicePrecision(hnew, lnew)
end

@inline +(x::TwicePrecision, y::TPVal) = y + x


-(x::TPVal, y::TPVal) = x + (-y)
-(x::BaseReal, y::TPVal) = x + (-y)
-(x::TPVal, y::BaseReal) = x + (-y)


#
*(x::TPVal, v::BaseNumber) = TwicePrecision(x) * v
*(x::TPVal, v::TwicePrecision) = TwicePrecision(x) * v
*(v::TwicePrecision, x::TPVal) = x * v
*(v::Number, x::TPVal) = x * v

function *(x::TPVal{H,L}, v::SReal) where {H,L}
    v == 0 && return TPVal(H() * v, L() * v)
    x * TPVal(oftype(H() * v, v))
end

for T in (SFloat16, SFloat32, SFloat64)
    @eval begin
        function *(x::TPVal{H,L}, v::SInteger) where {H<:$T,L<:$T}
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

#= TODO: shouldn't need this anymore
function *(x::TPVal{H,L}, s::SInteger) where {H,L,T<:Union{Float16, Float32, Float64}}
    v == 0 && return TPVal(H::T1 * s, L::T1 * s)
    nb = ceil(Int, log2(abs(s)))
    u = Base.truncbits(H::T1, get(nb))
    TPVal(canonicalize2(SReal(u* s), SReal((H::T1-u) + L::T1*s))...)
end
=#

# TODO: check this

#*(x::TPVal{V1,T1}, y::TPVal{V2,T2}) where {V1,V2,T1,T2} = *(promote(x, y)...)
/(x::TPVal{H,L}, v::Real) where {H,L} = x / tpval(ofeltype(gethi(x)::H/v, v))


/(x::TPVal, v::BaseNumber) = x / TwicePrecision(oftype(gethi(x)/v, v))
/(x::TPVal, v::SNumber) = x / TPVal(ofeltype(gethi(x)/v, v))

/(v::BaseNumber, x::TPVal) = TwicePrecision(oftype(gethi(x)/v, v)) / x
/(v::SNumber, x::TPVal) = TPVal(ofeltype(gethi(x)/v, v)) / x

/(v::TwicePrecision, x::TPVal) = v / TwicePrecision(x)

/(v::TPVal, x::TwicePrecision) = TwicePrecision(v) / x




function /(x::TPVal{Hx,Lx}, y::TPVal{Hy,Ly}) where {Hx,Lx,Hy,Ly}
    hi = gethi(x) / gethi(y)
    uh, ul = Base.mul12(hi, gethi(y))
    lo = ((((gethi(x) - uh) - ul) + getlo(x)) - hi*getlo(y))/gethi(y)
    ret = TPVal(canonicalize2(hi, lo)...)
    ifelse(iszero(hi) | !isfinite(hi), TPVal(hi, hi), ret)
end

const nbits16 = SInt(Base.cld(precision(Float16), 2))
const nbits32 = SInt(Base.cld(precision(Float32), 2))
const nbits64 = SInt(Base.cld(precision(Float64), 2))

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

Base.nbitslen(len::SReal, offset::SReal) = len < 2 ? SZero : ceil(Int, log2(max(offset-SOne, len-offset))) + SOne


#nbitslen(r::StaticRange{T,B,E,S,F,L}) where {T,B,E,S,F,L} = nbitslen(T, L, F)
#@inline ($f)(x::Float64) = nan_dom_err(ccall(($(string(f)), libm), Float64, (Float64,), x), x) 

#

#nbithelper(::Type{T}) where T = cld(SReal(precision(T)), SReal(2))

#=
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
=#

narrow(::Type{T}) where {T<:SFloat} = SFloat64
narrow(::Type{<:SFloat64}) = SFloat32
narrow(::Type{<:SFloat32}) = SFloat16
narrow(::Type{<:SFloat16}) = SFloat16

#=
function _rat(x::SVal{X}, y::SVal{Y}, m::SVal{M}, a::SVal{A}, b::SVal{B}, c::SVal{C}, d::SVal{D}) where {X,Y,M,A,B,C,D}
    f = trunc(Int, y)
    ynew = y
    ynew -= f
    anew = f*a + c
    cnew = a
    bnew = f*b + d
    dnew = b
    if max(abs(anew), abs(bnew)) <= ofeltype(Int, m)
        return cnew, dnew
    elseif ofeltype(x, anew)/oftype(x, bnew) == x
        return anew, bnew
    elseif abs(ynew) <= m
        ynew = inv(ynew)
        _rat(x, ynew, m, anew, bnew, cnew, dnew)
    else
        return anew, bnew
    end
end
=#
#=
function rat(v::SReal)
    _rat(v, v, maxintfloat(narrow(typeof(v)), Int), SOne, SZero, SZero, SOne)
end

function _rat(x::SReal, y::SReal, m::SReal, a::SReal, b::SReal, c::SReal, d::SReal)
   while abs(y) <= m
        f = trunc(Int, y)
        y -= f
        a, c = f*a + c, a
        b, d = f*b + d, b
        max(abs(a), abs(b)) <= convert(Int,m) || return c, d
        ofeltype(x,a) / ofeltype(x,b) == x && break
        y = inv(y)
    end
    return a, b
end
=#
function rat(x::SReal)
    _getf(x, x, maxintfloat(narrow(typeof(x)), Int), SOne, SZero, SZero, SOne)
end

function _getf(x::SReal, y::SReal, m::SReal, a::SReal, b::SReal, c::SReal, d::SReal)
    _sub_y(trunc(Int, y), x, y, m, a, b, c, d)
end

function _sub_y(f::SReal, x::SReal, y::SReal, m::SReal, a::SReal, b::SReal, c::SReal, d::SReal)
    if abs(y) > m
        a, b
    else
        _update_abcd(f, x, y - f, m, a, b, c, d)
    end
end

function _update_abcd(f::SReal, x::SReal, y::SReal, m::SReal, a::SReal, b::SReal, c::SReal, d::SReal)
    _inv_y(x, y, m, f*a+c, f*b+d, a, b)
end

function _inv_y(x::SReal, y::SReal, m::SReal, a::SReal, b::SReal, c::SReal, d::SReal)
    if max(abs(a), abs(b)) > ofeltype(Int, m)
        c, d
    elseif ofeltype(x,a) / ofeltype(x,b) == x
        a, b
    else
        _getf(x, inv(y), m, a, b, c, d)
    end
end
#=
function _rat(x::SReal, y::SReal, m::SReal, a::SReal, b::SReal, c::SReal, d::SReal)
    if abs(y) <= m
        a, b
    else
        _ratsub(trunc(Int, y), x, y, m, a, b, c, d)
    end
end

function _ratsub(f::SReal, x::SReal, y::SReal, m::SReal, a::SReal, b::SReal, c::SReal, d::SReal)
    _ratsub2(x, y, m, f*a+c, a, f*b+d, b)
end

function _ratsub2(x::SReal, y::SReal, m::SReal, a::SReal, b::SReal, c::SReal, d::SReal)
    if max(abs(a), abs(b)) <= ofeltype(Int,m)
        c, d
    elseif oftype(x,a)/oftype(x,b) == x
        a, b
    else
        _rat(x, y, m, a, b, c, d)
    end
end
=#

# this should be taken care of using promotion
#<(x::TPVal, y::SReal) = <(x, y)
#<(x::SReal, y::TPVal) = <(TPVal(x), y)


<(x::TPVal{Hx,Lx}, y::TPVal{Hy,Ly}) where {Hx,Lx,Hy,Ly} =
    gethi(x)::Hx < gethi(y)::Hy || ((gethi(x)::Hx == gethi(y)::Hy) & (getlo(x)::Lx < getlo(y)::Ly))

Base.show(io::IO, r::TPVal) = showsval(io, r)
Base.show(io::IO, ::MIME"text/plain", r::TPVal) = showsval(io, r)

showsval(io::IO, r::TPVal{H,L}) where {H,L} = print(io, "TPVal($H, $L)")
