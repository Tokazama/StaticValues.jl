
function splitprec(::Type{F}, i::SInteger) where {F<:AbstractFloat}
    hi = truncbits(F(i), cld(precision(F), F(2)))
    ihi = oftype(i, hi)
    hi, i - ihi
end
#=
truncmask(x::SFloat16, mask) = reinterpret(Float16, values(mask) & reinterpret(UInt16, values(x))
truncmask(x::SFloat32, mask) = reinterpret(Float32, values(mask) & reinterpret(UInt32, values(x))
truncmask(x::SFloat64, mask) = reinterpret(Float64, values(mask) & reinterpret(UInt64, values(x))

function truncmask(x::F, mask) where {F<:Union{SFloat16, SFloat32, SFloat64}}
    reinterpret(F, mask & reinterpret(Base.uinttype(F), x))
end
truncmask(x, mask) = x

function truncbits(x::F, nb) where {F<:Union{SFloat16, SFloat32, SFloat64}}
    truncmask(x, typemax(uinttype(F)) << nb)
end
truncbits(x, nb) = x
=#
"""
    TPVal{Tuple{H,L},T}
"""
struct TPVal{V<:Tuple{H where H,L where L},T}
    function TPVal{Tuple{H,L},T}() where {H,L,T}
        if T === eltype(H) === eltype(L)
            return new{Tuple{H,L},T}()
        else
            error("high and low precision values must were typed as $T but got,
                  $(eltype(H)) and $(eltype(H)).")
        end
    end
end

Base.@pure gethi(::TPVal{Tuple{H,L},T}) where {H,L,T} = H::T
Base.@pure gethi(::Type{<:TPVal{Tuple{H,L},T}}) where {H,L,T} = H::T

Base.@pure getlo(::TPVal{Tuple{H,L},T}) where {H,L,T} = L::T
Base.@pure getlo(::Type{<:TPVal{Tuple{H,L},T}}) where {H,L,T} = L::T

eltype(::TPVal{Tuple{H,L},T}) where {H,L,T} = T
values(::TPVal{Tuple{H,L},T}) where {H,L,T} = TwicePrecision{T}(H::T,L::T)

TPVal{T}(hi::SReal) where T = TPVal{T}(hi, zero(hi))

function TPVal(::Type{T}, hi::SReal{H}, lo::SReal{L}) where {T,H,L}
    if T === eltype(hi) === eltype(lo)
        return TPVal{Tuple{H,L},T}()
    else
        return TPVal(T, convert_static_val(T, hi), convert_static_val(T, lo))
    end
end

TPVal(hi::SReal, lo::SReal) = TPVal(promote_type(eltype(hi), eltype(lo)), hi, lo)
TPVal(x::TwicePrecision{T}) where T = TPVal{Tuple{x.hi,x.lo},T}()

TPVal(::Type{T}, ::Tuple{SInteger{N},SInteger{D}}) where {N,D,T<:Union{Float16,Float32}} =
    TPVal(T, convert_static_val(T, T, Val(T(N/D))))

TPVal(::Type{T}, ::Tuple{SReal{N},SReal{D}}) where {T,N,D} = TPVal{T}(N) / D

function TPVal(::Type{T}, ::SReal{X}) where {X,T}
    xT = convert(T, X)
    TPVal{Tuple{xT,T(X - xT)},T}()
end
#TPVal{T}(x::SReal) where T = TPVal{T}(x, zero(x))

TPVal(x::SReal) = TPVal(eltype(x), x)

TPVal(::Type{T}, i::SInteger{X}) where {X,T<:AbstractFloat} =
    TPVal(T, canonicalize2(splitprec(T, i)...)...)


SReal(x::TPVal{Tuple{H,L},T}) where {H,L,T} = SReal(Val(H::T + L::T))

#---
# Numerator/Denominator constructors
TPVal{T}(nd::Tuple{<:SReal,<:SReal}, nb::SInteger) where T =
    twiceprecision(TPVal{T}(nd), nb)

# Truncating constructors. Useful for generating values that can be
# exactly multiplied by small integers.
function twiceprecision(v::SReal, nb::SInteger) 
    hi = Base.truncbits(v, nb)
    TPVal(hi, v-hi)
end

function twiceprecision(
    val::TPVal{Tuple{H,L},T},
    nb::SInteger{N}) where {T<:Union{Float16, Float32, Float64},H,L,N}
    hi = Base.truncbits(H::T, N)
    TPVal{Tuple{T(hi), T((H::T - hi) + L::T)},T}()
end



#---conversion
(::Type{T})(x::TPVal{Tuple{H,L},T2}) where {T<:Number,T2,H,L} = T(H::T2 + L::T2)::T

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

@inline function +(::TPVal{Tuple{H,L},T}, y::SReal) where {H,L,T}
    s_hi, s_lo = Base.add12(SReal(H::T), y)
    hnew, hlow = Base.canonicalize2(s_hi, s_lo+L)
    TPVal(hnew, hlow)
end

+(x::SReal, y::TPVal) = y+x

@inline function +(x::TPVal{Tuple{Hx,Lx},T}, y::TPVal{Tuple{Hy,Ly},T}) where {Hx,Lx,Hy,Ly,T}
    r = Hx::T + Hy::T
    s = abs(Hx::T) > abs(Hy::T) ? (((Hx::T - r) + Hy::T) + Ly::T) + Lx::T : (((Hy::T - r) + Hx::T) + Lx::T) + Ly::T
    hnew, lnew = canonicalize2(SReal(r), SReal(s))
    TPVal(hnew,lnew)
end
+(x::TPVal{Tuple{Hx,Lx},Tx}, y::TPVal{Tuple{Hy,Ly},Ty}) where {Hx,Lx,Tx,Hy,Ly,Ty} = +(promote(x, y)...)

-(x::TPVal, y::TPVal) = x + (-y)
-(x::BaseReal, y::TPVal) = x + (-y)
-(x::TPVal, y::BaseReal) = x + (-y)

*(x::TPVal, v::BaseReal) = TwicePrecision(x) * v

# FIXME
function *(x::TPVal{Tuple{H,L},T}, v::SReal) where {T,H,L}
    v == SZero && return TPVal{Tuple{H::T* v,L::T* v},T}()
    x * TPVal(oftype(H::T * v, v))
end

function *(x::TPVal{Tuple{H,L},T}, v::SReal) where {T<:Union{Float16, Float32, Float64},H,L,V,Tv}
    v == 0 && return TPVal(SReal(H::T* v), SReal(L::T* v))
    nb = ceil(Int, log2(abs(v)))
    u = Base.truncbits(H::T, nb)
    TPVal(canonicalize2(SReal(u* v), SReal(((H::T-u) + L::T)* v))...)
end

function *(x::TPVal{Tuple{H,L},T}, s::SInteger) where {H,L,T<:Union{Float16, Float32, Float64}}
    v == 0 && return TPVal(H::T1 * s, L::T1 * s)
    nb = ceil(Int, log2(abs(s)))
    u = Base.truncbits(H::T1, get(nb))
    TPVal(canonicalize2(SReal(u* s), SReal((H::T1-u) + L::T1*s))...)
end

*(v::Number, x::TPVal) = x*v

@inline function *(x::TPVal{Tuple{Hx,Lx},T}, y::TPVal{Tuple{Hy,Ly},T}) where {Hx,Lx,Hy,Ly,T}
    zh, zl = mul12(SReal(Hx::T), SReal(Hy::T))
    hnew, lnew = canonicalize2(zh, SReal(Hx::T * Ly::T + Lx::T * Hy::T) + zl)
    ret = TPVal{Tuple{T(hnew)::T,T(lnew)::T},T}()
    ifelse(iszero(zh) | !isfinite(zh), TPVal{Tuple{T(zh),T(zh)},T}(), ret)
end

*(x::TPVal, y::TPVal) = *(promote(x, y)...)

/(x::TPVal{Tuple{H,L},T}, v::SReal) where {H,L,T} = x / TPVal(oftype(H::T/v, v))

function /(x::TPVal{Tuple{Hx,Lx},T}, y::TPVal{Tuple{Hy,Ly},T}) where {Hx,Lx,Hy,Ly,T}
    hi = SReal(Hx::T / Hy::T)
    uh, ul = mul12(hi, SReal(Hy::T))
    lo = ((((Hx - uh) - ul) + Lx) - hi*Ly)/Hy
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

function Base.rat(v::SReal)
    a = d = SOne(v)
    b = c = SZero(v)
    m = SReal(maxintfloat(Base.narrow(eltype(v)), Int))
    _rat(v, v, m, a, b, c, d)
end

<(x::TPVal, y::SReal) = <(x, TPVal(y))
<(x::SReal, y::TPVal) = <(TPVal(x), y)


<(x::TPVal{Tuple{Hx,Lx},T}, y::TPVal{Tuple{Hy,Ly},T}) where {T,Hx,Lx,Hy,Ly} =
    Hx::T < Hy::T || ((Hx::T == Hy::T) & (Lx::T < Ly::T))

Base.show(io::IO, r::TPVal) = showsval(io, r)
Base.show(io::IO, ::MIME"text/plain", r::TPVal) = showsval(io, r)

showsval(io::IO, r::TPVal{Tuple{H,L},T}) where {T,H,L} = print(io, "TPVal{$T}($H, $L)")
