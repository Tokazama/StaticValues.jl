struct SFloat{V,T<:AbstractFloat} <: AbstractFloat end

#const SBigFloat{V} = SVal{V,BigFloat}
const SFloat16{V} = SFloat{V,Float16}
const SFloat32{V} = SFloat{V,Float32}
const SFloat64{V} = SFloat{V,Float64}

const IEEESFloat{V,T} = Union{SFloat16{V,T}, SFloat32{V,T}, SFloat64{V,T}}


(::Type{SFloat{V,T}})(x::T) where {V,T<:AbstractFloat} = SFloat{x,T}()
(::Type{SFloat{V,T1}})(x::T2) where {V,T1<:AbstractFloat,T2<:AbstractFloat} = SFloat{T1(x)::T1,T1}()

Base.promote_rule(::Type{SFloat64}, ::Type{SUInt128}) = SFloat64
Base.promote_rule(::Type{SFloat64}, ::Type{SInt128}) = SFloat64
Base.promote_rule(::Type{SFloat32}, ::Type{SUInt128}) = SFloat32
Base.promote_rule(::Type{SFloat32}, ::Type{SInt128}) = SFloat32

Base.trunc(::Type{SSigned}, x::SFloat32) = trunc(SInt,x)
Base.trunc(::Type{SSigned}, x::SFloat64) = trunc(SInt,x)
Base.trunc(::Type{SUnsigned}, x::SFloat32) = trunc(SUInt,x)
Base.trunc(::Type{SUnsigned}, x::SFloat64) = trunc(SUInt,x)
Base.trunc(::Type{SInteger}, x::SFloat32) = trunc(SInt,x)
Base.trunc(::Type{SInteger}, x::SFloat64) = trunc(SInt,x)
Base.trunc(::Type{T}, x::SFloat16) where {T<:SInteger} = trunc(T, SFloat32(x))

# fallbacks
floor(::Type{T}, x::SFloat) where {T<:SInteger} = trunc(T,round(x, RoundDown))
floor(::Type{T}, x::SFloat16) where {T<:SInteger} = floor(T, SFloat32(x))
ceil(::Type{T}, x::SFloat) where {T<:SInteger} = trunc(T,round(x, RoundUp))
ceil(::Type{T}, x::SFloat16) where {T<:SInteger} = ceil(T, SFloat32(x))

round(::Type{T}, x::SFloat) where {T<:SInteger} = trunc(T,round(x, RoundNearest))
round(::Type{T}, x::SFloat16) where {T<:SInteger} = round(T, Float32(x))

Base.round(x::SFloat{V,T}, r::RoundingMode) where {V,T<:AbstractFloat} =
    SFloat{round(V::T, r),T}()

## floating point promotions ##
promote_rule(::Type{SFloat32}, ::Type{SFloat16}) = SFloat32
promote_rule(::Type{SFloat64}, ::Type{SFloat16}) = SFloat64
promote_rule(::Type{SFloat64}, ::Type{SFloat32}) = SFloat64

widen(::Type{SFloat16}) = SFloat32
widen(::Type{SFloat32}) = SFloat64

for op in (:+, :-, :*, :/, :\, :^)
    @eval begin
        ($op)(::SFloat{V1,T1}, ::SFloat{V2,T2}) where {V1,V2,T1<:AbstractFloat,T2<:AbstractFloat} = SFloat{V1::T1 * V2::T2}()
    end
end

muladd(x::SFloat{V1,T1}, y::SFloat{V2,T2}, z::SFloat{V3,T3}) where {V1,V2,V3,T1<:AbstractFloat,T2<:AbstractFloat,T3<:AbstractFloat} =
    SFloat{muladd(V1::T1, V2::T1, V3::T3)}()

Base.precision(::Type{SFloat16}) = SInt{11,Int}()
Base.precision(::Type{SFloat32}) = SInt{24,Int}()
Base.precision(::Type{SFloat64}) = SInt{53,Int}()

reinterpret(::Type{SUnsigned}, x::SFloat64) = reinterpret(SUInt64, x)
reinterpret(::Type{SUnsigned}, x::SFloat32) = reinterpret(SUInt32, x)
reinterpret(::Type{SSigned}, x::SFloat64) = reinterpret(SInt64, x)
reinterpret(::Type{SSigned}, x:S:SFloat32) = reinterpret(SInt32, x)

sign_mask(::Type{SFloat64}) =        SUnsigned{0x8000_0000_0000_0000,UInt64}()
exponent_mask(::Type{SFloat64}) =    SUnsigned{0x7ff0_0000_0000_0000,UInt64}()
exponent_one(::Type{SFloat64}) =     SUnsigned{0x3ff0_0000_0000_0000,UInt64}()
exponent_half(::Type{SFloat64}) =    SUnsigned{0x3fe0_0000_0000_0000,UInt64}()
significand_mask(::Type{SFloat64}) = SUnsigned{0x000f_ffff_ffff_ffff,UInt64}()

sign_mask(::Type{SFloat32}) =        SUnsigned{0x8000_0000,UInt32}()
exponent_mask(::Type{SFloat32}) =    SUnsigned{0x7f80_0000,UInt32}()
exponent_one(::Type{SFloat32}) =     SUnsigned{0x3f80_0000,UInt32}()
exponent_half(::Type{SFloat32}) =    SUnsigned{0x3f00_0000,UInt32}()
significand_mask(::Type{SFloat32}) = SUnsigned{0x007f_ffff,UInt32}()

sign_mask(::Type{SFloat16}) =        SUnsigned{0x8000,UInt16}()
exponent_mask(::Type{SFloat16}) =    SUnsigned{0x7c00,UInt16}()
exponent_one(::Type{SFloat16}) =     SUnsigned{0x3c00,UInt16}()
exponent_half(::Type{SFloat16}) =    SUnsigned{0x3800,UInt16}()
significand_mask(::Type{SFloat16}) = SUnsigned{0x03ff,UInt16}()

function nextfloat(f::IEEESFloat{V,T}, d::Integer) where {V,T}
   F = typeof(f)
    fumax = reinterpret(SUnsigned, T(SInf))
    U = typeof(fumax)

    isnan(f) && return f
    fi = reinterpret(SSigned, f)
    fneg = fi < 0
    fu = unsigned(fi & typemax(fi))

    dneg = d < 0
    da = uabs(d)
    if da > typemax(U)
        fneg = dneg
        fu = fumax
    else
        du = da % U
        if fneg ⊻ dneg
            if du > fu
                fu = min(fumax, du - fu)
                fneg = !fneg
            else
                fu = fu - du
            end
        else
            if fumax - fu < du
                fu = fumax
            else
                fu = fu + du
            end
        end
    end
    if fneg
        fu |= sign_mask(F)
    end
    reinterpret(F, fu)
end

==(x::SFloat{V,T}, y::SFloat{V,T}) = 
==(x::Float64, y::Float64) = eq_float(x, y)
!=(x::Float32, y::Float32) = ne_float(x, y)
!=(x::Float64, y::Float64) = ne_float(x, y)
<( x::Float32, y::Float32) = lt_float(x, y)
<( x::Float64, y::Float64) = lt_float(x, y)
<=(x::Float32, y::Float32) = le_float(x, y)
<=(x::Float64, y::Float64) = le_float(x, y)

isequal(x::Float32, y::Float32) = fpiseq(x, y)
isequal(x::Float64, y::Float64) = fpiseq(x, y)
isless( x::Float32, y::Float32) = fpislt(x, y)
isless( x::Float64, y::Float64) = fpislt(x, y)
for op in (:<, :<=, :isless)
    @eval ($op)(a::Float16, b::Float16) = ($op)(Float32(a), Float32(b))
end

