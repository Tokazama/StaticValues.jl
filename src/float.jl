struct SFloat64{V} <: AbstractFloat
    function SFloat64{V}() where V
        !(V isa Float64) && throw(ArgumentError("SFloat64 only supports static Float64 storage, got $(typeof(V))"))
        new{V}()
    end
end

struct SFloat32{V} <: AbstractFloat
    function SFloat32{V}() where V
        !(V isa Float32) && throw(ArgumentError("SFloat32 only supports static Float32 storage, got $(typeof(V))"))
        new{V}()
    end
end

struct SFloat16{V} <: AbstractFloat
    function SFloat16{V}() where V
        !(V isa Float16) && throw(ArgumentError("SFloat16 only supports static Float16 storage, got $(typeof(V))"))
        new{V}()
    end
end

const SFloat{V} = Union{SFloat16{V},SFloat32{V},SFloat64{V}}

SFloat(x::Float16) = SFloat16{x}()
SFloat(x::Float32) = SFloat32{x}()
SFloat(x::Float64) = SFloat64{x}()
SFloat(x::BaseNumber) = SFloat(float(x))

function SFloat(val::Val{V}) where V
    if V isa Float16
        SFloat16{V}()
    elseif V isa Float32
        SFloat32{V}()
    else
        SFloat64{V}()
    end
end

const SFloat64One = SFloat64{1.0}()
const SFloat64Zero = SFloat64{0.0}()
const SFloat64OneType = typeof(SFloat64{1.0}())
const SFloat64ZeroType = typeof(SFloat64{0.0}())

const SFloat32One = SFloat32{Float32(1)}()
const SFloat32Zero = SFloat32{Float32(0)}()
const SFloat32OneType = typeof(SFloat32{Float32(1)}())
const SFloat32ZeroType = typeof(SFloat32{Float32(0)}())

const SFloat16One = SFloat16{Float16(1)}()
const SFloat16Zero = SFloat16{Float16(0)}()
const SFloat16OneType = typeof(SFloat16{Float16(1)}())
const SFloat16ZeroType = typeof(SFloat16{Float16(0)}())

Base.show(io::IO, val::SFloat) = print(io, values(val))

promote_rule(::Type{SFloat16}, ::Type{SBool}) = SFloat16
promote_rule(::Type{<:SFloat32}, ::Type{<:SFloat16}) = SFloat32
promote_rule(::Type{<:SFloat64}, ::Type{<:SFloat16}) = SFloat64
promote_rule(::Type{<:SFloat64}, ::Type{<:SFloat32}) = SFloat64

# TODO: SInt128 promotions are type unstable!
for (ST,BT) in ((SFloat16, Float16), (SFloat32,Float32), (SFloat64,Float64))
    for (ST2,BT2) in zip(static_integer, base_integer)
        @eval begin
            (::Type{$ST})(x::$ST2{X}) where X = $ST{$BT(X::$BT2)}()
            promote_rule(::Type{<:$ST}, ::Type{<:$ST2}) = $ST
        end
    end
end

#(::Type{T})(x::Float16) where {T<:Integer} = T(Float32(x))

#Bool(x::Real) = x==0 ? false : x==1 ? true : throw(InexactError(:Bool, Bool, x))


Base.sign_mask(::Type{<:SFloat64})        = SUInt64(0x8000_0000_0000_0000)
Base.exponent_mask(::Type{<:SFloat64})    = SUInt64(0x7ff0_0000_0000_0000)
Base.exponent_one(::Type{<:SFloat64})     = SUInt64(0x3ff0_0000_0000_0000)
Base.exponent_half(::Type{<:SFloat64})    = SUInt64(0x3fe0_0000_0000_0000)
Base.significand_mask(::Type{<:SFloat64}) = SUInt64(0x000f_ffff_ffff_ffff)

Base.sign_mask(::Type{<:SFloat32})        = SUInt32(0x8000_0000)
Base.exponent_mask(::Type{<:SFloat32})    = SUInt32(0x7f80_0000)
Base.exponent_one(::Type{<:SFloat32})     = SUInt32(0x3f80_0000)
Base.exponent_half(::Type{<:SFloat32})    = SUInt32(0x3f00_0000)
Base.significand_mask(::Type{<:SFloat32}) = SUInt32(0x007f_ffff)

Base.sign_mask(::Type{<:SFloat16}) =        SUInt16(0x8000)
Base.exponent_mask(::Type{<:SFloat16}) =    SUInt16(0x7c00)
Base.exponent_one(::Type{<:SFloat16}) =     SUInt16(0x3c00)
Base.exponent_half(::Type{<:SFloat16}) =    SUInt16(0x3800)
Base.significand_mask(::Type{<:SFloat16}) = SUInt16(0x03ff)

# integer size of float
Base.uinttype(::Type{<:SFloat64}) = SUInt64
Base.uinttype(::Type{<:SFloat32}) = SUInt32
Base.uinttype(::Type{<:SFloat16}) = SUInt16


const IEEESFloat = Union{SFloat16,SFloat32,SFloat64}

significand_bits(::Type{T}) where T<:IEEESFloat = SInt(trailing_ones(Base.significand_mask(T)))
exponent_bits(::Type{T}) where T<:IEEESFloat = SInt(sizeof(eltype(T))*8 - significand_bits(SFloat64) - SOne)
exponent_bias(::Type{T}) where T<:IEEESFloat = SInt(Base.exponent_one(T) >> significand_bits(T))
exponent_max(::Type{T}) where T<:IEEESFloat = SInt(Base.exponent_mask(T) >> significand_bits(T)) - exponent_bias(T)
exponent_raw_max(::Type{T}) where T<:IEEESFloat = SInt(Base.exponent_mask(T) >> significand_bits(T))

static_float = (SFloat64,SFloat32,SFloat16)

for (ST,BT) in zip(static_float,base_float)
    @eval begin
        function Base.muladd(x::$ST{X}, y::$ST{Y}, z::$ST{Z}) where {X,Y,Z}
            $ST{muladd(X::$BT,Y::$BT,Z::$BT)}()
        end

        Base.prevfloat(x::$ST{V}) where V = $ST{prevfloat(V::$BT)}()
        Base.prevfloat(x::$ST{V}, n::Integer) where V = $ST{prevfloat(V::$BT, n)}()

        Base.floatmax(x::$ST) = $ST{floatmax($BT)}()
        Base.floatmax(::Type{$ST}) = $ST{floatmax($BT)}()

        Base.floatmin(x::$ST) = $ST{floatmin($BT)}()
        Base.floatmin(::Type{$ST}) = $ST{floatmin($BT)}()

        (/)(::$ST{V1}, ::$ST{V2}) where {V1,V2} = $ST{(/)(V1::$BT, V2::$BT)}()

        function Base.mul12(x::$ST, y::$ST)
            Base.@_inline_meta
            h = x * y
            ifelse(iszero(h) | !isfinite(h), (h, h), Base.canonicalize2(h, fma(x, y, -h)))
        end

        Base.log2(::$ST{V}) where V = $ST{log(V::$BT)/log(2)}()
        Base.log10(::$ST{V}) where V = $ST{log(V::$BT)/log(10)}()
        Base.round(::$ST{V}, m::RoundingMode) where V = $ST{round(V::$BT, m)}()

        function Base.div12(x::$ST, y::$ST)
            # We lose precision if any intermediate calculation results in a subnormal.
            # To prevent this from happening, standardize the values.
            xs, xe = frexp(x)
            ys, ye = frexp(y)
            r = xs / ys
            rh, rl = canonicalize2(r, -fma(r, ys, -xs)/ys)
            ifelse(iszero(r) | !isfinite(r), (r, r), (ldexp(rh, xe-ye), ldexp(rl, xe-ye)))
        end

        Base.frexp(::$ST{X}) where X = $ST{frexp(X::$BT)}()
    end
end



for (ST,BT) in zip(static_float,base_float)
    for (ST2,BT2) in zip(static_integer,base_integer)
        @eval begin
            Base.trunc(::Type{$BT2}, x::$ST{X}) where {T,X} = $ST2{trunc($BT2, X::$BT)}()
        end
    end
end


Base.unsigned(x::SFloat) = Base.unsigned(SInt(x))

Base.maxintfloat(::Type{<:SFloat64}) = SFloat64(9007199254740992.)
Base.maxintfloat(::Type{<:SFloat32}) = SFloat32(16777216.)
Base.maxintfloat(::Type{<:SFloat16}) = SFloat16(2048f0)
Base.maxintfloat(x::T) where {T<:SFloat} = maxintfloat(T)

Base.widen(::Type{SFloat16}) = SFloat32
Base.widen(::Type{SFloat32}) = SFloat64



Base.AbstractFloat(x::SFloat) = x
