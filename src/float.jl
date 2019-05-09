struct SFloat64{V} <: AbstractFloat
    function SFloat64{V}() where V
        !(typeof(V) === Float64) && throw(ArgumentError("SFloat64 only supports static Float64 storage, got $(typeof(V))"))
        new{V}()
    end
end

struct SFloat32{V} <: AbstractFloat
    function SFloat32{V}() where V
        !(typeof(V) === Float32) && throw(ArgumentError("SFloat32 only supports static Float32 storage, got $(typeof(V))"))
        new{V}()
    end
end

struct SFloat16{V} <: AbstractFloat
    function SFloat16{V}() where V
        !(typeof(V) === Float16) && throw(ArgumentError("SFloat16 only supports static Float16 storage, got $(typeof(V))"))
        new{V}()
    end
end

const SFloat{V} = Union{SFloat16{V}, SFloat32{V}, SFloat64{V}}
Base.show(io::IO, ::SFloat{V}) where V = print(io, V)

promote_rule(::Type{SFloat64}, ::Type{SUInt128}) = SFloat64
promote_rule(::Type{SFloat64}, ::Type{SInt128})  = SFloat64
promote_rule(::Type{SFloat32}, ::Type{SUInt128}) = SFloat32
promote_rule(::Type{SFloat32}, ::Type{SInt128})  = SFloat32
promote_rule(::Type{SFloat32}, ::Type{SFloat16}) = SFloat32
promote_rule(::Type{SFloat64}, ::Type{SFloat16}) = SFloat64
promote_rule(::Type{SFloat64}, ::Type{SFloat32}) = SFloat64

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
base_float = (Float64,Float32,Float16)

for (ST,BT) in zip(static_float,base_float)
    @eval begin
        function Base.mul12(x::$ST, y::$ST)
            h = x * y
            ifelse(iszero(h) | !isfinite(h), (h, h), Base.canonicalize2(h, fma(x, y, -h)))
        end

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



