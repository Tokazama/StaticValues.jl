
# TODO: SInt128 promotions are type unstable!
#(::Type{T})(x::Float16) where {T<:Integer} = T(Float32(x))

#Bool(x::Real) = x==0 ? false : x==1 ? true : throw(InexactError(:Bool, Bool, x))


Base.reinterpret(::Type{<:SUnsigned}, x::SFloat16) = reinterpret(SUInt16, x)
Base.reinterpret(::Type{<:SSigned}, x::SFloat16) = reinterpret(SInt16, x)

Base.reinterpret(::Type{<:SInt16}, x::SFloat16{X}) where X = SInt16{Base.bitcast(Int16, X::Float16)}()
Base.reinterpret(::Type{<:SUInt16}, x::SFloat16{X}) where X = SUInt16{Base.bitcast(UInt16, X::Float16)}()

Base.reinterpret(::Type{<:SFloat32}, x::SUInt32{X}) where X = SFloat32{Base.bitcast(Float32, X::UInt32)}()
Base.reinterpret(::Type{<:SFloat32}, x::SInt32{X}) where X = SFloat32{Base.bitcast(Float32, X::Int32)}()



function _sfloat32(exp::SUInt32ZeroType, sig::SUInt32, sign::SUInt32)
    __sfloat32(SOne, SUInt16(0x0200), sig, sign)
end
function __sfloat32(n_bit::SInt, bit::SUInt16, sig::SUInt32ZeroType, sign::SUInt32)
    if (bit & sig) == 0
        __sfloat32(n_bit, bit, sig, sign)
    else
        return (sign << SInt(31)) | (((-SInt(14) - n_bit + SInt(127)) << SInt(23)) % SUInt32) | (((sig & (~bit)) << n_bit) << (SInt(23) - SInt(10)))
    end
end

#=
ival = reinterpret(UInt16, Float16(3))
(ival & 0x7c00) >> 10

ival = reinterpret(SUInt16, SFloat16(1))
exp = (ival & SUInt16{0x7c00}()) >> SInt{10}()
sig = (ival & SUInt16{0x3ff}()) >> SZero
sign = (ival & SUInt16{0x8000}()) >> SInt{15}()

(ival & SUInt16(0x7c00)) >> SInt(10)
(ival & 0x7c00) >> 10
=#

_sfloat32(exp::SUInt32ZeroType,     sig::SUInt32ZeroType, sign::SUInt32) = (sign << SInt(31)) | exp | sig
_sfloat32(exp::SUInt32{0x0000001f}, sig::SUInt32ZeroType, sign::SUInt32ZeroType) = SUInt32(0x7f800000)
_sfloat32(exp::SUInt32{0x0000001f}, sig::SUInt32ZeroType, sign::SUInt32) = 0xff800000
_sfloat32(exp::SUInt32{0x0000001f}, sig::SUInt32,         sign::SUInt32) = SUInt32(0x7fc00000) | (sign<<SInt(31)) | (sig<<(SInt(13)))
_sfloat32(exp::SUInt32,             sig::SUInt32,         sign::SUInt32) = (sign << SInt(31)) | (((exp + SInt(112)) << SInt(23)) % SUInt32) | (sig << SInt(13))



function SFloat32(val::SFloat16)
    local ival::SUInt32 = reinterpret(SUInt16, val)
    return reinterpret(SFloat32, _sfloat32((ival & SUInt16{0x7c00}()) >> SInt{10}(),
                                           (ival & SUInt16{0x3ff}()) >> SZero,
                                           (ival & SUInt16{0x8000}()) >> SInt{15}()))
end


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

Base.:(-)(x::SFloat32{V1}, y::SFloat16{V2}) where {V1,V2} = SFloat32{(-)(V1::Float32, V2::Float16)::Float32}()

static_float = (SFloat64,SFloat32,SFloat16)

for (ST,BT) in zip(static_float,base_float)
    @eval begin
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
