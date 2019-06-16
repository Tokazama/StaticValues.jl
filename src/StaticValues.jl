module StaticValues

import Base: splitprec, truncbits, truncmask, twiceprecision, TwicePrecision, canonicalize2
import Base: ==, !=, +, -, *, /, ^, <, >, |, <=, >=, ~, :, !, <<, >>, >>>, &
import Base: eltype, promote_eltype, values, log10, isfinite, zero, iszero,
             abs, abs2, isless, max, min, div, rem, promote_rule, @pure

export SInt128, SInt16, SInt32, SInt64, SInt, SInt8,
       SUInt128, SUInt64, SUInt, SUInt32, SUInt16, SUInt8,
       SBool,
       SFloat16, SFloat32, SFloat64,
       AbstractInt64, AbstractInt32, AbstractInt16, AbstractInt8,
       AbstractUInt64, AbstractUInt32, AbstractUInt16, AbstractUInt8,
       AbstractFloat64, AbstractFloat32, AbstractFloat16,
       SSigned, SUnsigned, SInteger, SFloat, SReal,
       SVal, TPVal,
       SOne, SZero,
       SChar, SSymbol


base_unsigned = (UInt128,UInt16,UInt32,UInt64,UInt8)
const BaseUnsigned = Union{base_unsigned...}

base_signed = (Int128,Int16,Int32,Int64,Int8)
const BaseSigned = Union{base_signed...}

base_integer = (base_unsigned..., base_signed...)

const BaseInteger = Union{base_integer..., Bool}

base_float = (Float64,Float32,Float16)

const BaseFloat = Union{base_float...}

base_real = (base_integer..., base_float..., Rational, Irrational)
const BaseReal = Union{base_real..., Bool}

base_number = (base_real..., Complex)
const BaseNumber = Union{base_number...}

function defbasics(::Type{ST}, ::Type{BT}) where {ST,BT}
    @eval begin
        Base.@pure Base.values(::$ST{V}) where V = V::$BT
        Base.@pure Base.values(::Type{$ST{V}}) where V = V::$BT

        (::Type{<:$ST})(val::Val{V}) where V = $ST{$BT(V)}()

        Base.eltype(::$ST) = $BT
        Base.eltype(::Type{<:$ST}) = $BT

        Base.typemax(::$ST) = $ST{Base.typemax($BT)}()
        Base.typemax(::Type{$ST}) = $ST{Base.typemax($BT)}()

        Base.typemin(::$ST) = $ST{typemin($BT)}()
        Base.typemin(::Type{$ST}) = $ST{typemin($BT)}()

        promote_rule(::Type{<:$ST}, ::Type{$BT}) = $BT

        (::Type{$BT})(::$ST{X}) where X = X::$BT
        (::Type{<:$ST{<:Any}})(x::$ST) = x
        (::Type{<:$ST{<:Any}})(x::$BT) = $ST{x}()
        (::Type{<:$ST{<:Any}})(x::BaseNumber) = $ST($BT(x))

        ofeltype(::Type{$BT}, val::$ST) = val

        seek_static_val(::Type{$BT}, val::Val{V}) where V = $ST{V}()
    end
end

include("signed.jl")
include("unsigned.jl")
include("float.jl")

include("deffunctions.jl")
include("types.jl")
include("intfuncs.jl")
include("floatfuncs.jl")
include("rational.jl")

include("twiceprecision.jl")

include("complex.jl")

include("functions.jl")
#include("math.jl")

end
