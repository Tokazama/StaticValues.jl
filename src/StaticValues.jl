module StaticValues

import Base: splitprec, truncbits, truncmask, twiceprecision, TwicePrecision, canonicalize2
import Base: ==, +, -, *, /, ^, <, >, <=, >=, ~, :, !, abs, abs2, isless, max, min, div, rem, promote_rule
import Base: eltype, promote_eltype, values, log10, isfinite, zero, iszero

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
       sone,
       szero


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

include("int.jl")
include("float.jl")
include("rational.jl")
include("irrational.jl")
include("complex.jl")

static_real = (static_integer..., static_float..., SRational, SIrrational)

const SReal{V} = Union{SInteger{V},SFloat{V},SRational{V},SIrrational{V}}

SReal(x::BaseInteger) = SInteger(x)
SReal(x::BaseFloat) = SFloat(x)
SReal(x::Rational) = SRational(x)
SReal(x::Irrational) = SIrrational(x)

function SReal(val::Val{V}) where V
    if V isa Integer
        SInteger(val)
    elseif V isa AbstractFloat
        SFloat(val)
    elseif V isa Rational
        SRational(val)
    elseif V isa Irrational
        SIrrational(val)
    end
end

const SNumber{V} = Union{SComplex{V},SReal{V}}

SNumber(x::BaseReal) = SReal(x)
SNumber(x::Complex) = SComplex(x)

function SNumber(val::Val{V}) where V
    if V isa Real
        SReal(val)
    elseif V isa Complex
        SComplex(val)
    end
end

include("char.jl")
include("twiceprecision.jl")

const SVal{V} = Union{SNumber{V},SChar{V},TPVal{V}}

SVal(x::BaseNumber) = SNumber(x)
SVal(x::AbstractChar) = SChar(x)
SVal(x::TwicePrecision) = TPVal(x)

function SVal(val::Val{V}) where V
    if V isa Number
        SNumber(val)
    elseif V isa AbstractChar
        SChar(val)
    elseif V isa TwicePrecision
        TPVal(val)
    end
end

#include("promote.jl")
include("functions.jl")


const AbstractInt128 = Union{SInt128,Int128}
const AbstractInt64  = Union{SInt64,Int64}
const AbstractInt32  = Union{SInt32,Int32}
const AbstractInt16  = Union{SInt16,Int16}
const AbstractInt8   = Union{SInt8,Int8}

const AbstractUInt128 = Union{SUInt128,UInt128}
const AbstractUInt64  = Union{SUInt64,UInt64}
const AbstractUInt32  = Union{SUInt32,UInt32}
const AbstractUInt16  = Union{SUInt16,UInt16}
const AbstractUInt8   = Union{SUInt8,UInt8}

const AbstractFloat64 = Union{SFloat64,Float64}
const AbstractFloat32 = Union{SFloat32,Float32}
const AbstractFloat16 = Union{SFloat16,Float16}


end
