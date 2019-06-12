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
       SOne, SZero


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

include("utils.jl")
include("types.jl")
include("intfuncs.jl")
include("floatfuncs.jl")
include("rational.jl")
include("irrational.jl")

const SReal{V} = Union{SInteger{V},SFloat{V},SRational{V},SIrrational{V}}

SReal(x::BaseInteger) = SInteger(x)
SReal(x::BaseFloat) = SFloat(x)
SReal(x::Rational) = SRational(x)
SReal(x::Irrational) = SIrrational(x)

function SReal(::Val{V}) where V
    if V isa Integer
        if V isa Unsigned
            if V isa UInt8
                SUInt8{V}()
            elseif V isa UInt16
                SUInt16{V}()
            elseif V isa UInt32
                SUInt32{V}()
            elseif V isa UInt128
                SUInt128{V}()
            else
                SUInt64{V}()
            end
        elseif V isa Bool
            SBool{V}()
        elseif V isa Signed
            if V isa Int8
                SInt8{V}()
            elseif V isa Int16
                SInt16{V}()
            elseif V isa Int32
                SInt32{V}()
            elseif V isa Int128
                SInt128{V}()
            else
                SInt64{V}()
            end
        end
    elseif V isa AbstractFloat
        if V isa Float16
            SFloat16{V}()
        elseif V isa Float32
            SFloat32{V}()
        else
            SFloat64{V}()
        end
    elseif V isa Rational
        SRational(val)
    elseif V isa Irrational
        SIrrational(val)
    end
end

Base.promote_eltype(x::SReal, y::BaseNumber) = promote_type(eltype(x), eltype(y))
Base.promote_eltype(x::BaseNumber, y::SReal) = promote_type(eltype(x), eltype(y))
Base.promote_eltype(x::SReal, y::SReal) = promote_type(eltype(x), eltype(y))
include("twiceprecision.jl")

include("complex.jl")
static_real = (static_integer..., static_float..., SRational, SIrrational)


const SNumber{V} = Union{SComplex{V},SReal{V}}

SNumber(x::BaseReal) = SReal(x)
SNumber(x::Complex) = SComplex(x)

function SNumber(val::Val{V}) where V
    if V isa Real
        if V isa Integer
            if V isa Unsigned
                if V isa UInt8
                    SUInt8{V}()
                elseif V isa UInt16
                    SUInt16{V}()
                elseif V isa UInt32
                    SUInt32{V}()
                elseif V isa UInt128
                    SUInt128{V}()
                else
                    SUInt64{V}()
                end
            elseif V isa Bool
                SBool{V}()
            elseif V isa Signed
                if V isa Int8
                    SInt8{V}()
                elseif V isa Int16
                    SInt16{V}()
                elseif V isa Int32
                    SInt32{V}()
                elseif V isa Int128
                    SInt128{V}()
                else
                    SInt64{V}()
                end
            end
        elseif V isa AbstractFloat
            if V isa Float16
                SFloat16{V}()
            elseif V isa Float32
                SFloat32{V}()
            else
                SFloat64{V}()
            end
        elseif V isa Rational
            SRational(val)
        elseif V isa Irrational
            SIrrational(val)
        end
    elseif V isa Complex
        SComplex(val)
    end
end

include("char.jl")

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
