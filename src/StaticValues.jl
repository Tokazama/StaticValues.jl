module StaticValues

import Base: splitprec, truncbits, truncmask, twiceprecision, TwicePrecision, canonicalize2
import Base: ==, !=, +, -, *, /, ^, <, >, |, <=, >=, ~, :, !, <<, >>, >>>, &
import Base: eltype, promote_eltype, values, log10, isfinite, zero, iszero,
             abs, abs2, isless, max, min, div, rem, promote_rule, @pure
import Base: getindex, length, first, step, last, firstindex, lastindex, size, eltype

export SInt128, SInt16, SInt32, SInt64, SInt, SInt8,
       SUInt128, SUInt64, SUInt, SUInt32, SUInt16, SUInt8,
       SBool,
       SFloat16, SFloat32, SFloat64,
       AbstractInt64, AbstractInt32, AbstractInt16, AbstractInt8,
       AbstractUInt64, AbstractUInt32, AbstractUInt16, AbstractUInt8,
       AbstractFloat64, AbstractFloat32, AbstractFloat16,
       SSigned, SUnsigned, SInteger, SFloat, SReal, SNumber,
       SVal, TPVal,
       SOne, SZero,
       SChar, SSymbol,
       UnitSRange,
       UnitMRange,
#       OneToSRange,
       StaticStepRangeLen,
       StepSRangeLen,
       StepSRange,
       StepMRange,
       LinSRange,
       LinMRange,
       OffsetSRange,
       OffsetMRange,
       StaticIndices, LinearSIndices,
       NamedRange,
       srange

struct Dynamic end

const BaseUnsigned = Union{UInt128,UInt16,UInt32,UInt64,UInt8}
const BaseSigned = Union{Int128,Int16,Int32,Int64,Int8}
const BaseInteger = Union{Int128,Int16,Int32,Int64,Int8,
                          UInt128,UInt16,UInt32,UInt64,UInt8,Bool}
const BaseFloat = Union{Float64,Float32,Float16}
const BaseReal = Union{Int128,Int16,Int32,Int64,Int8,
                       UInt128,UInt16,UInt32,UInt64,UInt8,
                       Bool,Float64,Float32,Float16,Rational,Irrational}
const BaseNumber = Union{Int128,Int16,Int32,Int64,Int8,
                         UInt128,UInt16,UInt32,UInt64,UInt8,
                         Bool,Float64,Float32,Float16,
                         Rational,Irrational,
                         Complex}

include("deffunctions.jl")
include("signed.jl")
include("unsigned.jl")
include("int.jl")
include("float.jl")
include("rational.jl")
include("real.jl")
include("complex.jl")
include("char.jl")

include("types.jl")
include("intfuncs.jl")
include("floatfuncs.jl")

include("twiceprecision.jl")

include("functions.jl")

#const IEEESFloat = Union{Float16,Float32,Float64,SFloat16,SFloat32,SFloat64}

const F_or_FF = Union{<:AbstractFloat, Tuple{<:AbstractFloat,<:AbstractFloat}}

f64(x::BaseFloat) = Float64(x)
f64(x::Tuple{<:BaseFloat,<:BaseFloat}) = Float64(x[1]) + Float64(x[2])
f64(x::SFloat) = SFloat64(x)
f64(x::Tuple{<:SFloat,<:SFloat}) = SFloat64(x[1]) + SFloat64(x[2])

tp64(x::BaseNumber) = TwicePrecision{Float64}(x)
tp64(x::Tuple{<:BaseNumber,<:BaseNumber}) = TwicePrecision{Float64}(x[1], x[2])
tp64(x::SReal) = TPVal(ofeltype(Float64, x))
tp64(x::Tuple{H,L}) where {H<:SReal,L<:SReal} = TPVal(Float64, x[1]::H, x[2]::L)

include("staticrange.jl")
include("staticstartrange.jl")
include("staticunitrange.jl")
include("staticordinalrange.jl")
include("staticsteprange.jl")
include("staticlinrange.jl")
include("staticsteprangelen.jl")
include("srange.jl")
include("colon.jl")
include("rangefuncs.jl")
include("offsetrange.jl")
include("staticrangewrapper.jl")
include("namedrange.jl")

#include("math.jl")
#

end
