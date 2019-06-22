using StaticValues, Test
import StaticValues: BaseNumber, S2B, B2S

#=
@generated function sval(x::Int)
    y = Val{x}()
    :($y)
end


base_unsigned = (UInt128,UInt16,UInt32,UInt64,UInt8)
base_signed = (Int128,Int16,Int32,Int64,Int8)
base_integer = (base_unsigned..., base_signed...)
base_float = (Float64,Float32,Float16)
base_real = (base_integer..., base_float..., Rational, Irrational)
base_number = (base_real..., Complex)
static_unsigned = (SUInt128,SUInt16,SUInt32,SUInt64,SUInt8)
static_signed = (SInt128,SInt16,SInt32,SInt64,SInt8)
static_integer = (static_unsigned..., static_signed...)
static_float = (SFloat64,SFloat32,SFloat16)

=#
# test for preservation of 'staticness'
getvalues(x::SReal) = values(x)
getvalues(x::BaseNumber) = error("Got $(typeof(x)) instead of static value.")

@testset "inference" begin
    include("inferencetests.jl")
end

@testset "ranges" begin
    include("rangetests.jl")
end
