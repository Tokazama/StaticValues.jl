using StaticValues, Test
import StaticValues: BaseNumber

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

# test for preservation of 'staticness'
getvalues(x::SVal) = values(x)
getvalues(x::BaseNumber) = error("Got $(typeof(x)) instead of static value.")

@testseet "Static Real" begin
    # `getvalues` is used to ensure that $f(sval, sval) --> sval

    static_real_set = ([@inferred(S{B(1)}()) for (S,B) in zip(static_integer, base_integer)]...,)
    base_real_set = ([T(1) for T in (base_integer..., base_float...)]...,)

    @testset "Addition" begin
        for (S1,B1) in zip(static_real_set, base_real_set)
            for (S2,B2) in zip(static_real_set, base_real_set)
                @test @inferred(getvalues(S1 + S2)) === B1 + B2
                @test @inferred(values(S1 + B1)) === B1 + B1
                @test @inferred(values(S1 + B2)) === B1 + B2
            end
        end
    end


    @testset "Substraction" begin
        for (S1,B1) in zip(static_real_set, base_real_set)
            for (S2,B2) in zip(static_real_set, base_real_set)
                @test @inferred(values(S1 - S2)) === B1 - B2
            end
        end
    end

    @testset "Multiplication" begin
        for (S1,B1) in zip(static_real_set, base_real_set)
            for (S2,B2) in zip(static_real_set, base_real_set)
                @test @inferred(values(S1 * S2)) === B1 * B2
            end
        end
    end

    @testset "Division" begin
        for (S1,B1) in zip(static_real_set, base_real_set)
            for (S2,B2) in zip(static_real_set, base_real_set)
                @test @inferred(values(S1 / S2)) === B1 / B2
            end
        end
    end
end
