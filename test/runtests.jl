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

@testset "Static Real" begin
    # `getvalues` is used to ensure that $f(sval, sval) --> sval

#    static_real_set = ([@inferred(S{B(1)}()) for (S,B) in S2B]...,)
#    base_real_set = ([B(1) for (S,B) in (S2B)]...,)

    @testset "values, eltype" begin
        for (S,B) in S2B
            @test @inferred(values(S(1))) == B(1)
            @test eltype(values(S(1))) == eltype(B(1))
        end
    end

    @testset "Addition" begin
        for (S1,B1) in S2B
            for (S2,B2) in S2B
                @test @inferred(getvalues(S1(1) + S2(1))) == B1(1) + B2(1)
                @test @inferred(values(S1(1) + B1(1))) === B1(1) + B1(1)
                @test @inferred(values(S1(1) + B2(1))) === B1(1) + B2(1)
            end
        end
    end

    @testset "Substraction" begin
        for (S1,B1) in S2B
            for (S2,B2) in S2B
                @test @inferred(S1(1) - S2(1)) == B1(1) - B2(1)
            end
        end
    end

    @testset "Multiplication" begin
        for (S1,B1) in S2B
            for (S2,B2) in S2B
                @test @inferred(one(S1) * one(S2)) == one(B1) * one(B2)
            end
        end
    end

    @testset "Division" begin
        for (S1,B1) in S2B
            for (S2,B2) in S2B
                @test @inferred(one(S1) / one(S2)) == one(B1) / one(B2)
            end
        end
    end

    @testset "fma" begin
        for (S1,B1) in S2B
            @test @inferred(fma(one(S1), one(S1), one(S1))) == fma(one(B1), one(B1), one(B1))
        end
    end

    # TODO: Inferrence problem
    @testset "muladd" begin
        for (S1,B1) in S2B
            @test @inferred(muladd(one(S1), one(S1), one(S1))) == muladd(one(B1), one(B1), one(B1))
        end
    end

    @testset "fld" begin
        for (S,B) in S2B
            @test @inferred(fld(S(5),S(2))) == fld(B(5),B(2))
        end
    end

    @testset "cld" begin
        for (S,B) in S2B
            @test @inferred(cld(S(5),S(2))) == cld(B(5),B(2))
        end
    end

    @testset "rem" begin
        for (S,B) in S2B
            @test @inferred(rem(S(15),S(4))) == rem(B(15),B(4))
        end
    end

    @testset "max" begin
        for (S,B) in S2B
            @test @inferred(max(one(S),S(3))) == max(one(B),B(3))
        end

    end

    @testset "min" begin
        for (S,B) in S2B
            @test @inferred(min(one(S),S(3))) == min(one(B),B(3))
        end
    end

    @testset "minmax" begin
        for (S,B) in S2B
            @test @inferred(minmax(one(S),S(3))) == minmax(one(B),B(3))
        end
    end

end
