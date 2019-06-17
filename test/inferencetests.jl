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


using StaticValues: sfloatrange, linspace, linspace1, srangehp
@testset "Ranges" begin
    @testset "colon" begin
        @inferred(srange(Val(10), step=Val(1), stop=Val(0)))

        @inferred(srange(Val(1), step=Val(.2), stop=Val(2)))

        @inferred(srange(Val(1.), step=Val(.2), stop=Val(2.)))

        @inferred(srange(Val(2), step=Val(-.2), stop=Val(1)))

        @inferred(srange(Val(0.0), Val(-0.5)))

        @inferred(srange(Val(1),Val(0)))
    end

    @testset "floatrange" begin
        T = Float32
        b = SVal(1)
        s = SVal(2)
        l = SVal(10)
        d = SVal(1)

        # src/floatrange line 1
        @inferred(sfloatrange(T, b, s, l, d))

        b = SVal(1.0)
        s = SVal(2.0)
        d = SVal(1.0)
        # src/floatrange line 19
        @inferred(sfloatrange(b, s, l, d))
    end

    @testset "linspace" begin
        T = Int
        b = SVal(1)
        e = SVal(10)
        l = SVal(5)

        @inferred(linspace(T, b, e, l))

        @inferred(linspace(Float64, b, e, l))

        b = SVal(1)
        e = SVal(10)
        l = SVal(5)
        d = SVal(1)

        # src/linspace.jl line 73
        @inferred(linspace(Float64, b, e, l, d))

        b = SVal(1.0)
        e = SVal(10.0)
        l = SVal(5)
        @inferred(linspace(b, e, l))

        # src/linlspace.jl line 139
        T = Float16
        b = SVal(1.0)
        e = SVal(1.0)
        l = SVal(1)
        @inferred(linspace1(T, b, e, l))

        # src/linspace.jl line 159
#        @test @inferred(linrange(T,b,e,l)) == StaticRanges.SRange{Float16,SVal{1.0,Float64},SVal{0.0,Float64},Float16(1.0),1,1}()
    end

    @testset "srangehp" begin
        b = (SInt128(1), SInt128(1))
        s = (SInt128(1), SInt128(1))
        nb = SOne
        l = SInt(2)
        f = SInt(1)
        T = Float64

        # src/srangehp.jl line 1
        @test @inferred(srangehp(T, b, s, nb, l, f)) == StepSRangeLen{Float64,TPVal{SFloat64{1.0},SFloat64{0.0}},TPVal{SFloat64{1.0},SFloat64{0.0}},SFloat64{2.0},SInt64{2},SInt64{1}}()

        # src/srangehp.jl line 7
        @test @inferred(srangehp(Float32, b, s, nb, l, f)) == StepSRangeLen{Float32,SFloat64{1.0},SFloat64{1.0},SFloat64{2.0},SInt64{2},SInt64{1}}()

        # src/srangehp.jl line 21
        b = SVal(1.0)
        s = SVal(1.0)
        @test @inferred(srangehp(T, b, s, nb, l, f)) == StepSRangeLen{Float64,TPVal{SFloat64{1.0},SFloat64{0.0}},TPVal{SFloat64{1.0},SFloat64{0.0}},SFloat64{2.0},SInt64{2},SInt64{1}}()

        b = (SVal(1.0), SVal(0.0))
        s = (SVal(1.0), SVal(0.0))
        @test @inferred(srangehp(T, b, s, nb, l, f)) == StepSRangeLen{Float64,TPVal{SFloat64{1.0},SFloat64{0.0}},TPVal{SFloat64{1.0},SFloat64{0.0}},SFloat64{2.0},SInt64{2},SInt64{1}}()

        @test @inferred(srangehp(Float32, b, s, nb, l, f)) == StepSRangeLen{Float32,SFloat64{1.0},SFloat64{1.0},SFloat64{2.0},SInt64{2},SInt64{1}}()

        b = SVal(1.0)
        s = SVal(1.0)
        @test @inferred(srangehp(Float32, b, s, nb, l, f)) == StepSRangeLen{Float32,SFloat64{1.0},SFloat64{1.0},SFloat64{2.0},SInt64{2},SInt64{1}}()
    end

    #=
    @testset "steprange" begin
        # steprange_length Int
        @test length(steprange(SVal(1), SVal(1), SVal(5))) == 5
        # steprange_length general
        @test length(steprange(SVal(1.0), SVal(1.0), SVal(5.0))) == 5
        # steprange_last start == stop
        @test last(steprange(SVal(4), SVal(1), SVal(4))) == 4
        # steprange_last general
        @test last(steprange(SVal(1.0), SVal(1.0), SVal(5.0))) == 5.0
        # steprange_last_empty integer
        # steprange_last_empty general
    end
    =#

    # FIXME
    @testset "StepSRangeLen" begin
        b = SVal(1.0)
        s = SVal(2.0)
        l = SVal(10)
        f = SVal(1)

        # src/steprangelen.jl line 1
        @inferred(StepSRangeLen(b, s, l , f))
        @inferred(StepSRangeLen{Float64}(b, s, l, f))


        # src/steprangelen.jl line 9
        b = TPVal{1.0,0.0}()
        s = TPVal{2.0,0.0}()

        @inferred(StaticStepRangeLen(b, s, l, f))

        # src/steprangelen.jl line 28
        @inferred(StepSRangeLen{Float64}(b, s, l, f))
    end
end
