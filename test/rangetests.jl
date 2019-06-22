@testset "indexing" begin
    L32 = @inferred(range(SInt32(1), stop=SInt32(4), length=SInt(4)))
    L64 = @inferred(range(SInt64(1), stop=SInt64(4), length=SInt(4)))
    @test @inferred(L32[1]) == 1.0 && @inferred(L64[1]) == 1.0
    @test L32[2] == 2 && L64[2] == 2
    @test L32[3] == 3 && L64[3] == 3
    @test L32[4] == 4 && L64[4] == 4

    @test @inferred(range(SVal(1.0), stop=SVal(2.0), length=SVal(2)))[1] == 1.0
    @test @inferred(range(SVal(1.0f0), stop=SVal(2.0f0), length=SVal(2)))[1] == 1.0f0
    @test @inferred(range(SVal(Float16(1.0)), stop=SVal(Float16(2.0)), length=SVal(2)))[1] == Float16(1.0)

    let r = SInt(5):SInt(-1):SInt(1)
        @test r[1]==5
        @test r[2]==4
        @test r[3]==3
        @test r[4]==2
        @test r[5]==1
    end
    # FIXME @test @inferred(range(SVal(0.1), step=SVal(0.1), stop=SVal(0.3))[2]) == 0.2
    @test @inferred(range(SVal(0.1f0), step=SVal(0.1f0), stop=SVal(0.3f0))[2]) == 0.2f0

    @test @inferred(SVal(1):SVal(5))[1:4] == 1:SVal(1):4
    #FIXME @test @inferred(SVal(1.0):SVal(5))[1:4] == 1.0:4
    @test (SVal(2):SVal(6))[1:4] == 2:5
    @test (SOne:SVal(6))[2:5] == 2:5
    @test (SOne:SVal(6))[2:2:5] == 2:2:4
    @test (SOne:SVal(2):SVal(13))[2:6] == 3:2:11
    @test (SOne:SVal(2):SVal(13))[2:3:7] == 3:6:13

    #@test isempty(srange(1:4)[5:4])
    #@test_throws BoundsError range(1:10)[8:-1:-2]

    let r = typemax(SInt)-SVal(5):typemax(SInt)-SOne
        @test_throws BoundsError r[7]
    end
    @testset "indexing range with empty range (#4309)" begin
        @test (SVal(3):SVal(6))[5:4] == 7:6
        @test_throws BoundsError (SVal(3):SVal(6))[5:5]
        @test_throws BoundsError (SVal(3):SVal(6))[5]
        @test (SZero:SVal(2):SVal(10))[7:6] == 12:2:10
        @test_throws BoundsError (SZero:SVal(2):SVal(10))[7:7]
    end
end

@testset "intersect" begin
    @test intersect(SOne:SVal(5), SVal(2):SVal(3)) == 2:3
    @test intersect(SVal(-3):SVal(5), SVal(2):SVal(8)) == 2:5
    @test intersect(SVal(-8):SVal(-3), SVal(-8):SVal(-3)) == -8:-3
    @test intersect(SOne:SVal(5), SVal(5):SVal(13)) == 5:5
    @test isempty(intersect(SVal(-8):SVal(-3), SVal(-2):SVal(2)))
    @test isempty(intersect(SVal(-3):SVal(7), SVal(2):SVal(1)))
    @test intersect(SOne:SVal(11), SVal(-2):SVal(3):SVal(15)) == 1:3:10
    @test intersect(SOne:SVal(11), SVal(-2):SVal(2):SVal(15)) == 2:2:10
    @test intersect(SOne:SVal(11), SVal(-2):SVal(1):SVal(15)) == 1:11
    @test intersect(SOne:SVal(11), SVal(15):SVal(-1):SVal(-2)) == 1:11
    @test intersect(SOne:SVal(11), SVal(15):SVal(-4):SVal(-2)) == 3:4:11
    @test intersect(SVal(-20):SVal(-5), SVal(-10):SVal(3):SVal(-2)) == -10:3:-7
    @test isempty(intersect(SVal(-5):SVal(5), SVal(-6):SVal(13):SVal(20)))
    @test isempty(intersect(SOne:SVal(11), SVal(15):SVal(4):SVal(-2)))
    @test isempty(intersect(SVal(11):SOne, SVal(15):SVal(-4):SVal(-2)))
    #@test intersect(-5:5, 1+0*(1:3)) == 1:1
    #@test isempty(intersect(-5:5, 6+0*(1:3)))
    @test intersect(SVal(-15):SVal(4):SVal(7), SVal(-10):SVal(-2)) == SVal(-7):SVal(4):SVal(-3)
    @test intersect(SVal(13):SVal(-2):SVal(1), SVal(-2):SVal(8)) == (SVal(7):SVal(-2):SVal(1))
    @test isempty(intersect(SVal(13):SVal(2):SOne, SVal(-2):SVal(8)))
    @test isempty(intersect(SVal(13):SVal(-2):SOne, SVal(8):SVal(-2)))
    #@test intersect(5+0*(1:4), 2:8) == 5+0*(1:4)
    #@test isempty(intersect(5+0*(1:4), -7:3))
    @test intersect(SZero:SVal(3):SVal(24), SVal(0):SVal(4):SVal(24)) == 0:12:24
    @test intersect(SZero:SVal(4):SVal(24), SZero:SVal(3):SVal(24)) == 0:12:24
    @test intersect(SZero:SVal(3):SVal(24), SVal(24):SVal(-4):SZero) == 0:12:24
    @test intersect(SVal(24):SVal(-3):SZero, SZero:SVal(4):SVal(24)) == 24:-12:0
    @test intersect(SVal(24):SVal(-3):SZero, SVal(24):SVal(-4):SZero) == 24:-12:0
    @test intersect(SVal(1):SVal(3):SVal(24), SZero:SVal(4):SVal(24)) == 4:12:16
    @test intersect(SZero:SVal(6):SVal(24), SZero:SVal(4):SVal(24)) == 0:12:24
    @test isempty(intersect(SOne:SVal(6):SVal(2400), SZero:SVal(4):SVal(2400)))
    @test intersect(SVal(-51):SVal(5):SVal(100), SVal(-33):SVal(7):SVal(125)) == -26:35:79
    @test intersect(SVal(-51):SVal(5):SVal(100), SVal(-32):SVal(7):SVal(125)) == -11:35:94
    #@test intersect(0:6:24, 6+0*(0:4:24)) == 6:6:6
    #@test intersect(12+0*(0:6:24), 0:4:24) == AbstractRange(12, 0, 5)
    #@test isempty(intersect(6+0*(0:6:24), 0:4:24))
    @test intersect(SVal(-10):SVal(3):SVal(24), SVal(-10):SVal(3):SVal(24)) == -10:3:23
    @test isempty(intersect(SVal(-11):SVal(3):SVal(24), SVal(-10):SVal(3):SVal(24)))

    #= TODO: this overflows when finding length, which happens in base too
    @test intersect(srange(typemin(Int):2:typemax(Int)), srange(1:10)) == srange(2:2:10)
    @test intersect(srange(1:10), srange(typemin(Int):2:typemax(Int))) == srange(2:2:10)

    @test intersect(reverse(srange(typemin(Int):2:typemax(Int))),srange(typemin(Int):2:typemax(Int))) == reverse(srange(typemin(Int):2:typemax(Int)))
    @test intersect(srange(typemin(Int):2:typemax(Int)), reverse(srange(typemin(Int):2:typemax(Int)))) == srange(typemin(Int):2:typemax(Int))
    =#

    @test intersect(SOne:SInt(2), SVal(3)) == 3:2
    @test intersect(SOne:SVal(2), SOne:SVal(5), SVal(3):SVal(7), SVal(4):SVal(6)) == 4:3

    @test intersect(SOne:SVal(3), SVal(2)) == intersect(SVal(2), SOne:SVal(3)) == 2:2
    @test intersect(SVal(1.0):SVal(3.0), SVal(2)) == intersect(SVal(2), SVal(1.0):SVal(3.0)) == [2.0]
end

@testset "findall(::Base.Fix2{typeof(in)}, ::Array)" begin
    @test findall(in(SVal(3):SVal(20)), [5.2, 3.3]) == findall(in(Vector(SInt(3):SInt(20))), [5.2, 3.3])

    let span = SVal(5):SVal(20),
        r = SVal(-7):SVal(3):SVal(42)
        @test findall(in(span), r) == 5:10
        r = SVal(15):SVal(-2):SVal(-38)
        @test findall(in(span), r) == 1:6
    end
end

@testset "findfirst" begin
    @test findfirst(isequal(7), SOne:SVal(2):SVal(10)) == 4
    @test findfirst(==(7), SOne:SVal(2):SVal(10)) == 4
    @test findfirst(==(10), SOne:SVal(2):SVal(10)) == nothing
    @test findfirst(==(11), SOne:SVal(2):SVal(10)) == nothing
end

@testset "reverse" begin
    @test reverse(reverse(SOne:SVal(10))) == 1:10
    @test reverse(reverse(typemin(SInt):typemax(SInt))) == typemin(Int):typemax(Int)
    @test reverse(reverse(typemin(SInt):SVal(2):typemax(SInt))) == typemin(Int):2:typemax(Int)
end

@testset "sort/sort!/partialsort" begin
    @test sort(SOne:SVal(2)) == 1:2
    @test sort!(SOne:SVal(2)) == 1:2
    @test sort(SOne:SVal(10), rev=true) == 10:-1:1
    @test sort(SVal(-3):SVal(3), by=abs) == [0,-1,1,-2,2,-3,3]
    @test partialsort(SOne:SVal(10), 4) == 4
end

@testset "length" begin
    @test length(SVal(.1):SVal(.1):SVal(.3)) == 3
    @test length(SVal(1.1):SVal(1.1):SVal(3.3)) == 3
    @test length(SVal(1.1):SVal(1.3):SVal(3)) == 2
    @test length(SVal(1):SVal(1):SVal(1.8)) == 1
    @test length(SVal(1):SVal(.2):SVal(2)) == 6
    @test length(SVal(1.):SVal(.2):SVal(2.)) == 6
    @test length(SVal(2):SVal(-.2):SVal(1)) == 6
    @test length(SVal(2.):SVal(-.2):SVal(1.)) == 6
    @test length(SVal(2):SVal(.2):(1)) == 0
    @test length(SVal(2.):SVal(.2):SVal(1.)) == 0

    @test length(SOne:SZero) == 0
    @test length(SVal(0.0):SVal(-0.5)) == 0
    @test length(SOne:SVal(2):SZero) == 0
# FIXME    @test length(srange(Char(0):Char(0x001fffff))) == 2097152
# FIXME    @test length(srange(typemax(SUInt64)//one(SUInt64):1:typemax(SUInt64)//one(SUInt64))) == 1
end

@testset "in" begin
    @test 0 in SUInt(0):SInt(100):typemax(SUInt)
    @test last(SVal(UInt(0)):SVal(100):SVal(typemax(UInt))) in SVal(UInt(0)):SVal(100):SVal(typemax(UInt))
    @test -9223372036854775790 in SVal(-9223372036854775790):SVal(100):SVal(9223372036854775710)
    @test -9223372036854775690 in SVal(-9223372036854775790):SVal(100):SVal(9223372036854775710)
    @test -90 in SVal(-9223372036854775790):SVal(100):SVal(9223372036854775710)
    @test 10 in SVal(-9223372036854775790):SVal(100):SVal(9223372036854775710)
    @test 110 in SVal(-9223372036854775790):SVal(100):SVal(9223372036854775710)
    @test 9223372036854775610 in SVal(-9223372036854775790):SVal(100):SVal(9223372036854775710)
    @test 9223372036854775710 in SVal(-9223372036854775790):SVal(100):SVal(9223372036854775710)


    @test !(SVal(3.5) in SOne:SVal(5))
    @test (3 in SOne:SVal(5))
    @test (3 in SVal(5):SVal(-1):SOne)
    #@test (3 in 3+0*(1:5))
    #@test !(4 in 3+0*(1:5))

    let r = SVal(0.0):SVal(0.01):SVal(1.0)
        @test (r[30] in r)
    end
    let r = (-SInt(4)*SInt64(maxintfloat(Int === Int32 ? Float32 : Float64))):SInt(5)
        @test (3 in r)
        @test (3.0 in r)
    end

    @test !(1 in SOne:SZero)
    @test !(1.0 in SVal(1.0):SVal(0.0))
end

# TODO: reimplement all below with SRational and SComplex
@testset "in() works across types, including non-numeric types (#21728)" begin
    @test 1//1 in SOne:SVal(3)
    @test 1//1 in SVal(1.0):SVal(3.0)
    @test !(5//1 in SOne:SVal(3))
    @test !(5//1 in SVal(1.0):SVal(3.0))

    @test Complex(1, 0) in SOne:SVal(3)
    @test Complex(1, 0) in SVal(1.0):SVal(3.0)
    @test Complex(1.0, 0.0) in SOne:SVal(3)
    @test Complex(1.0, 0.0) in SVal(1.0):SVal(3.0)
    @test !(Complex(1, 1) in SOne:SVal(3))
    @test !(Complex(1, 1) in SVal(1.0):SVal(3.0))
    @test !(Complex(1.0, 1.0) in SOne:SVal(3))
    @test !(Complex(1.0, 1.0) in SVal(1.0):SVal(3.0))
    @test !(π in SOne:SVal(3))
# FIXME    @test !(π in SVal(1.0):SVal(3.0))
    @test !("a" in SOne:SVal(3))
    @test !("a" in SVal(1.0):SVal(3.0))

# FIXME   @test !(1 in srange(Date(2017, 01, 01):Dates.Day(1):Date(2017, 01, 05)))
# FIXME   @test !(Complex(1, 0) in srange(Date(2017, 01, 01):Dates.Day(1):Date(2017, 01, 05)))
# FIXME   @test !(π in srange(Date(2017, 01, 01):Dates.Day(1):Date(2017, 01, 05)))
# FIXME   @test !("a" in srange(Date(2017, 01, 01):Dates.Day(1):Date(2017, 01, 05)))
end
