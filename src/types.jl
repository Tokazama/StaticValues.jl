struct SBool{V} <: Integer
    function SBool{V}() where V
        !(typeof(V) === Bool) && throw(ArgumentError("SBool only supports static Bool storage, got $(typeof(V))"))
        new{V}()
    end
end

#static_unsigned = (SUInt128,SUInt16,SUInt32,SUInt64,SUInt8)

#static_signed = (SInt128,SInt16,SInt32,SInt64,SInt8)

#static_integer = (static_unsigned..., static_signed...)

const SInteger{V} = Union{SSigned{V},SUnsigned{V},SBool{V}}
function SInteger(val::Val{V}) where V
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
end

SInteger(x::Signed) = SSigned(x)

SInteger(x::Unsigned) = SUnsigned(x)

Base.unsigned(x::SBool{true}) = SUInt(1)
Base.unsigned(x::SBool{false}) = SUInt(0)


Base.Unsigned(x::SInteger) = Base.unsigned(x)



Base.show(io::IO, ::SInteger{V}) where V = show(io, V)

"""
    SRational

# Examples
```jldoctest
julia> SInt(3) // SInt(5)
3//5

julia> (SInt(3) // SInt(5)) // (SInt(2) // SInt(1))
3//10
```
"""
struct SRational{N<:SInteger,D<:SInteger} <: Real end


const SReal{V} = Union{SInteger{V},SFloat{V},SRational{V}}

SReal(x::BaseInteger) = SInteger(x)
SReal(x::BaseFloat) = SFloat(x)
SReal(x::Rational) = SRational(x)

Base.promote_eltype(x::SReal, y::BaseNumber) = promote_type(eltype(x), eltype(y))
Base.promote_eltype(x::BaseNumber, y::SReal) = promote_type(eltype(x), eltype(y))
Base.promote_eltype(x::SReal, y::SReal) = promote_type(eltype(x), eltype(y))

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
    end
end


"""
    SComplex{I,R}
"""
struct SComplex{I,R} <: Number
    SComplex(i::T, r::T) where T<:Real = new{i,r}()
end

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
        end
    elseif V isa Complex
        SComplex(val)
    end
end

struct SChar{C} <: AbstractChar
    function SChar{C}() where C
        !(C isa Char) && throw(ArgumentError("SChar only supports static Char storage, got $(typeof(C))"))
        return new{C}()
    end
end
SChar(c::Char) = SChar{c}()
Base.values(::SChar{C}) where C = C::Char

Base.isvalid(::SChar{C}) where C = isvalid(C::Char)
Base.show(io::IO, x::SChar{C}) where C = print(io, C::Char)

struct SSymbol{S}
    function SSymbol{S}() where S
        !(S isa Symbol) && throw(ArgumentError("SSymbol only supports static Symbol storage, got $(typeof(S))"))
        return new{S}()
    end
end

SSymbol(s::Symbol) = SSymbol{s}()
Base.values(::SSymbol{S}) where S = S::Symbol

Base.show(io::IO, x::SSymbol{S}) where S = print(io, S::Symbol)

const AbstractSymbol = Union{SSymbol{<:Any},Symbol}

"""
    TPVal{H,L}
"""
struct TPVal{H,L}
    function TPVal{H,L}() where {H,L}
        if eltype(H) == eltype(L)
            return new{H,L}()
        else
            error("high and low precision values must be of the same type but got,
                  $(eltype(H)) and $(eltype(L)).")
        end
    end
end


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


# static to base dict

S2B = Dict(SUInt128 => UInt128,
           SUInt16 => UInt16,
           SUInt32 => UInt32,
           SUInt64 => UInt64,
           SUInt8 => UInt8,

           SInt128 => Int128,
           SInt16 => Int16,
           SInt32 => Int32,
           SInt64 => Int64,
           SInt8 => Int8,

           SFloat64 => Float64,
           SFloat32 => Float32,
           SFloat16 => Float16)

B2S = Dict(UInt128 => SUInt128,
           UInt16 => SUInt16,
           UInt32 => SUInt32,
           UInt64 => SUInt64,
           UInt8 => SUInt8,

           Int128 => SInt128,
           Int16 => SInt16,
           Int32 => SInt32,
           Int64 => SInt64,
           Int8 => SInt8,

           Float64 => SFloat64,
           Float32 => SFloat32,
           Float16 => SFloat16)

SI2BI = Dict(SUInt128 => UInt128,
             SUInt16 => UInt16,
             SUInt32 => UInt32,
             SUInt64 => UInt64,
             SUInt8 => UInt8,

             SInt128 => Int128,
             SInt16 => Int16,
             SInt32 => Int32,
             SInt64 => Int64,
             SInt8 => Int8)

BUI2SUI = Dict(UInt128 => SUInt128,
             UInt16 => SUInt16,
             UInt32 => SUInt32,
             UInt64 => SUInt64,
             UInt8 => SUInt8)


BI2SI = Dict(UInt128 => SUInt128,
             UInt16 => SUInt16,
             UInt32 => SUInt32,
             UInt64 => SUInt64,
             UInt8 => SUInt8,

             Int128 => SInt128,
             Int16 => SInt16,
             Int32 => SInt32,
             Int64 => SInt64,
             Int8 => SInt8)

for (ST,BT) in S2B
    defbasics(ST, BT)
    defbool(ST, BT)
end


for (SIT,BIT) in SI2BI
    BFT = float(BIT)
    SFT = BF2SF[BFT]
    @eval begin
        Base.AbstractFloat(::$SIT{X}) where X = $SFT{$BFT(X::$BIT)}()
    end
end

Base.AbstractFloat(::SBool{X}) where X = SFloat64{Float64(X::Bool)}()

for (SI,BI) in SSI2BSI
    BUI = unsigned(BI)
    SUI = BUI2SUI[BUI]
    @eval begin
        Base.unsigned(::$SI{X}) where X = $SUI{Base.bitcast($BUI, X::$BI)::$BUI}()
        Base.unsigned(::Type{<:$SI{X}}) where X = $SUI

        Base.signed(::$SUI{X}) where X = $SI{$BI(X::$BUI)::$BI}()
        Base.signed(::Type{<:$SUI{X}}) where X = $SI
    end
end


SUInt128One, SUInt128Zero = defmath(SUInt128, UInt128)

SUInt64One, SUInt64Zero = defmath(SUInt64, UInt64)
SUInt64ZeroType = typeof(SUInt64Zero)
SUInt64OneType = typeof(SUInt64One)

SUInt32One, SUInt32Zero = defmath(SUInt32, UInt32)
SUInt32ZeroType = typeof(SUInt32Zero)
SUInt32OneType = typeof(SUInt32One)

SUInt16One, SUInt16Zero = defmath(SUInt16, UInt16)
SUInt8One, SUInt8Zero = defmath(SUInt8, UInt8)

SInt128One, SInt128Zero = defmath(SInt128, Int128)
SInt128NegOne = - SInt128One

SInt64One, SInt64Zero = defmath(SInt64, Int64)
const SZero = SInt64Zero
const SOne = SInt64One


SInt64NegOne = - SInt64One

SInt32One, SInt32Zero = defmath(SInt32, Int32)
SInt32NegOne = - SInt32One

SInt16One, SInt16Zero = defmath(SInt16, Int16)
SInt16NegOne = - SInt16One

SInt8One, SInt8Zero = defmath(SInt8, Int8)
SInt8NegOne = - SInt8One



SFloat64One, SFloat64Zero = defmath(SFloat64, Float64)
SFloat32One, SFloat32Zero = defmath(SFloat32, Float32)
SFloat16One, SFloat16Zero = defmath(SFloat16, Float16)

for (S,B) in SI2BI
    defint(S,B)
end

for (S,B) in SF2BF
    deffloat(S,B)
end

#Base.signed(x::Unsigned) = reinterpret(typeof(convert(Signed, zero(x))), x

for (ST1,BT1) in S2B
    for (ST2,BT2) in S2B
        if BT1 != BT2
            BASERULE = promote_rule(BT1, BT2)
            if BASERULE != Union{}
                NEWRULE = B2S[BASERULE]
                @eval begin
                    promote_rule(::Type{<:$ST1{<:Any}}, ::Type{$BT2}) = $BASERULE
                    promote_rule(::Type{<:$ST2{<:Any}}, ::Type{$BT1}) = $BASERULE
                    promote_rule(::Type{<:$ST1{<:Any}}, ::Type{<:$ST2}) = $NEWRULE
                end
            end
        end
    end
end

promote_rule(::Type{<:AbstractIrrational}, ::Type{SFloat16}) = SFloat16
promote_rule(::Type{<:AbstractIrrational}, ::Type{SFloat32}) = SFloat32
promote_rule(::Type{<:AbstractIrrational}, ::Type{T}) where {T<:SReal} = promote_type(SFloat64, T)

for (ST1,BT1) in S2B
    for (ST2,BT2) in S2B
        if BT1 != BT2
            @eval begin
                @generated function (::Type{<:$ST2{<:Any}})(::$ST1{X}) where X
                    ret = $ST2{$BT2(X::$BT1)}()
                    :($ret)
                end
                (::Type{$BT2})(::$ST1{X}) where X = $BT2(X::$BT1)
                ofeltype(::Type{$BT1}, val::$ST2{V}) where V = $ST1{$BT1(V::$BT2)}()

                ofeltype(::Type{$BT1}, val::$BT2) = $BT1(val)
                ofeltype(::$BT1, val::$ST2{V}) where V = $ST1{$BT1(V::$BT2)}()
                ofeltype(::$BT1, val::$BT2) = $BT1(val)

                ofeltype(::Type{$ST1}, val::$ST2{V}) where V = $ST1{$BT1(V::$BT2)}()
                ofeltype(::Type{$ST1}, val::$BT2) = $BT1(val)
                ofeltype(::$ST1, val::$ST2{V}) where V = $ST1{$BT1(V::$BT2)}()
                ofeltype(::$ST1, val::$BT2) = $BT1(val)

                Base.flipsign(::$ST1{V1}, ::$ST2{V2}) where {V1,V2} = $ST1{flipsign(V1::$BT1,V2::$BT2)}()
                Base.copysign(::$ST1{V1}, ::$ST2{V2}) where {V1,V2} = $ST1{copysign(V1::$BT1,V2::$BT2)}()
            end
        end
    end
end
SIntegerZeroType = Union{typeof(SInt8Zero),typeof(SUInt8Zero),typeof(SInt16Zero),
                         typeof(SUInt16Zero),typeof(SInt32Zero),typeof(SUInt32Zero),
                         typeof(SInt64Zero),typeof(SUInt64Zero),typeof(SInt128Zero),
                         typeof(SUInt128Zero)}
SIntegerOneType = Union{typeof(SInt8One),typeof(SUInt8One),typeof(SInt16One),typeof(SUInt16One),
                        typeof(SInt32One),typeof(SUInt32One),typeof(SInt64One),typeof(SUInt64One),
                        typeof(SInt128One),typeof(SUInt128One)}
SIntegerNegOneType = Union{SInt8{-Int8(1)},SInt16{-Int16(1)},SInt32{-Int32(1)},SInt64{-1},SInt128{-Int128(1)}}

SRational(num::SIntegerZeroType, den::SIntegerZeroType) =
    throw("invalid rational: zero($(eltype(num))//zero($(eltype(den))")
