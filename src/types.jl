struct SUInt128{V} <: Unsigned
    function SUInt128{V}() where V
        !(typeof(V) === UInt128) && throw(ArgumentError("SUInt128 only supports static UInt128 storage, got $(typeof(V))"))
        return new{V}()
    end
end

struct SUInt64{V} <: Unsigned
    function SUInt64{V}() where V
        !(typeof(V) === UInt64) && throw(ArgumentError("SUInt64 only supports static UInt64 storage, got $(typeof(V))"))
        new{V}()
    end
end

const SUInt{V} = SUInt64{V}

struct SUInt32{V} <: Unsigned
    function SUInt32{V}() where V
        !(typeof(V) === UInt32) && throw(ArgumentError("SUInt32 only supports static UInt32 storage, got $(typeof(V))"))
        new{V}()
    end
end

struct SUInt16{V} <: Unsigned
    function SUInt16{V}() where V
        !(typeof(V) === UInt16) && throw(ArgumentError("SUInt16 only supports static UInt16 storage, got $(typeof(V))"))
        new{V}()
    end
end

struct SUInt8{V} <: Unsigned
    function SUInt8{V}() where V
        !(typeof(V) === UInt8) && throw(ArgumentError("SUInt8 only supports static UInt8 storage, got $(typeof(V))"))
        new{V}()
    end
end

const SUnsigned{V} = Union{SUInt8{V},SUInt16{V},SUInt32{V},SUInt64{V},SUInt128{V}}

SUnsigned(x::UInt8) = SUInt8(x)
SUnsigned(x::UInt16) = SUInt16(x)
SUnsigned(x::UInt32) = SUInt32(x)
SUnsigned(x::UInt64) = SUInt64(x)
SUnsigned(x::UInt128) = SUInt128(x)


function SUnsigned(val::Val{V}) where V
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
end



# SSigned
struct SInt128{V} <: Signed
    function SInt128{V}() where V
        !(typeof(V) === Int128) && throw(ArgumentError("SInt128 only supports static Int128 storage, got $(typeof(V))"))
        new{V}()
    end
end

struct SInt64{V} <: Signed
    function SInt64{V}() where V
        !(typeof(V) === Int64) && throw(ArgumentError("SInt64 only supports static Int64 storage, got $(typeof(V))"))
        new{V}()
    end
end

const SInt{V} = SInt64{V}

struct SInt32{V} <: Signed
    function SInt32{V}() where V
        !(typeof(V) === Int32) && throw(ArgumentError("SInt32 only supports static Int32 storage, got $(typeof(V))"))
        new{V}()
    end
end

struct SInt16{V} <: Signed
    function SInt16{V}() where V
        !(typeof(V) === Int16) && throw(ArgumentError("SInt16 only supports static Int16 storage, got $(typeof(V))"))
        new{V}()
    end
end

struct SInt8{V} <: Signed
    function SInt8{V}() where V
        !(typeof(V) === Int8) && throw(ArgumentError("SInt8 only supports static Int8 storage, got $(typeof(V))"))
        new{V}()
    end
end

const SSigned{V} = Union{SInt8{V},SInt16{V},SInt32{V},SInt64{V},SInt128{V}}

SSigned(x::Int8) = SInt8(x)
SSigned(x::Int16) = SInt16(x)
SSigned(x::Int32) = SInt32(x)
SSigned(x::Int64) = SInt64(x)
SSigned(x::Int128) = SInt128(x)

function SSigned(val::Val{V}) where V
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

struct SBool{V} <: Integer
    function SBool{V}() where V
        !(typeof(V) === Bool) && throw(ArgumentError("SBool only supports static Bool storage, got $(typeof(V))"))
        new{V}()
    end
end

static_unsigned = (SUInt128,SUInt16,SUInt32,SUInt64,SUInt8)

static_signed = (SInt128,SInt16,SInt32,SInt64,SInt8)

static_integer = (static_unsigned..., static_signed...)

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

Base.unsigned(::SInt8{X}) where X = SUInt8{Base.bitcast(UInt8, X::Int8)}()
Base.unsigned(::SInt16{X}) where X = SUInt16{Base.bitcast(UInt16, X::Int16)}()
Base.unsigned(::SInt32{X}) where X = SUInt32{Base.bitcast(UInt32, X::Int32)}()
Base.unsigned(::SInt64{X}) where X = SUInt64{Base.bitcast(UInt64, X::Int64)}()
Base.unsigned(::SInt128{X}) where X = SUInt128{Base.bitcast(UInt128, X::Int128)}()
Base.unsigned(x::SUnsigned) = x

Base.Unsigned(x::SInteger) = Base.unsigned(x)

Base.unsigned(::Type{<:SInt8}) = SUInt8
Base.unsigned(::Type{<:SInt16}) = SUInt16
Base.unsigned(::Type{<:SInt32}) = SUInt32
Base.unsigned(::Type{<:SInt64}) = SUInt64
Base.unsigned(::Type{<:SInt128}) = SUInt128
Base.unsigned(::Type{T}) where {T<:SUnsigned} = T

Base.signed(x::SUInt8{X}) where X = SInt8{Int8(X::UInt8)}()
Base.signed(x::SUInt16{X}) where X = SInt16{Int16(X::UInt16)}()
Base.signed(x::SUInt32{X}) where X = SInt32{Int32(X::UInt32)}()
Base.signed(x::SUInt64{X}) where X = SInt64{Int64(X::UInt64)}()
Base.signed(x::SUInt128{X}) where X = SInt128{Int128(X::UInt128)}()

Base.Signed(x::SUnsigned) = signed(x)
Base.Signed(x::SSigned) = x


Base.widen(::Type{<:SInt8{X}}) where X = SInt16
Base.widen(::Type{<:SInt16{X}}) where X = SInt32
Base.widen(::Type{<:SInt32{X}}) where X= SInt64
Base.widen(::Type{<:SInt64{X}}) where X = SInt128
Base.widen(::Type{<:SUInt8{X}}) where X = SUInt16
Base.widen(::Type{<:SUInt16{X}}) where X = SUInt32
Base.widen(::Type{<:SUInt32{X}}) where X= SUInt64
Base.widen(::Type{<:SUInt64{X}}) where X = SUInt128

struct SFloat64{V} <: AbstractFloat
    function SFloat64{V}() where V
        !(V isa Float64) && throw(ArgumentError("SFloat64 only supports static Float64 storage, got $(typeof(V))"))
        new{V}()
    end
end

struct SFloat32{V} <: AbstractFloat
    function SFloat32{V}() where V
        !(V isa Float32) && throw(ArgumentError("SFloat32 only supports static Float32 storage, got $(typeof(V))"))
        new{V}()
    end
end

struct SFloat16{V} <: AbstractFloat
    function SFloat16{V}() where V
        !(V isa Float16) && throw(ArgumentError("SFloat16 only supports static Float16 storage, got $(typeof(V))"))
        new{V}()
    end
end

const SFloat{V} = Union{SFloat16{V},SFloat32{V},SFloat64{V}}


function SFloat(val::Val{V}) where V
    if V isa Float16
        SFloat16{V}()
    elseif V isa Float32
        SFloat32{V}()
    else
        SFloat64{V}()
    end
end

Base.show(io::IO, ::SFloat{V}) where V = show(io, V)
Base.show(io::IO, ::SInteger{V}) where V = show(io, V)


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

SUI2BUI = Dict(SUInt128 => UInt128,
               SUInt16 => UInt16,
               SUInt32 => UInt32,
               SUInt64 => UInt64,
               SUInt8 => UInt8)
BUI2SUI = Dict(UInt128 => SUInt128,
             UInt16 => SUInt16,
             UInt32 => SUInt32,
             UInt64 => SUInt64,
             UInt8 => SUInt8)

SSI2BSI = Dict(SInt128 => Int128,
               SInt16 => Int16,
               SInt32 => Int32,
               SInt64 => Int64,
               SInt8 => Int8)
BSI2SSI = Dict(Int128 => SInt128,
               Int16 => SInt16,
               Int32 => SInt32,
               Int64 => SInt64,
               Int8 => SInt8)


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

SF2BF = Dict(SFloat64 => Float64,
             SFloat32 => Float32,
             SFloat16 => Float16)

BF2SF = Dict(Float64 => SFloat64,
             Float32 => SFloat32,
             Float16 => SFloat16)

for (SIT,BIT) in SI2BI
    BFT = float(BIT)
    SFT = BF2SF[BFT]
    @eval begin
        Base.AbstractFloat(::$SIT{X}) where X = $SFT{$BFT(X::$BIT)}()
    end
end
Base.AbstractFloat(::SBool{X}) where X = SFloat64{Float64(X::Bool)}()

for (ST,BT) in S2B
    defbasics(ST, BT)
    defbool(ST, BT)
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

for (ST,BT) in SI2BI
    defint(ST,BT)
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

for (ST1,BT1) in S2B
    for (ST2,BT2) in S2B
        if BT1 != BT2
            eval(:((::Type{$BT2})(::$ST1{X}) where X = $BT2(X::$BT1)))
            if ST2 != SFloat32 && ST1 != SFloat16
                eval(:((::Type{<:$ST2{<:Any}})(::$ST1{X}) where X = $ST2{$BT2(X::$BT1)}()))
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
