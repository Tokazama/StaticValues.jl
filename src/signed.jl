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

Base.widen(::Type{<:SInt8{X}}) where X = SInt16
Base.widen(::Type{<:SInt16{X}}) where X = SInt32
Base.widen(::Type{<:SInt32{X}}) where X= SInt64
Base.widen(::Type{<:SInt64{X}}) where X = SInt128
Base.widen(::Type{<:SInt128{X}}) where X = SInt128  # Note: there's no `SBigInt` so max out at 128

Base.Signed(x::SSigned) = x


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

const AbstractInt128 = Union{SInt128,Int128}
const AbstractInt64  = Union{SInt64,Int64}
const AbstractInt32  = Union{SInt32,Int32}
const AbstractInt16  = Union{SInt16,Int16}
const AbstractInt8   = Union{SInt8,Int8}

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

for (ST,BT) in SSI2BSI
    defbasics(ST, BT)
    defbool(ST, BT)
end

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
