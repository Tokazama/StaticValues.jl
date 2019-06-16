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

Base.widen(::Type{<:SUInt8{X}}) where X = SUInt16
Base.widen(::Type{<:SUInt16{X}}) where X = SUInt32
Base.widen(::Type{<:SUInt32{X}}) where X= SUInt64
Base.widen(::Type{<:SUInt64{X}}) where X = SUInt128
Base.widen(::Type{<:SUInt128{X}}) where X = SUInt128

Base.unsigned(::Type{T}) where {T<:SUnsigned} = T
Base.unsigned(x::SUnsigned) = x

Base.Signed(x::SUnsigned) = signed(x)

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

const AbstractUInt128 = Union{SUInt128,UInt128}
const AbstractUInt64  = Union{SUInt64,UInt64}
const AbstractUInt32  = Union{SUInt32,UInt32}
const AbstractUInt16  = Union{SUInt16,UInt16}
const AbstractUInt8   = Union{SUInt8,UInt8}

SUI2BUI = Dict(SUInt128 => UInt128,
               SUInt16 => UInt16,
               SUInt32 => UInt32,
               SUInt64 => UInt64,
               SUInt8 => UInt8)

