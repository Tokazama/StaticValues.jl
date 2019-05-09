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
const SInteger{V} = Union{SSigned{V},SUnsigned{V}}
Base.leading_zeros(::SInteger{V}) where V = leading_zeros(V)
Base.leading_ones(::SInteger{V}) where V = leading_ones(V)

# with mixed signedness and same size, Unsigned wins
promote_rule(::Type{SUInt8},   ::Type{SInt8}  ) = SUInt8
promote_rule(::Type{SUInt16},  ::Type{SInt16} ) = SUInt16
promote_rule(::Type{SUInt32},  ::Type{SInt32} ) = SUInt32
promote_rule(::Type{SUInt64},  ::Type{SInt64} ) = SUInt64
promote_rule(::Type{SUInt128}, ::Type{SInt128}) = SUInt128

promote_rule(::Type{SInt16}, ::Union{Type{SInt8}, Type{SUInt8}}) = SInt16
promote_rule(::Type{SInt32}, ::Union{Type{SInt16}, Type{SInt8}, Type{SUInt16}, Type{SUInt8}}) = SInt32
promote_rule(::Type{SInt64}, ::Union{Type{SInt16}, Type{SInt32}, Type{SInt8}, Type{SUInt16}, Type{SUInt32}, Type{SUInt8}}) = SInt64
promote_rule(::Type{SInt128}, ::Union{Type{SInt16}, Type{SInt32}, Type{SInt64}, Type{SInt8}, Type{SUInt16}, Type{SUInt32}, Type{SUInt64}, Type{SUInt8}}) = SInt128
promote_rule(::Type{SUInt16}, ::Union{Type{SInt8}, Type{SUInt8}}) = SUInt16
promote_rule(::Type{SUInt32}, ::Union{Type{SInt16}, Type{SInt8}, Type{SUInt16}, Type{SUInt8}}) = SUInt32
promote_rule(::Type{SUInt64}, ::Union{Type{SInt16}, Type{SInt32}, Type{SInt8}, Type{SUInt16}, Type{SUInt32}, Type{SUInt8}}) = SUInt64
promote_rule(::Type{SUInt128}, ::Union{Type{SInt16}, Type{SInt32}, Type{SInt64}, Type{SInt8}, Type{SUInt16}, Type{SUInt32}, Type{SUInt64}, Type{SUInt8}}) = SUInt128


static_unsigned = (SUInt128,SUInt16,SUInt32,SUInt64,SUInt8)

static_signed = (SInt128,SInt16,SInt32,SInt64,SInt8)

static_integers = (static_unsigned..., static_signed...)


for (ST,BT) in zip(static_integers, base_integers)
    @eval begin
        (&)(::$ST{V1}, ::$ST{V2}) where {V1,V2} = $ST{(&)(V1::$BT, V2::$BT)}()
        (|)(::$ST{V1}, ::$ST{V2}) where {V1,V2} = $ST{(|)(V1::$BT, V2::$BT)}()
        xor(::$ST{V1}, ::$ST{V2}) where {V1,V2} = $ST{(|)(V1::$BT, V2::$BT)}()


        >>(::$ST{V1}, ::$ST{V2}) where {V1,V2} = $ST{>>(V1::$BT, V2::$BT)}()
        <<(::$ST{V1}, ::$ST{V2}) where {V1,V2} = $ST{<<(V1::$BT, V2::$BT)}()
        >>>(::$ST{V1}, ::$ST{V2}) where {V1,V2} = $ST{>>>(V1::$BT, V2::$BT)}()
    end
end
