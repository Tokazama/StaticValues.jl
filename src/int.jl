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


Base.leading_zeros(::SUnsigned{V}) where V = leading_zeros(V)
Base.leading_ones(::SUnsigned{V}) where V = leading_ones(V)

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

Base.leading_zeros(::SSigned{V}) where V = leading_zeros(V)
Base.leading_ones(::SSigned{V}) where V = leading_ones(V)

# with mixed signedness and same size, Unsigned wins
promote_rule(::Type{<:SUInt8},   ::Type{<:SInt8}  ) = SUInt8
promote_rule(::Type{<:SUInt16},  ::Type{<:SInt16} ) = SUInt16
promote_rule(::Type{<:SUInt32},  ::Type{<:SInt32} ) = SUInt32
promote_rule(::Type{<:SUInt64},  ::Type{<:SInt64} ) = SUInt64
promote_rule(::Type{<:SUInt128}, ::Type{<:SInt128}) = SUInt128

promote_rule(::Type{<:SInt16}, ::Union{Type{<:SInt8},Type{<:SUInt8}}) = SInt16
promote_rule(::Type{<:SInt32}, ::Union{Type{<:SInt16},Type{<:SInt8},Type{<:SUInt16},Type{<:SUInt8}}) = SInt32
promote_rule(::Type{<:SInt64}, ::Union{Type{<:SInt16},Type{<:SInt32},Type{<:SInt8},Type{<:SUInt16},Type{<:SUInt32},Type{<:SUInt8}}) = SInt64
promote_rule(::Type{<:SInt128}, ::Union{Type{<:SInt16},Type{<:SInt32},Type{<:SInt64}, Type{<:SInt8},Type{<:SUInt16},Type{<:SUInt32},Type{<:SUInt64},Type{<:SUInt8}}) = SInt128
promote_rule(::Type{<:SUInt16}, ::Union{Type{<:SInt8},Type{<:SUInt8}}) = SUInt16
promote_rule(::Type{<:SUInt32}, ::Union{Type{<:SInt16},Type{<:SInt8},Type{<:SUInt16},Type{<:SUInt8}}) = SUInt32
promote_rule(::Type{<:SUInt64}, ::Union{Type{<:SInt16},Type{<:SInt32},Type{<:SInt8},Type{<:SUInt16},Type{<:SUInt32},Type{<:SUInt8}}) = SUInt64
promote_rule(::Type{<:SUInt128}, ::Union{Type{<:SInt16},Type{<:SInt32},Type{<:SInt64},Type{<:SInt8},Type{<:SUInt16},Type{<:SUInt32},Type{<:SUInt64},Type{<:SUInt8}}) = SUInt128

static_unsigned = (SUInt128,SUInt16,SUInt32,SUInt64,SUInt8)

static_signed = (SInt128,SInt16,SInt32,SInt64,SInt8)

static_integer = (static_unsigned..., static_signed...)

for (ST,BT) in zip(static_integer, base_integer)
    @eval begin
        Base.typemax(::$ST) = $ST{Base.typemax($BT)}()
        Base.typemax(::Type{$ST}) = $ST{Base.typemax($BT)}()

        Base.typemin(::$ST) = $ST{typemin($BT)}()
        Base.typemin(::Type{$ST}) = $ST{typemin($BT)}()

        (&)(::$ST{V1}, ::$ST{V2}) where {V1,V2} = $ST{(&)(V1::$BT, V2::$BT)}()
        (|)(::$ST{V1}, ::$ST{V2}) where {V1,V2} = $ST{(|)(V1::$BT, V2::$BT)}()
        xor(::$ST{V1}, ::$ST{V2}) where {V1,V2} = $ST{(|)(V1::$BT, V2::$BT)}()


        >>(::$ST{V1}, ::$ST{V2}) where {V1,V2} = $ST{>>(V1::$BT, V2::$BT)}()
        <<(::$ST{V1}, ::$ST{V2}) where {V1,V2} = $ST{<<(V1::$BT, V2::$BT)}()
        >>>(::$ST{V1}, ::$ST{V2}) where {V1,V2} = $ST{>>>(V1::$BT, V2::$BT)}()

    end
end

struct SBool{V} <: Integer
    function SBool{V}() where V
        !(typeof(V) === Bool) && throw(ArgumentError("val must be of type Bool"))
        new{V}()
    end
    SBool(v::Bool) = SBool{v,Bool}()
end

SBool(::Val{V}) where V = SBool{V}()

Base.typemin(::Type{SBool}) = SBool{false}()
Base.typemax(::Type{SBool}) = SBool{true}()

(!)(::SBool{X}) where X = SBool{!X::Bool}()

(~)(::SBool{X}) where X = SBool{!X::Bool}()
(&)(::SBool{X}, ::SBool{Y}) where {X,Y} = SBool{(&)(X::Bool, Y::Bool)}()
(|)(::SBool{X}, y::SBool{Y}) where {X,Y} = SBool{(|)(X::Bool, Y::Bool)}()

xor(x::SBool{X}, y::SBool{Y}) where {X,Y} = (X::Bool != Y::Bool)

>>(x::SBool{X}, c::SUInt) where X = SInt(X::Bool) >> c
<<(x::SBool{X}, c::SUInt) where X = SInt(X::Bool) << c
>>>(x::SBool{X}, c::SUInt) where X = SInt(X::Bool) >>> c

Base.signbit(x::SBool) = SBool{false}()
Base.sign(x::SBool) = x
abs(x::SBool) = x
abs2(x::SBool) = x
iszero(x::SBool) = !x
isone(x::SBool) = x

<(x::SBool, y::SBool) = y &! x
<=(x::SBool, y::SBool) = y |! x

## do arithmetic as Int ##

+(x::SBool) =  SInt(x)
-(x::SBool) = -SInt(x)

+(x::SBool, y::SBool) = SInt(x) + SInt(y)
-(x::SBool, y::SBool) = SInt(x) - SInt(y)
*(x::SBool, y::SBool) = x & y
^(x::SBool, y::SBool) = x | !y
^(x::Union{SSigned,SUnsigned}, y::SBool) = ifelse(y, x, one(x))

# preserve -0.0 in `false + -0.0`
+(x::SBool, y::T) where T<:AbstractFloat = ifelse(x, oneunit(y) + y, y)

+(y::AbstractFloat, x::SBool) = x + y

*(y::AbstractFloat, x::SBool) = x * y

Base.div(x::SBool, y::SBool) = y ? x : throw(DivideError())
Base.fld(x::SBool, y::SBool) = div(x,y)
Base.cld(x::SBool, y::SBool) = div(x,y)
Base.rem(x::SBool, y::SBool) = y ? false : throw(DivideError())
Base.mod(x::SBool, y::SBool) = rem(x,y)

const SInteger{V} = Union{SSigned{V},SUnsigned{V},SBool{V}}

>>(::SInteger{V1}, v2::BaseInteger) where V1 = >>(V1, v2)
>>(v2::BaseInteger, ::SInteger{V1}) where V1 = >>(v2, V1)

<<(::SInteger{V1}, v2::BaseInteger) where V1 = <<(V1, v2)
<<(v2::BaseInteger, ::SInteger{V1}) where V1 = <<(v2, V1)

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

const SOne = SInt{1}()
const SZero = SInt{0}()

Base.unsigned(x::SBool{true}) = SUInt(1)
Base.unsigned(x::SBool{false}) = SUInt(0)

Base.unsigned(x::SInt8) = SUInt8(x)
Base.unsigned(x::SInt16) = SUInt16(x)
Base.unsigned(x::SInt32) = SUInt32(x)
Base.unsigned(x::SInt64) = SUInt64(x)
Base.unsigned(x::SInt128) = SUInt128(x)
Base.unsigned(x::SUnsigned) = x

Base.unsigned(::Type{<:SInt8}) = SUInt8
Base.unsigned(::Type{<:SInt16}) = SUInt16
Base.unsigned(::Type{<:SInt32}) = SUInt32
Base.unsigned(::Type{<:SInt64}) = SUInt64
Base.unsigned(::Type{<:SInt128}) = SUInt128
Base.unsigned(::Type{T}) where {T<:SUnsigned} = T
