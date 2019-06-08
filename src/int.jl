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

const SUInt128One = SUInt128{UInt128(1)}()
const SUInt128Zero = SUInt128{UInt128(0)}()

const SUInt128OneType = typeof(SUInt128{UInt128(1)}())
const SUInt128ZeroType = typeof(SUInt128{UInt128(0)}())

@pure Base.one(::SUInt128) = SUInt128One
@pure Base.one(::Type{<:SUInt128}) = SUInt128One

@pure Base.isone(::SUInt128OneType) = true
@pure Base.isone(::SUInt128{T}) where T = false

@pure Base.zero(::SUInt128) = SUInt128Zero
@pure Base.zero(::Type{<:SUInt128}) = SUInt128Zero

@pure Base.iszero(::SUInt128ZeroType) = true
@pure Base.iszero(::SUInt128{T}) where T = false

const SInt128One = SInt128{Int128(1)}()
const SInt128Zero = SInt128{Int128(0)}()
const SInt128OneType = typeof(SInt128{Int128(1)}())
const SInt128ZeroType = typeof(SInt128{Int128(0)}())

@pure Base.one(::SInt128) = SInt128One
@pure Base.one(::Type{<:SInt128}) = SInt128One

@pure Base.isone(::SInt128OneType) = true
@pure Base.isone(::SInt128{T}) where T = false

@pure Base.zero(::SInt128) = SInt128Zero
@pure Base.zero(::Type{<:SInt128}) = SInt128Zero

@pure Base.iszero(::SInt128ZeroType) = true
@pure Base.iszero(::SInt128{T}) where T = false

const SUInt64One = SUInt64{UInt64(1)}()
const SUInt64Zero = SUInt64{UInt64(0)}()
const SUInt64OneType = typeof(SUInt64{UInt64(1)}())
const SUInt64ZeroType = typeof(SUInt64{UInt64(0)}())

@pure Base.one(::SUInt64) = SUInt64One
@pure Base.one(::Type{<:SUInt64}) = SUInt64One

@pure Base.isone(::SUInt64OneType) = true
@pure Base.isone(::SUInt64{T}) where T = false

@pure Base.zero(::SUInt64) = SUInt64Zero
@pure Base.zero(::Type{<:SUInt64}) = SUInt64Zero

@pure Base.iszero(::SUInt64ZeroType) = true
@pure Base.iszero(::SUInt64{T}) where T = false


const SInt64One = SInt64{1}()
const SInt64Zero = SInt64{0}()
const SInt64OneType = typeof(SInt64{1}())
const SInt64ZeroType = typeof(SInt64{0}())

@pure Base.one(::SInt64) = SInt64One
@pure Base.one(::Type{<:SInt64}) = SInt64One

@pure Base.isone(::SInt64OneType) = true
@pure Base.isone(::SInt64{T}) where T = false

@pure Base.zero(::SInt64) = SInt64Zero
@pure Base.zero(::Type{<:SInt64}) = SInt64Zero

@pure Base.iszero(::SInt64ZeroType) = true
@pure Base.iszero(::SInt64{T}) where T = false

const SUInt32One = SUInt32{UInt32(1)}()
const SUInt32Zero = SUInt32{UInt32(0)}()
const SUInt32OneType = typeof(SUInt32{UInt32(1)}())
const SUInt32ZeroType = typeof(SUInt32{UInt32(0)}())

@pure Base.one(::SUInt32) = SUInt32One
@pure Base.one(::Type{<:SUInt32}) = SUInt32One

@pure Base.isone(::SUInt32OneType) = true
@pure Base.isone(::SUInt32{T}) where T = false

@pure Base.zero(::SUInt32) = SUInt32Zero
@pure Base.zero(::Type{<:SUInt32}) = SUInt32Zero

@pure Base.iszero(::SUInt32ZeroType) = true
@pure Base.iszero(::SUInt32{T}) where T = false

const SInt32One = SInt32{Int32(1)}()
const SInt32Zero = SInt32{Int32(0)}()
const SInt32OneType = typeof(SInt32{Int32(1)}())
const SInt32ZeroType = typeof(SInt32{Int32(0)}())

@pure Base.one(::SInt32) = SInt32One
@pure Base.one(::Type{<:SInt32}) = SInt32One

@pure Base.isone(::SInt32OneType) = true
@pure Base.isone(::SInt32{T}) where T = false

@pure Base.zero(::SInt32) = SInt32Zero
@pure Base.zero(::Type{<:SInt32}) = SInt32Zero

@pure Base.iszero(::SInt32ZeroType) = true
@pure Base.iszero(::SInt32{T}) where T = false

const SUInt16One = SUInt16{UInt16(1)}()
const SUInt16Zero = SUInt16{UInt16(0)}()
const SUInt16OneType = typeof(SUInt16{UInt16(1)}())
const SUInt16ZeroType = typeof(SUInt16{UInt16(0)}())

@pure Base.one(::SUInt16) = SUInt16One
@pure Base.one(::Type{<:SUInt16}) = SUInt16One

@pure Base.isone(::SUInt16OneType) = true
@pure Base.isone(::SUInt16{T}) where T = false

@pure Base.zero(::SUInt16) = SUInt16Zero
@pure Base.zero(::Type{<:SUInt16}) = SUInt16Zero

@pure Base.iszero(::SUInt16ZeroType) = true
@pure Base.iszero(::SUInt16{T}) where T = false

const SInt16One = SInt16{Int16(1)}()
const SInt16Zero = SInt16{Int16(0)}()
const SInt16OneType = typeof(SInt16{Int16(1)}())
const SInt16ZeroType = typeof(SInt16{Int16(0)}())

@pure Base.one(::SInt16) = SInt16One
@pure Base.one(::Type{<:SInt16}) = SInt16One

@pure Base.isone(::SInt16OneType) = true
@pure Base.isone(::SInt16{T}) where T = false

@pure Base.zero(::SInt16) = SInt16Zero
@pure Base.zero(::Type{<:SInt16}) = SInt16Zero

@pure Base.iszero(::SInt16ZeroType) = true
@pure Base.iszero(::SInt16{T}) where T = false

const SUInt8One = SUInt8{UInt8(1)}()
const SUInt8Zero = SUInt8{UInt8(0)}()
const SUInt8OneType = typeof(SUInt8{UInt8(1)}())
const SUInt8ZeroType = typeof(SUInt8{UInt8(0)}())

@pure Base.one(::SUInt8) = SUInt8One
@pure Base.one(::Type{<:SUInt8}) = SUInt8One

@pure Base.isone(::SUInt8OneType) = true
@pure Base.isone(::SUInt8{T}) where T = false

@pure Base.zero(::SUInt8) = SUInt8Zero
@pure Base.zero(::Type{<:SUInt8}) = SUInt8Zero

@pure Base.iszero(::SUInt8ZeroType) = true
@pure Base.iszero(::SUInt8{T}) where T = false

const SInt8One = SInt8{Int8(1)}()
const SInt8Zero = SInt8{Int8(0)}()
const SInt8OneType = typeof(SInt8{Int8(1)}())
const SInt8ZeroType = typeof(SInt8{Int8(0)}())

@pure Base.one(::SInt8) = SInt8One
@pure Base.one(::Type{<:SInt8}) = SUInt8One

@pure Base.isone(::SInt8OneType) = true
@pure Base.isone(::SInt8{T}) where T = false

@pure Base.zero(::SInt8) = SInt8Zero
@pure Base.zero(::Type{<:SInt8}) = SInt8Zero

@pure Base.iszero(::SInt8ZeroType) = true
@pure Base.iszero(::SInt8{T}) where T = false

const SOne = SInt{1}()
const SZero = SInt{0}()

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

Base.unsigned(x::SBool{true}) = SUInt(1)
Base.unsigned(x::SBool{false}) = SUInt(0)

Base.unsigned(x::SInt8) = SUInt8(x)
Base.unsigned(x::SInt16) = SUInt16(x)
Base.unsigned(x::SInt32) = SUInt32(x)
Base.unsigned(x::SInt64) = SUInt64(x)
Base.unsigned(x::SInt128) = SUInt128(x)
Base.unsigned(x::SUnsigned) = x

Base.Unsigned(x::SInteger) = Base.unsigned(x)

Base.unsigned(::Type{<:SInt8}) = SUInt8
Base.unsigned(::Type{<:SInt16}) = SUInt16
Base.unsigned(::Type{<:SInt32}) = SUInt32
Base.unsigned(::Type{<:SInt64}) = SUInt64
Base.unsigned(::Type{<:SInt128}) = SUInt128
Base.unsigned(::Type{T}) where {T<:SUnsigned} = T

Base.Signed(x::SSigned) = x

Base.Signed(x::SUInt8{X}) where X = SInt8{Int8(X::UInt8)}()
Base.Signed(x::SUInt16{X}) where X = SInt16{Int16(X::UInt16)}()
Base.Signed(x::SUInt32{X}) where X = SInt32{Int32(X::UInt32)}()
Base.Signed(x::SUInt64{X}) where X = SInt64{Int64(X::UInt64)}()
Base.Signed(x::SUInt128{X}) where X = SInt128{Int128(X::UInt128)}()


Base.trailing_zeros(x::SInteger{X}) where X = SInt{trailing_zeros(X)}()
Base.trailing_ones(x::SInteger{X}) where X = SInt{trailing_ones(X)}()


