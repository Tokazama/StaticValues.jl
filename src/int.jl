struct SBool{V} <: Integer
    function SBool{V}() where V
        !(typeof(V) === Bool) && throw(ArgumentError("SBool only supports static Bool storage, got $(typeof(V))"))
        new{V}()
    end
end

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

SIntegerZeroType = Union{typeof(SInt8Zero),typeof(SUInt8Zero),typeof(SInt16Zero),
                         typeof(SUInt16Zero),typeof(SInt32Zero),typeof(SUInt32Zero),
                         typeof(SInt64Zero),typeof(SUInt64Zero),typeof(SInt128Zero),
                         typeof(SUInt128Zero)}
SIntegerOneType = Union{typeof(SInt8One),typeof(SUInt8One),typeof(SInt16One),typeof(SUInt16One),
                        typeof(SInt32One),typeof(SUInt32One),typeof(SInt64One),typeof(SUInt64One),
                        typeof(SInt128One),typeof(SUInt128One)}
SIntegerNegOneType = Union{SInt8{-Int8(1)},SInt16{-Int16(1)},SInt32{-Int32(1)},SInt64{-1},SInt128{-Int128(1)}}

function defint(::Type{ST}, ::Type{BT}) where {ST,BT<:BaseInteger}
    @eval begin
        Base.:(~)(::$ST{X}) where X = $ST{~X}()
        Base.:(|)(::$ST{X}, ::$ST{Y}) where {X,Y} = $ST{(|)(X::$BT, Y::$BT)}()
        Base.xor(::$ST{X}, ::$ST{Y}) where {X,Y} = $ST{xor(X::$BT, Y::$BT)}()
        Base.:(&)(::$ST{X}, ::$ST{Y}) where {X,Y} = $ST{(&)(X::$BT, Y::$BT)}()

        Base.trailing_zeros(::$ST{X}) where X = SInt{trailing_zeros(X::$BT)}()
        Base.trailing_ones(::$ST{X}) where X = SInt{trailing_ones(X::$BT)}()
        Base.count_ones(::$ST{X}) where X = SInt{count_ones(x::$BT)}()
        Base.leading_zeros(::$ST{X}) where X = SInt{leading_zeros(X::$BT)}()
        Base.leading_ones(::$ST{X}) where X = SInt{leading_ones(X::$BT)}()

        Base.flipsign(::$ST{X}, ::$ST{Y}) where {X,Y} = $ST{flipsign(X::$BT, Y::$BT)}()
        Base.flipsign(::$ST{X}, ::SSigned{Y}) where {X,Y}= $ST{flipsign(X::$BT, Y)}()

        Base.rem(x::$ST, ::Type{<:$ST{<:Any}}) = x

        Base.powermod(::$ST{X}, ::$ST{P}, ::$ST{M}) where {X,P,M} = $ST{powermod(X::$BT, P::$BT, M::$BT)}()

        @generated function Base.binomial(::$ST{N}, ::$ST{K}) where {N,K}
            x = $ST{binomial(N::$BT, K::$BT)}()
            :($x)
        end

        @generated function Base.factorial(n::$ST{N}) where N
            x = $ST{factorial(N::$BT)}()
            :($x)
        end
        @generated function Base.sqrt(::$ST{X}) where X
            x = SFloat64{sqrt(X::$BT)}()
            :($x)
        end
    end
end

for (S,B) in SI2BI
    defint(S,B)
end




#=
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
Base.abs(x::SBool) = x
Base.abs2(x::SBool) = x
Base.iszero(x::SBool) = !x
Base.isone(x::SBool) = x

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
=#
#>>(::SInteger{V1}, v2::BaseInteger) where V1 = >>(V1, v2)
#>>(v2::BaseInteger, ::SInteger{V1}) where V1 = >>(v2, V1)

#<<(::SInteger{V1}, v2::BaseInteger) where V1 = <<(V1, v2)
#<<(v2::BaseInteger, ::SInteger{V1}) where V1 = <<(v2, V1)

#>>(x::SInteger, y::SInt) = ifelse(0 <= y, x >> unsigned(y), x << unsigned(-y))

#=
function >>(x::SInteger, c::SInteger)
    Base.@_inline_meta
    typemin(SInt) <= c <= typemax(SInt) && return x >> (c % SInt)
    (x >= 0 || c < 0) && return zero(x)
    oftype(x, -SOne)
end
=#




