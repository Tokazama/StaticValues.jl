
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




