abstract type StaticBool{T} <: Integer end
struct SBool{V,Bool} <: StaticBool{V}
    function SBool{V,Bool}() where {V}
        !(typeof(V) === Bool) && throw(ArgumentError("val must be of type Bool"))
        new{V,Bool}()
    end

    SBool(v::Bool) = SBool{v,Bool}()
end

SBool{V}() where {V} = SBool{V,typeof(V)}()
SBool(::Val{V}) where V = SBool{V,typeof(V)}()

mutable struct MBool{T} <: StaticBool{T}
    value::T

    MBool(v::Bool) = new{Bool}(v)
    MBool(v::SBool{V,Bool}) where {V} = new{SBool{V::Bool,Bool}}(v)
    MBool(v::Val{V}) where V = MBool(SBool(v))
end

Base.Bool(::SBool{V}) where V = V::Bool


#=
typemin(::Type{SBool}) = SBool{false,Bool}()
typemax(::Type{SBool}) = SBool{true,Bool}()

(!)(::SBool{X}) where X = SBool{!X}()

(~)(::SBool{X}) where X = SBool{!X}()
(&)(::SBool{X}, ::SBool{Y}) where {X,Y} = SBool{(&)(X::Bool, Y::Bool)}()
(|)(::SBool{X}, y::SBool{Y}) where {X,Y} = SBool{(|)(X::Bool, Y::Bool)}()

xor(x::SBool{X}, y::SBool{Y}) where {X,Y} = (X::Bool != Y::Bool)

>>(x::SBool, c::SUInt) = SInt(x) >> c
<<(x::SBool, c::SUInt) = SInt(x) << c
>>>(x::SBool, c::SUInt) = SInt(x) >>> c

Base.signbit(x::SBool) = SBool{false,Bool}()
Base.sign(x::SBool) = x
Base.abs(x::SBool) = x
Base.abs2(x::SBool) = x
Base.iszero(x::SBool) = !x
Base.isone(x::SBool) = x

<(x::SBool, y::SBool) = y&!x
<=(x::SBool, y::SBool) = y|!x

## do arithmetic as Int ##

+(x::SBool) =  SInt(x)
-(x::SBool) = -SInt(x)

+(x::SBool, y::SBool) = SInt(x) + SInt(y)
-(x::SBool, y::SBool) = SInt(x) - SInt(y)
*(x::SBool, y::SBool) = x & y
^(x::SBool, y::SBool) = x | !y
^(x::SInteger, y::SBool) = ifelse(y, x, one(x))

# preserve -0.0 in `false + -0.0`
function +(x::SBool, y::T)::promote_type(Bool,T) where T<:AbstractFloat
    return ifelse(x, oneunit(y) + y, y)
end
+(y::AbstractFloat, x::SBool) = x + y

# make `false` a "strong zero": false*NaN == 0.0
*(x::SBool, y::T)::promote_type(Bool,T) where T<:AbstractFloat =
    ifelse(x, y, copysign(zero(y), y))
*(y::AbstractFloat, x::MBool) = x * y

Base.div(x::SBool, y::SBool) = y ? x : throw(DivideError())
Base.fld(x::SBool, y::SBool) = div(x,y)
Base.cld(x::SBool, y::SBool) = div(x,y)
Base.rem(x::SBool, y::SBool) = y ? false : throw(DivideError())
Base.mod(x::SBool, y::SBool) = rem(x,y)
=#
