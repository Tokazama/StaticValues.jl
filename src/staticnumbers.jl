
(::Type{SInteger{V,T}})(x::T) where {V,T<:Integer} = SUnsigned{x,T}()
(::Type{SInteger{V,T1}})(x::T2) where {V,T1<:Integer,T2<:Integer} = SUnsigned{T1(x)::T1,T1}()

@pure Base.values(::SInteger{V,T}) where {V,T} = V::T


Base.leading_ones(::SUnsigned{V,T}) where {V,T} = leading_ones(V::T)

"""
    StaticReal{T}
"""
const StaticReal{T} = Union{StaticFloat{T},StaticInteger{T},StaticIrrational{T},StaticRational{T}}

const SReal{V,T} = Union{SFloat{V,T},SInteger{V,T},SIrrational{V,T},SRational{V,T}}
SReal(v::Integer) = SInteger(v)
SReal(v::StaticInteger) = SInteger(v)
SReal(v::AbstractFloat) = SFloat(v)
SReal(v::AbstractIrrational) = SIrrational(v)
SReal(v::StaticRational) = SRational(v)

const MReal{T} = Union{MFloat{T},MInteger{T},MIrrational{T},MRational{T}}
MReal(v::Integer) = MInteger(v)
MReal(v::StaticInteger) = MInteger(v)
MReal(v::AbstractFloat) = MFloat(v)
MReal(v::AbstractIrrational) = MIrrational(v)
MReal(v::StaticRational) = MRational(v)


# TODO: StaticComplex
abstract type StaticComplex{T} <: Number end

# V is (Imaginary,Real)
#struct SComplex{V,T} <: StaticComplex{T} end
struct SComplex{V<:Tuple{I where I,R where R},T} <: StaticComplex{T}
    SComplex(i::T, r::T) where T<:Real = new{Tuple{i,i},T}()
end
SComplex(x::Real, y::Real) = SComplex(promote(x,y)...)
SComplex(x::Real) = SComplex(x, zero(x))

mutable struct MComplex{T} <: StaticComplex{T}
    value::T
end


"""
    StaticNumber{T}
"""
const StaticNumber{T} = Union{StaticReal{T},StaticComplex{T}}

const SNumber{V,T} = Union{SComplex{V,T},SReal{V,T}}

const MNumber{T} = Union{MComplex{T},MReal{T}}

StaticNumber(x::Real) = StaticReal(x)
StaticNumber(x::SNumber) = SNumber(x)
StaticNumber(x::MNumber) = MNumber(x)
function StaticNumber(v::Val{V}) where V
    if isa(V,Real)
        SReal(v)
    elseif isa(V,Complex)
        SComplex(v)
    else
        @error "Val must contain subtype of Number, got typeof(V) = $(typeof(V))"
    end
end

SNumber(x::Complex) = SComplex(x)
SNumber(x::StaticComplex) = SComplex(x)
SNumber(x::Real) = SReal(x)


(::Type{T1})(x::SNumber{V,T2}) where {T1,V,T2} = T1(V::T2)
(::Type{SNumber{V,T1}})(x::T2) where {V,T1,T2} = SNumber(T1(x)::T1)
(::Type{SNumber{V,T}})(x::SNumber{V2,T2}) where {V,T<:Number,V2,T2<:Number} = SNumber{T(V2::T2)::T,T}()


MNumber(x::Complex) = MComplex(x)
MNumber(x::StaticComplex) = MComplex(x)
MNumber(x::Real) = MReal(x)

Base.flipsign(x::T1, y::T2) where {T1<:StaticNumber,T2<:StaticNumber} = 
    convert(promote_type(T1,T2), flipsign(values(x), values(y)))
Base.flipsign(x::StaticNumber, y::Number) = oftype(x, flipsign(values(x), y))
Base.flipsign(x::Number, y::StaticNumber) = oftype(flipsign(x, values(y)))

#=
(::Type{T1})(x::MNumber{T2}) where {T1,T2} = T(x.value::V)

(::Type{MNumber{T1}})(x::T2) where {T1<:Number,T2<:Number} = MNumber(T1(x)::T1)
(::Type{MNumber{T}})(x::T) where {T<:Number} = MNumber(x)
(::Type{MNumber{T1}})(x::MNumber{T2}) where {T1<:Number,T2<:Number} = MNumber(T1(values(x))::T1)
(::Type{MNumber{T}})(x::MNumber{T}) where {T<:Number} = x
(::Type{MNumber{T}})(x::MNumber{SNumber{V,T}}) where {V,T<:Number} = MNumber(V::T)
(::Type{MNumber{T1}})(x::MNumber{SNumber{V,T2}}) where {V,T1<:Number,T2<:Number} = MNumber(T1(V::T2)::T1)

(::Type{MNumber{SNumber{V,T1}}})(x::T2) where {V,T1<:Number,T2<:Number} = MNumber(SNumber(T1(x)::T1))

(::Type{MNumber{SNumber{V,T}}})(x::T) where {V,T<:Number} = MNumber(SNumber(T(x)::T))
(::Type{MNumber{SNumber{V,T1}}})(x::MNumber{T2}) where {V,T1<:Number,T2<:Number} = MNumber(SNumber(T1(values(x))::T1))
(::Type{MNumber{SNumber{V,T}}})(x::MNumber{T}) where {V,T<:Number} = MNumber(SNumber(values(x)))
(::Type{MNumber{SNumber{V1,T}}})(x::MNumber{SNumber{V2,T}}) where {V1,V2,T<:Number} = x
(::Type{MNumber{SNumber{V1,T1}}})(x::MNumber{SNumber{V2,T2}}) where {V1,T1<:Number,V2,T2<:Number} =
    MNumber(SNumber{T1(V2::T2)::T1}())
## Conversions ##
#=
Base.convert(::Type{SVal{V,T}}, x) where {V,T} = SVal{oftype(V, x),T}()
Base.convert(::Type{SVal{V,T}}, ::SVal{V2,T2}) where {V,T,V2,T2} = SVal{T(V2),T}()
Base.convert(::Type{T}, x::SVal{V,T}) where {V,T} = V
Base.convert(::Type{T1}, x::SVal{V,T2}) where {V,T1,T2} = T2(V)
=#

# Bool
(::Type{T})(x::SBool{V}) where {T,V} = T(V)
(::Type{SBool{V,T}})(x::Number) where {V,T} = SBool(Bool(x))
(::Type{SBool{V,T}})(x::SBool) where {V,T} = x

(::Type{T1})(x::MBool{T2}) where {T1,T2} = T(x.value::V)
(::Type{MBool{Bool}})(x::MBool{T}) where T = MBool(Bool(x.value))
(::Type{MBool{SBool}})(x::Number) where T = MBool(SBool)


# promote rules
Base.promote_rule(::Type{SBool}, ::Type{T}) where {T<:Number} = T
Base.promote_rule(::Type{MBool}, ::Type{T}) where {T<:Number} = T

Base.promote_rule(::Type{MBool{SBool}}, ::Type{MBool{Bool}}) = MBool{Bool}
Base.promote_rule(::Type{T}, ::Type{SBool}) where {T<:MBool} = MBool{T}
=#
