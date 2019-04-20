"""
    StaticFloat{T}
"""
abstract type StaticFloat{T} <: AbstractFloat end

struct SFloat{V,T<:AbstractFloat} <: StaticFloat{T}
    function SFloat{V,T}() where {V,T<:AbstractFloat}
        !(typeof(V) === T) && throw(ArgumentError("val must be of type $T"))
        new{V,T}()
    end
end

mutable struct MFloat{T} <: StaticFloat{T}
    value::T
end

SFloat(v::T) where T<:AbstractFloat = SFloat{V,T}()
SFloat{V}() where V = SFloat{V,typeof(V)}()
SFloat(::Val{V}) where V = SFloat{V}()
SFloat(v::SFloat) = v
SFloat(v::MFloat{T}) where T = SFloat(v.value::T)


MFloat(v::T) where T<:AbstractFloat = MFloat{T}(v)
MFloat(v::T) where T<:MFloat = v
MFloat(v::SFloat{V,T}) where {V,T<:AbstractFloat} = MFloat{SFloat{V::T,T}}(v)
MFloat(v::Val{V}) where V = MFloat(SFloat(v))
