abstract type NamedRange{N,T,R} <: StaticRangeWrapper{T,R} end

Base.names(r::NamedRange{N,T,R}) where {N,T,R} = N
Base.names(r::Type{<:NamedRange{N,T,R}}) where {N,T,R} = N


NamedRange(n::NTuple{N,Symbol}, r::SRange) where N = NamedSRange{n}(r)
NamedRange(n::NTuple{N,Symbol}, r::MRange) where N = NamedMRange{n}(r)


struct NamedSRange{N,T,R} <: NamedRange{N,T,R}

    function NamedSRange{N,T,R}() where {N,T,R}
        isstatic(R) || error("NamedSRange only supports static ranges.")
        new{N,T,R}()
    end
end

NamedSRange{N}(r::SRange{T}) where {N,T} = NamedSRange{N,T,typeof(r)}()

"""
    NamedMRange

A mutable range that may be indexed by 
"""
mutable struct NamedMRange{N,T,R} <: NamedRange{N,T,R}
    names::N
    parent::R

    NamedMRange{N}(r::AbstractRange{T}) where {N,T} = new{N,T,typeof(r)}(names, r)
end

Base.parent(r::NamedMRange) = getfield(r, :parent)

@inline getindex(r::NamedRange, i::Integer) = getindex(parent(r), i)

@inline getindex(r::NamedRange, s::Symbol) = getindex(r, SSymbol(s))

@inline @generated function getindex(r::NamedRange, ::SSymbol{S}) where S
    idx = findfirst(y->y==S::Symbol, names(r))
    :(parent(r)[$idx])
end

#@inline Base.setindex!(x::LArray,y,i...) = getfield(x,:__x)[i...] = y

@inline function getindex(r::NamedRange, inds::AbstractVector{<:Number})
    _namedrange(names(r)[inds], getindex(parent(r), inds))
end

_namedrange(n::NTuple{N,Symbol}, r::Vector) where N = NamedTuple{n}(r...)
_namedrange(n::NTuple{N,Symbol}, r::SRange) where N = NamedSRange{n}(r)
_namedrange(n::NTuple{N,Symbol}, r::AbstractRange) where N = NamedMRange{n}(r)

function getindex(x::NamedRange, s::AbstractArray{Symbol,1})
    NamedTuple{(s...,)}([getindex(x, si) for si in s]...)
end
