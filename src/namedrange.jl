abstract type NamedRange{N,T,R} <: StaticRangeWrapper{T,R} end

Base.names(::NamedRange{N,T,R}) where {N,T,R} = N

NamedRange(n::NTuple{N,Symbol}, r::SRange) where N = NamedSRange{n}(r)
NamedRange(n::NTuple{N,Symbol}, r::MRange) where N = NamedMRange{n}(r)


struct NamedSRange{N,T,R<:SRange{T}} <: NamedRange{N,T,R}
    NamedSRange{N}(r::SRange{T}) where {N,T} = new{N,T,tyepof(r)}()
end

mutable struct NamedMRange{N,T,R} <: NamedRange{N,T,R}
    parent::R
end

Base.parent(r::NamedMRange) = r.parent

@inline getindex(r::NamedRange, i::Number) = getindex(parent(r), i)

@inline getindex(r::NamedRange, s::Symbol) = getindex(r, SSymbol(s))

@inline @generated function getindex(r::NamedRange, ::SSymbol{S}) where S
    idx = findfirst(y->y==S::Symbol, names(x))
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

#getindex(t::NamedTuple, i::Int) = getfield(t, i)
#getindex(t::NamedTuple, i::Symbol) = getfield(t, i)

#= TODO: this needs to be implemented for all types herein
function Base.similar(x::LArray{T,K,D,Syms},::Type{S},dims::NTuple{N,Int}) where {T,K,D,Syms,S,N}
    tmp = similar(x.__x,S,dims)
    LArray{S,N,typeof(tmp),Syms}(tmp)
end
=#


