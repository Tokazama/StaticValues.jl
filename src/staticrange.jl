"""
    StaticRange{T,L}
"""
abstract type StaticRange{T,L} <: AbstractRange{T} end

@pure length(::StaticRange{T,L}) where {T,L<:SInteger} = L()
@pure length(::Type{<:StaticRange{T,L}}) where {T,L<:SInteger} = L()

# this will facilitate indirect indexing through custom indices
firstindex(r::StaticRange) = firstindex(IndexStyle(r), r)
firstindex(r::Type{<:StaticRange}) = lastindex(IndexStyle(r), r)

firstindex(::IndexLinear, ::StaticRange) = SOne
firstindex(::IndexLinear, ::Type{<:StaticRange}) = SOne

@inline lastindex(::IndexLinear, r::StaticRange) = length(r)
@inline lastindex(::IndexLinear, r::Type{<:StaticRange}) = length(r)

Base.show(io::IO, r::StaticRange) = showrange(io, r)
Base.show(io::IO, ::MIME"text/plain", r::StaticRange) = showrange(io, r)
