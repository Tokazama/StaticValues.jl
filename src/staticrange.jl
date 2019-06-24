"""
    StaticRange{T,L}

Abstract super type for mutable and static ranges that mirror those found in `Base`.
"""
abstract type StaticRange{T,L} <: AbstractRange{T} end

@pure length(::StaticRange{T,L}) where {T,L<:SInteger} = L()
@pure length(::Type{<:StaticRange{T,L}}) where {T,L<:SInteger} = L()

# this will facilitate indirect indexing through custom indices
firstindex(r::StaticRange) = _firstindex(IndexStyle(r), r)
firstindex(r::Type{<:StaticRange}) = _firstindex(IndexStyle(r), r)

@pure _firstindex(::IndexLinear, ::StaticRange) = SOne
@pure _firstindex(indstyle::IndexLinear, ::Type{<:StaticRange}) = SOne

@inline _firstindex(indstyle::StaticRange, ::StaticRange) = first(indstyle)

lastindex(r::StaticRange) = _lastindex(IndexStyle(r), r)

@inline _lastindex(::IndexLinear, r::StaticRange) = length(r)
@inline _lastindex(::IndexLinear, r::Type{<:StaticRange}) = length(r)

@inline _lastindex(indstyle::StaticRange, ::StaticRange) = length(indstyle)

Base.checkindex(::Type{Bool}, r::StaticRange, i::Real) =
    firstindex(r) <= i && i <= lastindex(r)

Base.checkindex(::Type{Bool}, r::StaticRange, i::AbstractRange) =
    checkindex(Bool, r, first(i)) && checkindex(Bool, r, last(i))

function _in_range(x, r::StaticRange)
    if step(r) == 0
        return !isempty(r) && first(r) == x
    else
        n = round(Integer, (x - first(r)) / step(r)) + SOne
        return n >= 1 && n <= length(r) && r[n] == x
    end
end

Base.show(io::IO, r::StaticRange) = showrange(io, r)
Base.show(io::IO, ::MIME"text/plain", r::StaticRange) = showrange(io, r)
