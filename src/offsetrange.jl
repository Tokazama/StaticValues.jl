abstract type StaticOffsetRange{T,P<:StaticRange{T},F} <: AbstractRange{T} end

@pure indexoffset(r::StaticOffsetRange{T,P,F}) where {T,P,F<:SInteger} = F()::F

first(r::StaticOffsetRange) = first(parent(r))
step(r::StaticOffsetRange) = step(parent(r))
last(r::StaticOffsetRange) = last(parent(r))

firstindex(r::StaticOffsetRange) = firstindex(parent(r)) + indexoffset(r)
lastindex(r::StaticOffsetRange) = lastindex(parent(r)) + indexoffset(r)

@inline Base.@propagate_inbounds function getindex(r::StaticOffsetRange, i::Integer)
    Base.@_inline_meta
    parent(r)[i - indexoffset(r)]
end

Base.show(io::IO, r::StaticOffsetRange) = print(io, parent(r))

struct OffsetSRange{T,P,F} <: StaticOffsetRange{T,P,F}
    function OffsetSRange{T,P,F}() where {T,P,F}
        check_params(OffsetSRange{T,P,F})
        new{T,P,F}()
    end
end

@pure parent(r::OffsetSRange{T,P}) where {T,P} = P()::P

mutable struct OffsetMRange{T,P,F} <: StaticOffsetRange{T,P,F}
    parent::P
    offset::F
end

indexoffset(r::OffsetMRange{T,P,F}) where {T,P,F<:BaseInteger} = r.offset
parent(r::OffsetMRange{T,P}) where {T,P} = r.parent::P

function check_params(::Type{OffsetSRange{T,P,F}}) where {T,P,F}
    isstatic(P) || error("OffsetSRange only supports static ranges, got range of type $(nameof(T))")
end
