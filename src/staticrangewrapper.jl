const SRange{T} = Union{<:UnitSRange{T},<:StepSRange{T},<:StepSRangeLen{T},<:LinSRange{T},<:OffsetSRange{T}}
const MRange{T} = Union{<:UnitMRange{T},<:StepMRange{T},<:StepMRangeLen{T},<:LinMRange{T},<:OffsetMRange{T}}

abstract type StaticRangeWrapper{T,R<:StaticRange{T}} <: AbstractRange{T} end

@pure parent(r::StaticRangeWrapper{T,R}) where {T,R<:SRange} = R()::R

@inline first(r::StaticRangeWrapper) = first(parent(r))
@inline step(r::StaticRangeWrapper) = step(parent(r))
@inline last(r::StaticRangeWrapper) = last(parent(r))

@inline firstindex(r::StaticRangeWrapper) = firstindex(parent(r))
@inline lastindex(r::StaticRangeWrapper) = lastindex(parent(r))

@inline size(r::StaticRangeWrapper) = firstindex(parent(r))
@inline length(r::StaticRangeWrapper) = firstindex(parent(r))


