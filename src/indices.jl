abstract type AbstractIndices{T,N,I<:Tuple,L} <: AbstractArray{T,N} end

_axeltype(inds::Tuple) = (eltype(first(inds)), _axeltype(Base.tail(inds))...)
_axeltype(inds::Tuple{R1,R2}) where {R1,R2} = (eltype(first(inds)), eltype(last(inds)))
_axeltype(inds::Tuple{R}) where R<:AbstractRange = (eltype(first(inds)),)

_axlength(inds::Tuple) = length(first(inds)) * _axlength(Base.tail(inds))
_axlength(inds::Tuple{R}) where R = length(first(inds))

"""
    StaticCartesianIndices{T,N,I,L}

Indexing returns a tuple of length N and type T

# Examples
```jldoctest

julia> x = SOne:SInt(10)
SInteger(1):SInteger(10)

julia> y = SOne:SInt(5)
SInteger(1):SInteger(5)

julia> a = CartesianSIndices((x,y));

```
"""
abstract type StaticCartesianIndices{T<:Tuple,N,I,L} <: AbstractIndices{T,N,I,L} end

struct CartesianSIndices{T,N,I,L} <: StaticCartesianIndices{T,N,I,L} end

CartesianSIndices(inds::SRange...) = CartesianSIndices(inds)
CartesianSIndices(inds::Tuple{Vararg{<:SRange,N}}) where N =
    CartesianSIndices{Tuple{_axeltype(inds)...},N,typeof(inds),_axlength(inds)}()

mutable struct CartesianMIndices{T,N,I} <: StaticCartesianIndices{T,N,I,Dynamic}
    indices::I
end

CartesianMIndices(inds::Vararg{<:AbstractRange,N}) where N = CartesianMIndices(inds)

function CartesianMIndices(inds::Tuple{Vararg{<:AbstractRange,N}}) where N
    CartesianMIndices{Tuple{_axeltype(inds)...},N,typeof(inds)}(inds)
end

"""
    StaticLinearIndices

Indexing returns a single value.
"""
abstract type StaticLinearIndices{T,N,I,L} <: AbstractIndices{T,N,I,L} end

"""
    LinearSIndices
"""
struct LinearSIndices{T,N,I,L} <: StaticLinearIndices{T,N,I,L} end

LinearSIndices(inds::Vararg{<:SRange,N}) where N = LinearSIndices(inds)

function LinearSIndices(inds::Tuple{Vararg{<:SRange,N}}) where N
    LinearSIndices{typejoin(_axeltype(inds)...),N,typeof(inds),_axlength(inds)}()
end

"""
    LinearMIndices
"""
mutable struct LinearMIndices{T,N,I} <: StaticLinearIndices{T,N,I,Dynamic}
    indices::I
end

LinearMIndices(inds::Vararg{<:AbstractRange,N}) where N = LinearMIndices(inds)
LinearMIndices(inds::Tuple{Vararg{<:AbstractRange,N}}) where N =
    LinearMIndices{typejoin(_axeltype(inds)...),N,typeof(inds)}(inds)

const SIndices{T,N,I,L} = Union{LinearSIndices{T,N,I,L},CartesianSIndices{T,N,I,L}}

const MIndices{T,N,I} = Union{LinearMIndices{T,N,I},CartesianMIndices{T,N,I}}

@pure Base.axes(::SIndices{T,N,I,L}) where {T,N,I,L} = ntuple(i->fieldtype(I, i)(), Val(N))::I
@pure Base.axes(::Type{<:SIndices{T,N,I,L}}) where {T,N,I,L} = ntuple(i->fieldtype(I, i)(), Val(N))::I


Base.axes(inds::MIndices) = inds.indices

@inline Base.axes(inds::AbstractIndices, i::Int) = axes(inds)[i]
@inline Base.axes(inds::Type{<:SIndices}, i::Int) = axes(inds)[i]
@inline Base.axes(inds::AbstractIndices, i::SInt{I}) where I = axes(inds)[I]

@inline Base.size(inds::AbstractIndices, i::Int) = length(axes(inds, i))::Int

@inline Base.size(inds::Type{<:SIndices}, i::Int) = length(axes(inds, i))

@inline Base.size(inds::AbstractIndices{T,N}) where {T,N} = (length.(axes(inds)))

@pure Base.length(inds::SIndices{T,N,I,L}) where {T,N,I,L} = L

@inline Base.length(inds::MIndices) = prod(size(inds))

function getindex(inds::StaticLinearIndices{T,N,I,L}, i::Int) where {T,N,I,L}
    Base.@_inline_meta
    @boundscheck if firstindex(inds) > i || i > lastindex(inds)
        throw(BoundsError(inds, i))
    end
    @inbounds T(getindex(firstindex(inds):lastindex(inds), i))  # just incase someone defines crazy first/last indexing
end


function getindex(inds::StaticLinearIndices{T,N,I,L}, i::AbstractRange) where {T,N,I,L}
    Base.@_inline_meta
    @boundscheck if firstindex(inds) > first(i) || last(i) > lastindex(inds)
        throw(BoundsError(inds, i))
    end
    @inbounds getindex(firstindex(inds):lastindex(inds), i)
end

#function checkbounds(::Type{Bool}, inds::AbstractIndices, )
#end

# TODO
#@generated function to_subindices(::IndexCartesian, ::Size{S}, subinds::SAxesUnion{N,Ax}, i::Int) where {S,N,Ax}
function getindex(inds::StaticCartesianIndices, i::Integer)
    Base.@_inline_meta
    @boundscheck checkbounds(inds, i)
    @inbounds unsafe_getindex(inds, i)
end

#=
function unsafe_getindex(ci::CartesianSIndices{T,N,I,L}, i::BaseInteger)::T where {T,N,I,L}
    out = []
    ind = i - 1
    for ax in axes(ci)
        indnext = div(ind, length(ax))
        push!(out, first(ax) + (ind - length(ax) * indnext + 1 - firstindex(ax)) * step(ax))
        ind = indnext
    end
    return Tuple(out)
end



@generated function unsafe_getindex(ci::CartesianSIndices{T,N,I,L}, i::BaseInteger) where {T,N,I,L}
    out = Expr[]
    ind = :(i - 1)
    for D in 1:N
        ax = :(axes($ci, $D))
        indnext = :(div($ind, _length($ax)))
        if D == 1
            push!(out, :(first($ax) + ($ind - _length($ax) * $indnext + 1 - firstindex($ax)) * step($ax)))
        else
            push!(out, :(first($ax) + ($ind - _length($ax) *  $indnext + 1 - firstindex($ax)) * step($ax)))
        end
        ind = indnext
    end
    return quote
        ($(out...),)
    end
end

_length(::UnitSRange{T,B,E,L}) where {T,B,E,L} = values(L)

=#
@generated function unsafe_getindex(inds::CartesianSIndices{T,N,I,L}, i::SInteger) where {T,N,I,L}
    out = []
    ind = i - SOne
    for D in 1:N
        ax = axes(inds, D)
        indnext = div(ind, length(ax))
        if D == 1
            push!(out, first(ax) + (ind - length(ax) * indnext + SOne - firstindex(ax)) * step(ax))
        else
            push!(out, first(ax) + (ind - length(ax) *  indnext + SOne - firstindex(ax)) * step(ax))
        end
        ind = indnext
    end
    return quote
        ($(out...),)
    end
end

#@generated function unsafe_getindex(inds::CartesianSIndices{T,N,I}, i::Integer) where {T,N,I}
#    :(_ind2subs($(axes(inds)), i - 1))
#end


@inline function unsafe_getindex(inds::StaticCartesianIndices{T,N,I}, i::Integer) where {T,N,I}
    Base.@_inline_meta
    _ind2sub(axes(inds), i)::T
end


_ind2sub(::Tuple{}, ind::Integer) = (Base.@_inline_meta; ind == 1 ? () : throw(BoundsError()))
_ind2sub(dims::Base.DimsInteger, ind::Integer) = (Base.@_inline_meta; _ind2sub_recurse(dims, ind-1))
_ind2sub(inds::Tuple{Vararg{<:AbstractRange,N}}, ind::Integer) where N =
    (Base.@_inline_meta; _ind2sub_recurse(inds, ind-1))
_ind2sub(inds::Tuple{AbstractArray}, ind::Integer) =
    throw(ArgumentError("Linear indexing is not defined for one-dimensional arrays"))
#_ind2sub(inds::Tuple{OneTo}, ind::Integer) = (ind,)

_ind2sub_recurse(::Tuple{}, ind) = (ind+1,)
function _ind2sub_recurse(indslast::NTuple{1}, ind)
    Base.@_inline_meta
    (_lookup(ind, indslast[1]),)
end
function _ind2sub_recurse(inds, ind)
    Base.@_inline_meta
    r1 = inds[1]
    indnext, f, l = _div(ind, r1)
    (ind-l*indnext+f, _ind2sub_recurse(Base.tail(inds), indnext)...)
end

_lookup(ind, d::Integer) = ind+1
_lookup(ind, r::AbstractUnitRange) = ind+first(r)
_lookup(ind, r::StaticUnitRange) = ind+first(r)
_div(ind, d::Integer) = div(ind, d), 1, d
_div(ind, r::AbstractUnitRange) = (d = length(r); (div(ind, d), first(r), d))
_div(ind, r::StaticUnitRange) = (d = length(r); (div(ind, d), first(r), d))

# TODO
function getindex(inds::StaticLinearIndices{T,N}, i::Vararg{T,N}) where {T,N}
    Base.@_inline_meta
    @boundscheck checkbounds(inds, i...)
    @inbounds unsafe_getindex(inds, i)
end

function unsafe_getindex(li::LinearMIndices{T,N,I}, i::NTuple{N}) where {T,N,I}
    sz = 1
    out = zero(T)
    for D in 1:N
        ax = axes(li, D)
        if D == 1
            out = _inds2idx(ax, i[D])
        else
            out = out + _inds2idx(ax, sz, i[D])
        end
        sz *= size(li, D)
    end
    return out
end

_inds2idx(ax::UnitSRange{T,B}, i::Integer) where {T,B} = i
_inds2idx(ax::UnitSRange{T,B}, sz::Int, i::Integer) where {T,B} = sz * (values(B)::T + i - 2)

_inds2idx(ax::AbstractRange, i::Integer) = first(ax) + i - first(ax) * step(ax)
_inds2idx(ax::AbstractRange, sz::Int, i::Integer) = sz * (first(ax) + (i -firstindex(ax)) * step(ax) - 1)

@generated function unsafe_getindex(li::LinearSIndices{T,N,I,L}, i::NTuple{N}) where {T,N,I,L}
    sz = 1
    out = zero(T)
    for D in 1:N
        ax = :(axes(li, $D))
        if D == 1
            out = :(first($ax) + i[$D] - first($ax) * step($ax))
        else
            out = :($out + $sz * (first($ax) + (i[$D] - firstindex($ax)) * step($ax) - SOne))
        end
        sz *= size(li, D)
    end
    return quote
        $out
    end
end

# T<:Tuple for cartesian inds so easiest to index in that context
function getindex(inds::StaticCartesianIndices{T,N,I,L}, i...) where {T,N,I,L}
    Base.@_inline_meta
    @boundscheck checkbounds(inds, i...)
    @inbounds unsafe_getindex(inds, i)
end

function unsafe_getindex(inds::CartesianMIndices{T,N,I}, i::Tuple) where {T,N,I}
    Base.@_inline_meta
    map(getindex, axes(inds), i)::T
end

@inline function unsafe_getindex(inds::CartesianSIndices{T,N,I}, i::Tuple)::T where {T,N,I}
    map(getindex, axes(inds), i)::T
end
