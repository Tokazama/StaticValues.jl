"""
    StaticUnitRange{T,B,E,L}

Supertype for static ranges with a step size of oneunit(T) with elements of type T.
UnitRange and other types are subtypes of this

# Examples
```jldoctest
```
"""
abstract type StaticUnitRange{T,B,E,L} <: StaticStartRange{T,B,E,L} end

# TODO: these should be static
@inline step(::StaticUnitRange{T}) where T = one(T)::T
@inline step(::Type{<:StaticUnitRange{T}}) where T = one(T)::T


# if length is dynamic 
length(r::StaticUnitRange{T,B,E,Dynamic}) where {T,B,E,Dynamic} = Integer(last(r) - first(r) + step(r))
function length(r::StaticUnitRange{T,B,E,Dynamic}) where {T<:Union{Int,Int64,Int128},B,E}
    Base.@_inline_meta
    (last(r) - first(r)) + one(T)
end

length(r::StaticUnitRange{T,B,E,Dynamic}) where {T<:Union{UInt,UInt64,UInt128},B,E} =
    last(r) < first(r) ? zero(T) : (last(r) - first(r) + one(T))

"UniteSRange - Static parametric type  variant of StaticUnitRange"
struct UnitSRange{T,B,E,L} <: StaticUnitRange{T,B,E,L} end

"UniteMRange - Mutable variant of StaticUnitRange"
mutable struct UnitMRange{T,B,E} <: StaticUnitRange{T,B,E,Dynamic}
    start::B
    stop::E
end

UnitMRange{Bool}(start::Bool, stop::SBool{E}) where E =
   UnitMRange{Bool,Bool,SBool{E}}(start, stop)

UnitMRange{T}(start::B, stop::T) where {T<:BaseInteger,B<:SInteger} =
   UnitMRange{T,B,T}(start, ifelse(stop >= start, stop, ofeltype(T, start - oneunit(stop - start))))

UnitMRange{T}(start::T, stop::E) where {T<:BaseInteger,E<:SInteger} =
   UnitMRange{T,T,E}(start, ifelse(stop >= start, stop, ofeltype(T,start-oneunit(stop - start))))

UnitMRange{T}(start::T, stop::T) where T =
    UnitMRange{T,T,T}(start, ifelse(stop >= start, stop, ofeltype(T,start-oneunit(stop - start))))

function UnitMRange(start::B, stop::E) where {B,E}
    T = Base.promote_eltype(B, E)
    UnitMRange{T}(ofeltype(T, start), ofeltype(T, stop))
end

function UnitMRange{T}(start::B, stop::T) where {T,B<:SReal}
    if stop >= start
        UnitMRange{T,B,T}(start, ofeltype(T, start + floor(stop - start)))
    else
        UnitMRange{T,B,T}(start, ofeltype(T, oneunit(stop - start)))
    end
end

function UnitMRange{T}(start::T, stop::SReal{E}) where {T,E}
    if stop >= start
        last = ofeltype(T, start + floor(E::T - start))
    else
        last = ofeltype(T, start - floor(E::T - start))
    end
    UnitMRange{T,T,typeof(last)}(start, last)
end

UnitSRange{Bool}(start::SBool{B}, stop::SBool{E}) where {B,E} =
    UnitSRange{Bool,SBool{B},SBool{E}}(start, stop)

function UnitSRange{T}(start::B, stop::E) where {B<:SReal,E<:SReal,T<:BaseInteger}
    if stop >= start
        return UnitSRange{T,B}(start, stop)
    else
        return UnitSRange{T,B}(start, ofeltype(T, start-one(start)))
    end
end

function UnitSRange{T}(start::B, stop::E) where {B<:SReal,E<:SReal,T}
    if stop >= start
        UnitSRange{T,B}(start, ofeltype(T, start+floor(stop-start)))
    else
        UnitSRange{T,B}(start, ofeltype(T, start-one(start - stop)))
    end
end

UnitSRange{T,B}(start::B, stop::E) where {T<:Union{Int,Int64,Int128},B,E} =
    UnitSRange{T,B,E}(start, stop, ofeltype(Int, (stop - start) + one(B)))

UnitSRange{T,B}(start::B, stop::E) where {T<:Union{UInt,UInt64,UInt128},B,E} =
    UnitSRange{T,B,E}(start, stop, stop < stop ? zero(B) : (stop - start) + one(start))

UnitSRange{T,B}(start::B, stop::E) where {T<:Real,B,E} =
    UnitSRange{T,B,E}(start, stop, integer(start - stop + oneunit(B)))

UnitSRange{T,B,E}(start::B, stop::E, len::L) where {T,B,E,L} = UnitSRange{T,B,E,L}()

isstatic(::UnitSRange) = true
isstatic(::Type{<:UnitSRange}) = true
isstatic(::UnitMRange) = true
isstatic(::Type{<:UnitMRange}) = false

#promote_rule(a::Type{UnitRange{T1}}, b::Type{UnitRange{T2}}) where {T1,T2} = el_same(promote_type(T1,T2), a, b)
Base.UnitRange(r::StaticUnitRange) = UnitRange(values(first(r)), values(last(r)))
Base.UnitRange{T}(r::StaticUnitRange) where {T<:Real} = UnitRange{T}(values(first(r)), values(last(r)))

UnitSRange(r::Union{AbstractUnitRange,UnitMRange}) = UnitSRange(tostatic(first(r)), tostatic(last(r)))
UnitMRange(r::Union{AbstractUnitRange,UnitSRange}) = UnitMRange(first(r), last(r))
UnitMRange(r::UnitMRange) = r
UnitSRange(r::UnitSRange) = r

UnitSRange{T}(r::Union{AbstractUnitRange,UnitMRange}) where T = UnitSRange(tostatic(first(r)), tostatic(last(r)))
UnitMRange{T}(r::Union{AbstractUnitRange,UnitSRange}) where T = UnitMRange(first(r), last(r))
UnitMRange{T}(r::UnitMRange{T}) where T = r
UnitSRange{T}(r::UnitSRange{T}) where T = r
UnitMRange{T}(r::UnitMRange) where T = UnitSRange(tostatic(first(r)), tostatic(last(r)))
UnitSRange{T}(r::UnitSRange) where T = UnitMRange(first(r), last(r))

#promote_rule(a::Type{UnitRange{T1}}, ::Type{UR}) where {T1,UR<:AbstractUnitRange} =
#    promote_rule(a, UnitRange{eltype(UR)})

StaticUnitRange{T}(start::SVal, stop::SVal) where T = UnitSRange{T}(start, stop)
StaticUnitRange{T}(start::SVal, stop::Any) where T = UnitMRange{T}(start, stop)
StaticUnitRange{T}(start::Any, stop::SVal) where T = UnitMRange{T}(start, stop)

const OverflowSafe = Union{Bool,Int8,Int16,Int32,Int64,Int128,
                           UInt8,UInt16,UInt32,UInt64,UInt128}

_in_unit_range(v::StaticUnitRange, val, i::Integer) =
    i > 0 && val <= last(v) && val >= first(v)

function getindex(v::UnitMRange{T}, i::Integer) where {T<:OverflowSafe}
    Base.@_inline_meta
    val = first(v) + (i - one(i))
    @boundscheck _in_unit_range(v, val, i) || Base.throw_boundserror(v, i)
    val % T
end

function getindex(r::UnitSRange{T,B,E,L}, i::Integer) where {T,B,E,L}
    Base.@_inline_meta
    @boundscheck if i < 1 || i > values(L)
        throw(BoundsError(r, i))
    end
    unsafe_getindex(r, T(i))
end

@pure unsafe_getindex(::UnitSRange{T,B,E,L}, i::T) where {T,B,E,L} = values(B) + i - one(T)


function getindex(r::StaticUnitRange, s::Union{AbstractUnitRange{<:Integer},StaticUnitRange{<:Integer}})
    Base.@_inline_meta
    @boundscheck checkbounds(r, s)
    f = first(r)
    st = ofeltype(f, f + first(s)-SOne)
    if st isa SReal
        return range(st, length=SInteger(length(s)))
    else
        return range(st, length=length(s))
    end
end

@pure Base.checkindex(::Type{Bool}, r::UnitSRange{T,B,E,L}, i::Real) where {T,B,E,L} =
    SOne <= i && i <= L()::L

@inline Base.checkindex(::Type{Bool}, r::UnitSRange{T,B,E,L}, i::AbstractRange) where {T,B,E,L} =
    checkindex(Bool, r, first(i)) && checkindex(Bool, r, last(i))

function Base.checkindex(::Type{Bool}, inds::StaticUnitRange{T,B,E,L}, i::Real) where {T,B,E,L}
    Base.@_inline_meta
    _checkfirst(first(inds)::B, i) & _checklast(last(inds)::E, i)
end

_checkfirst(b::BaseReal, i::Real) = b <= i
@pure _checkfirst(::B, i::Real) where B<:SReal = values(B) <= i

_checklast(l::BaseReal, i::Real) = i <= l
@pure _checklast(::L, i::Real) where L<:SReal = i <= values(L)

@pure function Base.iterate(r::UnitSRange{T,B,E,L}) where {T,B,E,L}
    if values(L) < 1
        return nothing
    else
        return (values(B)::T, values(B)::T)::Tuple{T,T}
    end
end

@inline function Base.iterate(r::UnitSRange{T,B,E,L}, i::T) where {T,B,E,L}
    if i < values(E)::T
        return (i + one(T), i + one(T))::Tuple{T,T}
    else
        return nothing
    end
end

showrange(io::IO, r::StaticUnitRange) = print(io, "$(first(r)):$(last(r))")
