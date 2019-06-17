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

function UnitMRange{T}(start::B, stop::T) where {T,B<:SReal}
    if stop >= B::T
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

StaticUnitRange{T}(start::SVal, stop::SVal) where T = UnitSRange{T}(start, stop)
StaticUnitRange{T}(start::SVal, stop::Any) where T = UnitMRange{T}(start, stop)
StaticUnitRange{T}(start::Any, stop::SVal) where T = UnitMRange{T}(start, stop)

showrange(io::IO, r::StaticUnitRange) = print(io, "$(first(r)):$(last(r))")
