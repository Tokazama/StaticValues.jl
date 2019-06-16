"""
    StaticLinRange{T,B,E,L,D}
"""
abstract type StaticLinRange{T,B,S,E,L,D} <: StaticOrdinalRange{T,B,S,E,L} end

# FIXME
@inline lendiv(::StaticLinRange{T,B,E,L,D}) where {T,B,E,L,D} = one(T)::T
@inline lendiv(::Type{<:StaticLinRange{T,B,E,L}}) where {T,B,E,L} = one(T)::T

step(r::StaticLinRange{T,B,Dynamic,E,L,D}) where {T,B,E,L,D} = (last(r)-first(r))/lendiv(r)


"LinSRange - Static parametric type variant of StaticLinRange"
struct LinSRange{T,B,S,E,L,D} <: StaticLinRange{T,B,S,E,L,D} end

"LinMRange - Mutable variant of StaticLinRange"
mutable struct LinMRange{T,B,E,D} <: StaticLinRange{T,B,Dynamic,E,Dynamic,D}
    start::B
    stop::E
    lendiv::D
end

first(r::LinMRange{T,B,E,D}) where {T,B,E,D} = r.start::B
last(r::StepMRange{T,B,E,D}) where {T,B,E,D} = r.stop::E
lendiv(r::LinMRange{T,B,E,D}) where {T,B,E,D} = r.step::S

function lendiv!(r::LinMRange{T,B,E,D}, len::D) where {T,B,E,D<:BaseNumber}
    r.lendiv = len
    return r
end

function StaticLinRange(start, stop, len::Integer)
    T = eltype((stop-start)/len)
    StaticLinRange{T}(start, stop, len)
end

function StaticLinRange{T}(start::B, stop::E, len::L) where {T,B,E,L<:Integer}
    len >= 0 || throw(ArgumentError("srange($start, stop=$stop, length=$len): negative length"))
    if len == 0
        start == stop || throw(ArgumentError("srange($start, stop=$stop, length=$len): endpoints differ"))
        return StaticLinRange{T,B}(start, stop, len, SOne, SOne)
    end
    return StaticLinRange{T,B}(start, stop, len, max(len-SOne, SOne))
end

StaticLinRange{T,B}(start::B, stop::E, len::L, lendiv::D) where {T,B,E,D,L} =
    StaticLinRange{T,B}(start, (stop-start)/lendiv, stop, len)


StaticLinRange{T,B}(start::B, step::S, stop::E, len::L, lendiv::D) where {T,B,S<:SVal,E,L,D} =
    LinSRange{T,B,S,E,L,D}()

StaticLinRange{T,B}(start::B, step::S, stop::E, len::L, lendiv::D) where {T,B,S<:BaseNumber,E,L,D} =
    LinMRange{T,B,E,D}(start, stop, lendiv)

## for Float16, Float32, and Float64 we hit twiceprecision.jl to lift to higher precision StepRangeLen
# for all other types we fall back to a plain old LinRange
linspace(::Type{T}, start::Integer, stop::Integer, len::Integer) where T = StaticLinRange{T}(start, stop, len)



