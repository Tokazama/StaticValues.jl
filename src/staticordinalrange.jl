"""
    StaticOrdinalRange

"""
abstract type StaticOrdinalRange{T,B,S,E,L} <: StaticStartRange{T,B,E,L} end

@pure step(::StaticOrdinalRange{T,B,S}) where {T,B,S<:SVal} = S()
@pure step(::Type{<:StaticOrdinalRange{T,B,S}}) where {T,B,S<:SVal} = S()

@pure step(::StaticOrdinalRange{T,B,TPVal{H,L}}) where {T,B,H,L} = H() + L()
@pure step(::Type{<:StaticOrdinalRange{T,B,TPVal{H,L}}}) where {T,B,H,L} = H() + L()


@inline step(r::StaticStartRange{T,B,S}) where {T,B,S<:BaseNumber} = getfield(r, :step)::S
@inline step(r::StaticStartRange{T,B,S}) where {T,B,S<:TwicePrecision} = T(getfield(r, :step))


function length(r::StaticOrdinalRange{T,B,S,E,Dynamic}) where {T<:Union{Int,UInt,Int64,UInt64,Int128,UInt128},B,S,E}
    isempty(r) && return zero(T)
    if step(r) > 1
        return convert(T, div(unsigned(last(r) - first(r)), step(r))) + one(T)
    elseif step(r) < -1
        return convert(T, div(unsigned(first(r) - last(r)), -step(r))) + one(T)
    elseif step(r) > 0
        return div((last(r) - first(r)), step(r)) + one(T)
    else
        return div(first(r) - last(r), -step(r)) + one(T)
    end
end

function length(r::StaticOrdinalRange{T,B,S,E,Dynamic}) where {T,B,S,E}
    n = Integer(div((last(r) - first(r)) + step(r), step(r)))
    isempty(r) ? zero(n) : n
end

showrange(io::IO, r::StaticOrdinalRange) = print(io, "$(first(r)):$(step(r)):$(last(r)) \t (static)")

