"""
    StaticStartRange{T,B,E,L}

StaticRange subtype with parametric parameters for the first and last parts of a range.
"""
abstract type StaticStartRange{T,B,E,L} <: StaticRange{T,L} end

@pure first(r::StaticStartRange{T,B}) where {T,B<:SVal} = B()
@pure first(::Type{<:StaticStartRange{T,B}}) where {T,B<:SVal} = B()

@pure last(::StaticStartRange{T,B,E}) where {T,B,E<:SVal} = E()
@pure last(::Type{<:StaticStartRange{T,B,E}}) where {T,B,E<:SVal} = E()

@inline first(r::StaticStartRange{T,B}) where {T,B<:BaseNumber} = getfield(r, :start)::B
@inline first!(r::StaticStartRange{T,B}, start::B) where {T,B<:BaseNumber} = setfield!(r, :start, start)

@inline last(r::StaticStartRange{T,B,E}) where {T,B,E<:BaseNumber} = getfield(r, :stop)::E
@inline last!(r::StaticStartRange{T,B,E}, stop::E) where {T,B,E<:BaseNumber} = setfield!(r, :stop, stop)


