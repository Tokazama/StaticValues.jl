# change the stored lendiv to len b/c it's more intuitive to work around length than the
# inferred divisor
"""
    StaticLinRange{T,B,E,L,D}
"""
abstract type StaticLinRange{T,B,S,E,L,D} <: StaticOrdinalRange{T,B,S,E,L} end

@pure lendiv(::StaticLinRange{T,B,E,L,D}) where {T,B,E,L,D<:SReal} = D()
@pure lendiv(::Type{<:StaticLinRange{T,B,E,L,D}}) where {T,B,E,L,D<:SReal} = D()

# TODO: changed mind, length! should change compute and change lendiv
"""
    LinSRange

Static parametric type variant of StaticLinRange
"""
struct LinSRange{T,B,S,E,L,D} <: StaticLinRange{T,B,S,E,L,D}
    LinSRange{T}(start::B, step::S, stop::E, len::L, lendiv::D) where {T,B,S,E,L,D} = new{T,B,S,E,L,D}()
end


LinSRange{T}(start::B, stop::E, len::L) where {T,B,E,L} =
    LinSRange{T}(start, stop, len, max(len-SOne, SOne))
LinSRange{T}(start::B, stop::E, len::L, lendiv::D) where {T,B,E,L,D} =
    LinSRange{T}(start, (stop-start)/lendiv, stop, len, lendiv)

"""
    LinMRange

Mutable variant of StaticLinRange. Notable difference from `LinRange` is the absence of a
`lendiv` field. This allows the length to be changed dynamically with `length!(range, length)`.
"""
mutable struct LinMRange{T,B,E,L} <: StaticLinRange{T,B,Dynamic,E,L,Dynamic}
    start::B
    stop::E
    len::L
end

first(r::LinMRange{T,B,E}) where {T,B<:BaseReal,E} = getfield(r, :start)

step(r::LinMRange) = (last(r)-first(r))/lendiv(r)

last(r::LinMRange{T,B,E}) where {T,B,E<:BaseReal} = getfield(r, :stop)

length(r::LinMRange{T,B,E,<:BaseInteger}) where {T,B,E} = getfield(r, :len)

length!(r::LinMRange{T,B,E,L}, len::L) where {T,B,E,L<:BaseNumber} = setfield!(r, :len, len)

lendiv(r::LinMRange) = max(length(r)-SOne, SOne)


#LinMRange{T}(start::B, stop::E, len::L, lendiv::D) where {T,B,E,L,D} =
#    LinMRange{T,B,E,L}(start, stop, len, lendiv)


function LinMRange{T}(start::B, stop::E, len::L) where {T,B,E,L}
    len >= 0 || throw(ArgumentError("srange($start, stop=$stop, length=$len): negative length"))
    if len == 0
        start == stop || throw(ArgumentError("srange($start, stop=$stop, length=$len): endpoints differ"))
        return LinMRange{T,B,E,L}(start, stop, len)
    end
    return LinMRange{T,B,E,L}(start, stop, len)
end

function StaticLinRange(start, stop, len::Integer)
    StaticLinRange{eltype((stop-start)/len)}(start, stop, len)
end

for (S,B) in S2B
    @eval begin
        function StaticLinRange{T}(start::$S, stop::$S, len::SInteger) where T
            len >= 0 || throw(ArgumentError("srange($start, stop=$stop, length=$len): negative length"))
            LinSRange{T}(start, stop, len, max(len-SOne, SOne))
        end

        function StaticLinRange{T}(start::$S, stop::$S, len::SIntegerZeroType) where T
            LinSRange{T}(start, stop, len, SOne, SOne)
        end

        function StaticLinRange{T}(start::$S{X}, stop::$S{X}, len::SIntegerZeroType) where {T,X}
            throw(ArgumentError("range($start, stop=$stop, length=$len): endpoints differ"))
        end

        function StaticLinRange{T}(start::$B, stop::$B, len::BaseInteger) where {T}
            LinMRange{T}(start, stop, len)
        end

        function StaticLinRange{T}(start::$S, stop::$S, len::BaseInteger) where {T}
            LinMRange{T}(start, stop, len)
        end

        function StaticLinRange{T}(start::$B, stop::$S, len::BaseInteger) where {T}
            LinMRange{T}(start, stop, len)
        end

        function StaticLinRange{T}(start::$S, stop::$B, len::BaseInteger) where {T}
            LinMRange{T}(start, stop, len)
        end

        function StaticLinRange{T}(start::$B, stop::$S, len::SInteger) where {T}
            LinMRange{T}(start, stop, len)
        end

        function StaticLinRange{T}(start::$S, stop::$B, len::SInteger) where {T}
            LinMRange{T}(start, stop, len)
        end

    end
end

