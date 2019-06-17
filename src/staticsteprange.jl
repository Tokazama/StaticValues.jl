"""
    StaticStepRange
"""
abstract type StaticStepRange{T,B,S,E,L} <: StaticOrdinalRange{T,B,S,E,L} end


"StepSRange - Static parametric type variant of StaticStepRange"
struct StepSRange{T,B,S,E,L} <: StaticStepRange{T,B,S,E,L} end


"StepMRange - Mutable variant of StaticStepRange"
mutable struct StepMRange{T,B,S,E} <: StaticStepRange{T,B,S,E,Dynamic}
    start::B
    step::S
    stop::E
end

function first!(r::StepMRange{T,B,S,E}, start::B) where {T,B<:BaseNumber,S,E}
    r.start = start
    return r
end

function step!(r::StepMRange{T,B,S,E}, start::S) where {T,B,S<:BaseNumber,E}
    r.step = step
    return r
end

function last!(r::StepMRange{T,B,S,E}, stop::E) where {T,B,S,E<:BaseNumber}
    r.stop = stop
    return r
end

StepSRange(start::B, step::S, stop::E) where {B,S,E} =
    StepSRange{eltype(start)}(start, step, stop)

#StepSRange{T}(start::B, step::S, stop::E) where {T,B,S,E} = StepSRange{T,B}(start, step, stop)


function StaticStepRange{T}(start::B, step::S, stop::E) where {T,B<:Real,S<:Real,E<:Real}
    z = zero(step)
    step == z && throw(ArgumentError("step cannot be zero"))

    if (step > z) != (stop > start)
        if T<:Integer
            if step > 0
                last = start - oneunit(stop - start)
            else
                last = start + oneunit(stop - start)
            end
        else
            last = start - step
        end
    else
        # Compute absolute value of difference between `B` and `E`
        # (to simplify handling both signed and unsigned T and checking for signed overflow):
        absdiff, absstep = stop > start ? (stop - start, step) : (start - stop, -step)

        # Compute remainder as a nonnegative number:
        if T <: Signed && absdiff < 0
            # handle signed overflow with unsigned rem
            remain = oftype(start, unsigned(absdiff) % absstep)
        else
            remain = absdiff % absstep
        end
        # Move `E` closer to `B` if there is a remainder:
        last = stop > start ? stop - remain : stop + remain
    end
    return StaticStepRange{T,B,S}(start, step, last)
end

function StaticStepRange{T,B,S}(start::B, step::S, stop::E) where {B,E,S,T<:Union{Int,UInt,Int64,UInt64,Int128,UInt128}}
    (start != stop) & ((step > 0)) != (stop > start) && return StepSRange{T,B,S,E,typeof(SZero)}()
    if step > 1
        return StaticStepRange{T,B,S,E}(start, step, stop, ofeltype(Int, div(Base.unsigned(stop - start), step)) + one(start))
    elseif step < -1
        return StaticStepRange{T,B,S,E}(start, step, stop, ofeltype(Int, div(Base.unsigned(stop - start), -step)) + one(start))
    elseif step > 0
        return StaticStepRange{T,B,S,E}(start, step, stop, ofeltype(Int, div(stop - start, step) + one(start)))
    else
        return StaticStepRange{T,B,S,E}(start, step, stop, ofeltype(Int, div(start - stop, -step) + one(start)))
    end
end

function StaticStepRange{T,B,S}(start::B, step::S, stop::E) where {B,E,S,T}
    (start != stop) &
    ((step > 0)) !=
    (stop > start) ? StepSRange{T,B,S,E}(start, step, stop, SZero) :
                     StepSRange{T,B,S,E}(start, step, stop, ofeltype(Int, div(stop - start + step, step)))
end

StaticStepRange{T,B,S,E}(start::B, step::S, stop::E, len::L) where {T,B<:SVal,S<:SVal,E<:SVal,L<:SInteger} =
    StepSRange{T,B,S,E,L}()

StaticStepRange{T,B,S,E}(start::B, step::S, stop::E, len::L) where {T,B,S,E,L} =
    StepMRange{T,B,S,E}(start, step, stop)
