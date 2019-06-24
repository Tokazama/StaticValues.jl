"""
    StaticStepRange
"""
abstract type StaticStepRange{T,B,S,E,L} <: StaticOrdinalRange{T,B,S,E,L} end


"StepSRange - Static parametric type variant of StaticStepRange"
struct StepSRange{T,B,S,E,L} <: StaticStepRange{T,B,S,E,L} end

StepSRange(start::B, step::S, stop::E) where {B,S,E} = StepSRange{eltype(start)}(start, step, stop)

StepSRange{T}(start::B, step::S, stop::E) where {T,B,S,E} = StepSRange{T,B,S}(start, step, _steprange_last(start, step, stop))

StepSRange{T,B,S}(start::B, step::S, stop::E) where {B,E,S,T} = StepSRange{T,B,S,E}(start, step, stop, steprange_len(start, step, stop))

StepSRange{T,B,S,E}(start::B, step::S, stop::E, len::L) where {T,B,S,E,L} = StepSRange{T,B,S,E,L}()


"StepMRange - Mutable variant of StaticStepRange"
mutable struct StepMRange{T,B,S,E} <: StaticStepRange{T,B,S,E,Dynamic}
    start::B
    step::S
    stop::E
end

first!(r::StepMRange{T,B,S,E}, start::B) where {T,B<:BaseNumber,S,E} = setfield!(r, :start, start)

length(r::StepMRange) = steprange_len(first(r), step(r), last(r))

step!(r::StepMRange{T,B,S,E}, step::B) where {T,B,S<:BaseNumber,E} = setfield!(r, :step, step)

last!(r::StepMRange{T,B,S,E}, stop::B) where {T,B,S,E<:BaseNumber} = setfield!(r, :stop, stop)

StepMRange(start::B, step::S, stop::E) where {B,S,E} = StepMRange{eltype(start)}(start, step, stop)

StepMRange{T}(start::B, step::S, stop::E) where {T,B,S,E} = StepMRange{T,B,S}(start, step, _steprange_last(start, step, stop))

StepMRange{T,B,S}(start::B, step::S, stop::E) where {T,B,S,E} = StepMRange{T,B,S,E}(start, step, stop)

isstatic(::StepSRange) = true
isstatic(::Type{<:StepSRange}) = true

isstatic(::StepMRange) = false
isstatic(::Type{<:StepMRange}) = false

#=
function _steprange_last(start::B, step::S, stop::E) where {B<:Real,S<:Real,E<:Real}
    z = zero(step)
    step == z && throw(ArgumentError("step cannot be zero"))

    if (step > z) != (stop > start)
        if start isa Integer
            if step > 0
                return start - oneunit(stop - start)
            else
                return start + oneunit(stop - start)
            end
        else
            return start - step
        end
    else
        if stop > start
            if start isa Signed && (stop - start) < 0
                return stop - ofeltype(start, unsigned(stop - start) % step)
            else
                return stop - ((stop - start) % step)
            end
        else
            if start isa Signed && (start - stop) < 0
                return stop + oftype(start, unsigned(start - stop) % -step)
            else
                return stop + ((start - stop) % -step)
            end
        end
    end
end
=#
# to make StepRange constructor inlineable, so optimizer can see `step` value
function _steprange_last(start::B, step::S, stop::E) where {B,S,E}
    if isa(start,AbstractFloat) || isa(step,AbstractFloat)
        throw(ArgumentError("StepRange should not be used with floating point"))
    end
    z = zero(step)
    step == z && throw(ArgumentError("step cannot be zero"))

    if stop == start
        last = stop
    else
        if (step > z) != (stop > start)
            last = steprange_last_empty(start, step, stop)
        else
            # Compute absolute value of difference between `start` and `stop`
            # (to simplify handling both signed and unsigned T and checking for signed overflow):
            absdiff, absstep = stop > start ? (stop - start, step) : (start - stop, -step)

            # Compute remainder as a nonnegative number:
            if B <: Signed && absdiff < zero(absdiff)
                # handle signed overflow with unsigned rem
                remain = ofeltype(B, unsigned(absdiff) % absstep)
            else
                remain = absdiff % absstep
            end
            # Move `stop` closer to `start` if there is a remainder:
            last = stop > start ? stop - remain : stop + remain
        end
    end
    last
end

function steprange_last_empty(start::Integer, step, stop)
    # empty range has a special representation where stop = start-1
    # this is needed to avoid the wrap-around that can happen computing
    # start - step, which leads to a range that looks very large instead
    # of empty.
    if step > zero(step)
        last = start - oneunit(stop-start)
    else
        last = start + oneunit(stop-start)
    end
    last
end
# For types where x+oneunit(x) may not be well-defined
steprange_last_empty(start, step, stop) = start - step

function steprange_len(start, step, stop)
    n = Integer(div((stop - start) + step, step))
    isempty(r) ? zero(n) : n
end

function steprange_len(start::B, step::S, stop::E) where {B<:Integer,S,E}
    if (start != stop) & ((step > zero(step)) != (stop > start))
        zero(B)
    elseif step > 1
        ofeltype(B, div(unsigned(stop - start), step)) + one(B)
    elseif step < -1
        ofeltype(B, div(unsigned(start - stop), -step)) + one(B)
    elseif step > 0
        div(stop - start, step) + one(B)
    else
        div(start - stop, -step) + one(B)
    end
end

Base.StepRange(r::StaticUnitRange) = UnitRange(values(first(r)), values(last(r)))
#UnitSRange(r::UnitRange{T}) where T = UnitSRange{T}(SVal(first(r)), SVal(last(r)))
#UnitMRange(r::UnitRange{T}) where T = UnitSRange{T}(first(r), last(r))


