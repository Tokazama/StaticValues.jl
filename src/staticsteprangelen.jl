"""
    StaticStepRangeLen{T,B,S,E,L,F}
"""
abstract type StaticStepRangeLen{T,B,S,E,L,F} <: StaticOrdinalRange{T,B,S,E,L} end

@pure offset(::StaticStepRangeLen{T,B,S,E,L,F}) where {T,B,S,E,L,F<:SInteger} = F()
@pure offset(::Type{<:StaticStepRangeLen{T,B,S,E,L,F}}) where {T,B,S,E,L,F<:SInteger} = F()

@pure getstarthi(r::StaticStepRangeLen{T,TPVal{Hb,Lb}}) where {T,Hb,Lb} = Hb()::Hb
@pure getstartlo(r::StaticStepRangeLen{T,TPVal{Hb,Lb}}) where {T,Hb,Lb} = Lb()::Lb

@pure getstephi(r::StaticStepRangeLen{T,B,TPVal{Hs,Ls}}) where {T,B,Hs,Ls} = Hs()::Hs
@pure getsteplo(r::StaticStepRangeLen{T,B,TPVal{Hs,Ls}}) where {T,B,Hs,Ls} = Ls()::Ls

@inline offset(r::StaticStepRangeLen{T,B,S,E,L,F}) where {T,B,S,E,L,F<:BaseInteger} = getfield(r, :offset)::F
@inline offset!(r::StaticStepRangeLen{T,B,S,E,L,F}, f::F) where {T,B,S,E,L,F<:BaseInteger} =
    setfield!(r, :offset, f)

"StepSRangeLen - Static parametric type variant of StaticStepRangeLen"
struct StepSRangeLen{T,B,S,E,L,F} <: StaticStepRangeLen{T,B,S,E,L,F} end

"StepMRangeLen - Mutable variant of StaticStepRangeLen"
mutable struct StepMRangeLen{T,B,S,L,F} <: StaticStepRangeLen{T,B,S,Dynamic,L,F}
    start::B       # reference value (might be smallest-magnitude value in the range)
    step::S      # step value
    len::L     # length of the range
    offset::F  # the index of ref
end

@inline getstarthi(r::StepMRangeLen{T,TwicePrecision{Tb}}) where {T,Tb} = r.start.hi::Tb
@inline getstartlo(r::StepMRangeLen{T,TwicePrecision{Tb}}) where {T,Tb} = r.start.lo::Tb

@inline getstephi(r::StepMRangeLen{T,B,TwicePrecision{Ts}}) where {T,B,Ts} = r.step.hi::Ts
@inline getsteplo(r::StepMRangeLen{T,B,TwicePrecision{Ts}}) where {T,B,Ts} = r.step.lo::Ts

@inline length!(r::StepMRangeLen{T,B,S,E,L}, len::L) where {T,B,S,E,L<:BaseInteger} =
    setfield!(r, :len, len)
length(r::StepMRangeLen{T,B,S,L,F}) where {T,B,S,L,F} = r.len::L



function StaticStepRangeLen{T,B,S}(
    start::B, step::S, len::L, offset::F) where {T,B<:TPVal,S<:TPVal,L<:SVal,F<:SVal}
    check_params(StaticStepRangeLen, start, step, len, offset)

    u = len - offset
    shift_hi, shift_lo = u*gethi(step), u * getlo(step)
    x_hi, x_lo = Base.add12(gethi(start), shift_hi)

    StepSRangeLen{T,B,S}(ofeltype(T, x_hi + (x_lo + (shift_lo + getlo(start)))), len, offset)
end

function StaticStepRangeLen{T,B,S}(
    start::B, step::S, len::L, offset::F) where {T,B<:SVal,S<:SVal,L<:SVal,F<:SVal}
    check_params(StaticStepRangeLen, start, step, len, offset)
    StepSRangeLen{T,B,S}(start + (len-offset) * step, len, offset)
end

StepSRangeLen{T,B,S}(stop::E, len::L, offset::F) where {T,B,S,E,L,F} =
    StepSRangeLen{T,B,S,E,L,F}()

# fallback to mutable
StaticStepRangeLen{T,B,S}(start::B, step::S, len::L, offset::F) where {T,B,S,L,F} =
    StepMRangeLen{T,B,S,L,F}(start, step, len, offset)

StaticStepRangeLen(ref::R, step::S, len::Integer, offset::Integer = SOne) where {R,S} =
    StaticStepRangeLen{eltype(ref+0*step),R,S}(ref, step, len, offset)

StaticStepRangeLen{T}(ref::R, step::S, len::Integer, offset::Integer = SOne) where {T,R,S} =
    StaticStepRangeLen{T,R,S}(ref, step, len, offset)

function check_params(::Type{StaticStepRangeLen}, start::B, step::S, len::L, offset::F) where {B,S,L,F}
    len >= 0 || throw(ArgumentError("StaticStepRangeLen: length cannot be negative, got $len"))
    1 <= offset <= max(1,len) || throw(ArgumentError("StaticStepRangeLen: offset must be in [1,$len], got $offset"))
    return nothing
end

SITuple = Tuple{<:Integer,<:Integer}

srangehp(::Type{Float64}, b::SITuple, s::SITuple, nb::Integer, l::Integer, f::Integer) =
    StaticStepRangeLen(tpval(Float64, b), tpval(Float64, s, nb), l, f)

srangehp(::Type{T}, b::SITuple, s::SITuple, nb::Integer, l::Integer, f::Integer) where T =
    StaticStepRangeLen{T}(b[1]/b[2], s[1]/s[2], int(l), f)

srangehp(::Type{Float64}, b::F_or_FF, s::F_or_FF, nb::Integer, l::Integer, f::Integer) =
    StaticStepRangeLen(tp64(b), Base.twiceprecision(tp64(s), nb), int(l), f)

srangehp(::Type{T}, b::F_or_FF, s::F_or_FF,
         nb::Integer, l::Integer, f::Integer) where {T<:Union{Float16,Float32}} =
    StaticStepRangeLen{T}(f64(b), f64(s), Int(l), f)

showrange(io::IO, r::StaticStepRangeLen) = print(io, "$(values(first(r))):$(values(step(r))):$(values(last(r))) \t (static)")

