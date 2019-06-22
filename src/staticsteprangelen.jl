"""
    StaticStepRangeLen{T,B,S,E,L,F}
"""
abstract type StaticStepRangeLen{T,R,S,E,L,F} <: StaticOrdinalRange{T,R,S,E,L} end

@pure offset(::StaticStepRangeLen{T,R,S,E,L,F}) where {T,R,S,E,L,F<:SInteger} = F()
@pure offset(::Type{<:StaticStepRangeLen{T,R,S,E,L,F}}) where {T,R,S,E,L,F<:SInteger} = F()

@pure reference(::StaticStepRangeLen{T,R,S,E,L,F}) where {T,R<:SVal,S,E,L,F} = R()
@pure reference(::Type{<:StaticStepRangeLen{T,R,S,E,L,F}}) where {T,R<:SVal,S,E,L,F} = R()

@pure getrefhi(r::StaticStepRangeLen{T,TPVal{Hb,Lb}}) where {T,Hb,Lb} = Hb()::Hb
@pure getreflo(r::StaticStepRangeLen{T,TPVal{Hb,Lb}}) where {T,Hb,Lb} = Lb()::Lb

@pure getstephi(r::StaticStepRangeLen{T,R,TPVal{Hs,Ls}}) where {T,R,Hs,Ls} = Hs()::Hs
@pure getsteplo(r::StaticStepRangeLen{T,R,TPVal{Hs,Ls}}) where {T,R,Hs,Ls} = Ls()::Ls

@inline first(r::StaticStepRangeLen) = unsafe_getindex(r, SOne)

"StepSRangeLen - Static parametric type variant of StaticStepRangeLen"
struct StepSRangeLen{T,R,S,E,L,F} <: StaticStepRangeLen{T,R,S,E,L,F}
    function StepSRangeLen{T,R,S,E,L,F}() where {T,R,S,E,L,F}
        L() >= 0 || throw(ArgumentError("StepSRangeLen: length cannot be negative, got $(values(L))"))
        1 <= F() <= max(1, L()) || throw(ArgumentError("StaticStepRangeLen: offset must be in [1,$(values(L))], got $offset"))
        new{eltype(T),R,S,E,L,F}()  # ensure that T <: BaseType
    end
end

StepSRangeLen(ref::R, step::S, len::SInteger, offset::SInteger = SOne) where {R,S} =
    StepSRangeLen{eltype(ref+SZero*step)}(ref, step, len, offset)

StepSRangeLen{T}(ref::R, step::S, len::SInteger, offset::SInteger = SOne) where {T,R<:SReal,S<:SReal} =
    StepSRangeLen{T,R,S}(ref, step, ref + (len-offset) * step, len, offset)

function StepSRangeLen{T}(start::TPVal, step::TPVal, len::SInteger, offset::SInteger) where T
    u = len - offset
    shift_hi, shift_lo = u*gethi(step), u * getlo(step)
    x_hi, x_lo = add12(gethi(start), shift_hi)
    StepSRangeLen{T,typeof(start),typeof(step)}(start, step, ofeltype(T, x_hi + (x_lo + (shift_lo + getlo(start)))), len, offset)
end

StepSRangeLen{T,B,S}(start::B, step::S, stop::E, len::SInteger, offset::SInteger) where {T,B,S,E} =
    StepSRangeLen{T,B,S,E,typeof(len),typeof(offset)}()

"StepMRangeLen - Mutable variant of StaticStepRangeLen"
mutable struct StepMRangeLen{T,R,S,L,F} <: StaticStepRangeLen{T,R,S,Dynamic,L,F}
    ref::R       # reference value (might be smallest-magnitude value in the range)
    step::S      # step value
    len::L     # length of the range
    offset::F  # the index of ref

    function StepMRangeLen{T,R,S}(ref::R, step::S, len::Integer, offset::Integer = SOne) where {T,R,S}
        len >= 0 || throw(ArgumentError("length cannot be negative, got $len"))
        1 <= offset <= max(1,len) || throw(ArgumentError("StepMRangeLen: offset must be in [1,$len], got $offset"))
        new{T,R,S,typeof(len),typeof(offset)}(ref, step, len, offset)
    end
end

@inline offset(r::StepMRangeLen{T,R,S,L,F}) where {T,R,S,L,F<:BaseInteger} = getfield(r, :offset)
@inline offset!(r::StepMRangeLen{T,R,S,L,F}, f::F) where {T,R,S,L,F<:BaseInteger} = setfield!(r, :offset, f)

@inline reference(r::StepMRangeLen{T,R,S,L,F}) where {T,R<:BaseAny,S,L,F} = getfield(r, :ref)
@inline reference!(r::StepMRangeLen{T,R,S,L,F}, ref::R) where {T,R<:BaseAny,S,L,F} = setfield!(r, :ref, ref)


StepMRangeLen(ref::R, step::S, len::Integer, offset::Integer = SOne) where {R,S} =
    StepMRangeLen{eltype(ref+0*step)}(ref, step, len, offset)

StepMRangeLen{T}(ref::R, step::S, len::Integer, offset::Integer=SOne) where {T,R,S} =
    StepMRangeLen{T,R,S}(ref, step, len, offset)

@inline getrefhi(r::StepMRangeLen{T,TwicePrecision{Tb}}) where {T,Tb} = r.ref.hi::Tb
@inline getreflo(r::StepMRangeLen{T,TwicePrecision{Tb}}) where {T,Tb} = r.ref.lo::Tb

@inline getstephi(r::StepMRangeLen{T,B,TwicePrecision{Ts}}) where {T,B,Ts} = r.step.hi::Ts
@inline getsteplo(r::StepMRangeLen{T,B,TwicePrecision{Ts}}) where {T,B,Ts} = r.step.lo::Ts

@inline length!(r::StepMRangeLen{T,B,S,L}, len::L) where {T,B,S,L<:BaseInteger} = setfield!(r, :len, len)
@inline length(r::StepMRangeLen{T,B,S,L,F}) where {T,B,S,L,F} = getfield(r, :len)

step(r::StepMRangeLen{T,B,S}) where {T,B,S<:BaseAny} = getfield(r, :step)

StaticStepRangeLen(ref::SVal, step::SVal, len::SInteger, offset::SInteger = SOne) = StepSRangeLen(ref, step, len, offset)
StaticStepRangeLen{T}(ref::SVal, step::SVal, len::SInteger, offset::SInteger = SOne) where T = StepSRangeLen{T}(ref, step, len, offset)

StaticStepRangeLen(ref::R, step::S, len::Integer, offset::Integer = SOne) where {R,S} = StepMRangeLen(ref, step, len, offset)
StaticStepRangeLen{T}(ref::R, step::S, len::Integer, offset::Integer = SOne) where {T,R,S} = StepMRangeLen{T}(ref, step, len, offset)

showrange(io::IO, r::StaticStepRangeLen) = print(io, "$(first(r)):$(step(r)):$(last(r))")
