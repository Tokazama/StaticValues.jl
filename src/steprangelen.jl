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

isstatic(::StepSRangeLen) = true
isstatic(::Type{<:StepSRangeLen}) = true

isstatic(::StepMRangeLen) = false
isstatic(::Type{<:StepMRangeLen}) = false

#promote_rule(a::Type{StepRangeLen{T,R,S}}, ::Type{OR}) where {T,R,S,OR<:AbstractRange} =
#    promote_rule(a, StepRangeLen{eltype(OR), eltype(OR), eltype(OR)})
Base.StepRangeLen(r::StaticRange) = StepRangeLen{eltype(r)}(r)
Base.StepRangeLen{T}(r::StaticRange) where {T} =
    StepRangeLen(T(first(r)), T(step(r)), values(length(r)))
Base.StepRangeLen{T,R,S}(r::StaticRange) where {T,R,S} =
    StepRangeLen{T,R,S}(R(first(r)), S(step(r)), values(length(r)))

StepSRangeLen(r::AbstractRange) = StepSRangeLen{eltype(r)}(r)
StepSRangeLen{T}(r::AbstractRange) where {T} =
    StepSRangeLen(SVal(ofeltype(T, first(r))), SVal(ofeltype(T, step(r))), SVal(length(r)))
StepSRangeLen{T,R,S}(r::AbstractRange) where {T,R,S} =
    StepSRangeLen(SVal(R(first(r))), SVal(S(step(r))), SVal(length(r)))

StepMRangeLen(r::AbstractRange) = StepMRangeLen{eltype(r)}(r)
StepMRangeLen{T}(r::AbstractRange) where {T} =
    StepMRangeLen(ofeltype(T, first(r)), ofeltype(T, step(r)), length(r))
StepMRangeLen{T,R,S}(r::AbstractRange) where {T,R,S} =
    StepMRangeLen{T}(R(first(r)), S(step(r)), length(r))

@inline function _getindex_hiprec(r::StaticStepRangeLen{T,R}, i::Integer) where {T,R<:Union{<:TPVal,TwicePrecision}}
    u = i - offset(r)
    shift_hi, shift_lo = u*getstephi(r), u*getsteplo(r)
    x_hi, x_lo = add12(getrefhi(r), shift_hi)
    x_hi, x_lo = add12(x_hi, x_lo + (shift_lo + getreflo(r)))
    tpval(x_hi, x_lo)
end

@inline function _getindex_hiprec(r::StaticStepRangeLen{T,R}, i::Integer) where {T,R} # without rounding by T
    reference(r) + (i - offset(r)) * step(r)
end


