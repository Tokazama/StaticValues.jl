maybe_sval(val::Val{V}) where {V} = seek_static_val(eltype(V), val)
maybe_sval(val::SVal) = val
maybe_sval(::Nothing) = nothing
maybe_sval(val) = val

"""
    srange

# Examples
```jldoctest
julia> using StaticRanges

julia> 
```
"""

function srange(start::B; length::L=nothing, stop::E=nothing, step::S=nothing) where {B,S,E,L}
    _sr(maybe_sval(start),
        maybe_sval(step),
        maybe_sval(stop),
        maybe_sval(length))
end

function srange(start::B, stop::E; length::L=nothing, step::S=nothing) where {B,S,E,L}
    _sr(maybe_sval(start),
        maybe_sval(step),
        maybe_sval(stop),
        maybe_sval(length))
end

#srange(r::AbstractRange{T}) where T = srange(SVal(first(r)), stop=SVal(last(r)), length=SVal(length(r)))

#srange(r::StepRange{T}) where T = StepSRange(SVal(first(r)), SVal(step(r)), SVal(last(r)))
#srange(r::StepRangeLen{T,R,S}) where {T,R<:Real,S<:Real} = StepSRangeLen{T}(SVal(first(r)), SVal(step(r)), SVal(length(r)), SVal(r.offset))
#srange(r::StepRangeLen{T,R,S}) where {T,R<:TwicePrecision,S<:TwicePrecision} = StepSRangeLen{T}(HPSVal(r.ref), HPSVal(r.step), SVal(r.len), SVal(r.offset))
#srange(r::AbstractUnitRange{T}) where T = UnitSRange{T}(SVal(first(r)), SVal(last(r)))

# Range from start to stop: range(a, [step=s,] stop=b), no length
_sr(start, step,      stop, ::Nothing) = (:)(start, step, stop)
_sr(start, ::Nothing, stop, ::Nothing) = (:)(start, stop)

_sr(b::Real,          s::Nothing,       ::Nothing,  l::Integer) = StaticUnitRange{eltype(b)}(b, b + l - SOne)
_sr(b::AbstractFloat, s::Nothing,       ::Nothing,  l::Integer) = _sr(b, oftype(b, 1),   e, l)
_sr(b::AbstractFloat, s::AbstractFloat, ::Nothing,  l::Integer) = _sr(promote(b, s)..., e, l)
_sr(b::Real,          s::AbstractFloat, ::Nothing,  l::Integer) = _sr(float(b), s, e, l)
_sr(b::AbstractFloat, s::Real,          ::Nothing,  l::Integer) = _sr(b, float(s), e, l)
_sr(b::B,             s::Nothing,       ::Nothing,  l::Integer) where B = _sr(b, one(b-b), e, l)
_sr(b::B,             s,                ::Nothing,  l::Integer) where B =
    _srangestyle(Base.OrderStyle(eltype(B)), Base.ArithmeticStyle(eltype(B)), b, s, l)


_sr(b::AbstractFloat16, s::AbstractFloat16, ::Nothing, l::Integer) = __sr(eltype(b), b, s, nothing, l)
_sr(b::AbstractFloat32, s::AbstractFloat32, ::Nothing, l::Integer) = __sr(eltype(b), b, s, nothing, l)
_sr(b::AbstractFloat64, s::AbstractFloat64, ::Nothing, l::Integer) = __sr(eltype(b), b, s, nothing, l)

_srangestyle(::Base.Ordered, ::Base.ArithmeticWraps,  start, step, len::Integer) =
    StaticStepRange(start, step, oftype(start, start + step * (len - one(len))))
_srangestyle(::Any,          ::Any, start, step, len::Integer) =
    steprangelen(typeof(start+zero(step)*step), start, step, len)

_sr(start::AbstractFloat16, ::Nothing, stop::AbstractFloat16, len::Integer) = __sr(eltype(start), start, nothing, stop, len)
_sr(start::AbstractFloat32, ::Nothing, stop::AbstractFloat32, len::Integer) = __sr(eltype(start), start, nothing, stop, len)
_sr(start::AbstractFloat64, ::Nothing, stop::AbstractFloat64, len::Integer) = __sr(eltype(start), start, nothing, stop, len)

function _sr(start::B, ::Nothing, stop::S, len::Integer) where {B<:Real,S<:Real}
    T = eltype(B)
    if T === eltype(S)
        StaticLinRange{T}(start, stop, len)
    else
        T2 = promote_type(T, eltype(S))
        _sr(to_eltype(T2, start), nothing, to_eltype(T2, stop), len)
    end
end

function _sr(start::B, ::Nothing, stop::S, len::Integer) where {B,S}
    T = eltype(B)
    if T === eltype(S)
        StaticLinRange{T}(start, stop, len)
    else
        T2 = promote_type(T, eltype(S))
        _sr(to_eltype(T2, start), nothing, to_eltype(T2, stop), len)
    end
end


function _sr(start::B, ::Nothing, stop::S, len::Integer) where {B<:Integer,S<:Integer}
    T = eltype(B)
    if T === eltype(S)
        linspace(T, start, stop, len)
    else
        T2 = promote_type(T, eltype(S))
        _sr(to_eltype(T2, start), nothing, to_eltype(T2, stop), len)
    end
end

function __sr(start::AbstractFloat, ::Nothing, stop::AbstractFloat, len::Integer)
    len < 2 && return linspace1(T, start, stop, len)
    if start == stop
        return srangehp(eltype(start), start, zero(start), SZero, len, SOne)
    end
    # Attempt to find exact rational approximations
    start_n, start_d = Base.rat(start)
    stop_n, stop_d = Base.rat(stop)
    if start_d != 0 && stop_d != 0
        den = lcm(start_d, stop_d)
        m = maxintfloat(typeof(start), Int)
        if den != 0 && abs(den*start) <= m && abs(den*stop) <= m
            start_n = round(Int, den*start)
            stop_n = round(Int, den*stop)
            if T(start_n/den) == start && T(stop_n/den) == stop
                return linspace(eltype(start), start_n, stop_n, len, den)
            end
        end
    end
    linspace(start, stop, len)
end

function __sr(a::AbstractFloat, st::AbstractFloat, ::Nothing, len::Integer)
    start_n, start_d = Base.rat(a)
    step_n, step_d = Base.rat(st)
    if start_d != 0 && step_d != 0 &&
       ofeltype(a, start_n/start_d) == a &&
       ofeltype(a, step_n/step_d) == st

        den = lcm(start_d, step_d)
        m = maxintfloat(eltype(a), Int)
        if abs(den*a) <= m && abs(den*st) <= m &&
                rem(den, start_d) == 0 && rem(den, step_d) == 0
            start_n = round(Int, den*a)
            step_n = round(Int, den*st)
            return sfloatrange(eltype(a), start_n, step_n, len, den)
        end
    end
    srangehp(eltype(a), a, st, SZero, len, SOne)
end

function sfloatrange(::Type{T}, b::Integer, s::Integer, l::Real, d::Integer) where T
    if l < 2 || s == 0
        return srangehp(T, (b, d), (s, d), SZero, SOne, l)
    end
    # index of smallest-magnitude value
    imin = clamp(round(Int, -b/s+SOne), SOne, ofeltype(Int, l))
    # Compute smallest-magnitude element to 2x precision
    ref_n = b+(imin-SOne)*s  # this shouldn't overflow, so don't check
    nb = Base.nbitslen(T, l, imin)
    srangehp(T, (ref_n, d), (s, d), nb, ofeltype(Int, l), imin)
end

function sfloatrange(b::AbstractFloat, s::AbstractFloat, l::Integer, d::AbstractFloat)
    T = promote_type(typeof(b), typeof(s), typeof(d))
    m = maxintfloat(T, Int)
    if abs(b) <= m && abs(s) <= m && abs(d) <= m
        ia, ist, idivisor = round(Int, b), round(Int, s), round(Int, d)
        if ia == b && ist == s && idivisor == d
            # We can return the high-precision range
            return sfloatrange(T, ia, ist, ofeltype(Int, l), idivisor)
        end
    end
    # Fallback (misses the opportunity to set offset different from 1,
    # but otherwise this is still high-precision)
    srangehp(T, (b, d), (s, d), Base.nbitslen(T, l, SOne), ofeltype(Int, l), SOne)
end
