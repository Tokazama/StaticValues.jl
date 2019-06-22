import Base: _range, _rangestyle

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

# floor(Int, (stop-start)/step)
for (ST,BT) in S2B
    for B in (ST,BT)
    for S in (SVal,BaseAny)
    for E in (ST,BT)
        if B == ST && S == SVal && E == ST
            @eval begin
                _scolon(::Base.Ordered, ::Any, start::$B, step::$S, stop::$E) =
                    StepSRange(start, step, stop)
                _scolon(::Base.Ordered, ::Base.ArithmeticRounds, start::$B, step::$S, stop::$E) =
                    StepSRangeLen(start, step, floor(Int, (stop-start)/step)+SOne)
                _scolon(::Any, ::Any, start::$B, step::$S, stop::$E) =
                    StepSRangeLen(start, step, floor(Int, (stop-start)/step)+SOne)

            end
        else
            @eval begin
                _scolon(::Base.Ordered, ::Any, start::$B, step::$S, stop::$E) =
                    StepMRange(start, step, stop)
                _scolon(::Base.Ordered, ::Base.ArithmeticRounds, start::$B, step::$S, stop::$E) =
                    StepMRangeLen(start, step, floor(Int, (stop-start)/step)+SOne)
                _scolon(::Any, ::Any, start::$B, step::$S, stop::$E) =
                    StepMRangeLen(start, step, floor(Int, (stop-start)/step)+SOne)
            end
        end
    end
    end
    end
end

_range(b::SReal, ::Nothing, ::Nothing, l::BaseInteger) = UnitMRange{eltype(b)}(b, b + l - SOne)
_range(b::BaseReal, ::Nothing, ::Nothing, l::SInteger) = UnitMRange{eltype(b)}(b, b + l - SOne)
_range(b::SReal, ::Nothing, ::Nothing, l::SInteger) = UnitSRange{eltype(b)}(b, b + l - SOne)

Base.OrderStyle(x::Type{<:SReal}) = Base.OrderStyle(eltype(x))
Base.ArithmeticStyle(x::Type{<:SReal}) = Base.ArithmeticStyle(eltype(x))

_rangestyle(::Base.Ordered, ::Base.ArithmeticWraps, a::SVal, step::SVal, len::SInteger) =
    StepSRange{eltype(a)}(a, step, ofeltype(a, a+step*(len-SOne)))
_rangestyle(::Any, ::Any, a::SVal, step::SVal, len::SInteger) =
    StepSRangeLen{eltype(a+SZero*step)}(a, step, len)

_rangestyle(::Base.Ordered, ::Base.ArithmeticWraps, a::SVal, step::Real, len::Integer) =
    StepMRange{eltype(a)}(a, step, ofeltype(eltype(a), a+step*(len-SOne)))
_rangestyle(::Any, ::Any, a::SVal, step::Real, len::Integer) =
    StepMRangeLen{eltype(a+SZero*step)}(a, step, len)

_rangestyle(::Base.Ordered, ::Base.ArithmeticWraps, a::Real, step::SReal, len::Integer) =
    StepMRange{eltype(a)}(a, step, ofeltype(a, a+step*(len-SOne)))
_rangestyle(::Any, ::Any, a::Real, step::SReal, len::Integer) =
    StepMRangeLen{typeof(a+SZero*step)}(a, step, len)

_rangestyle(::Base.Ordered, ::Base.ArithmeticWraps, a::Real, step::Real, len::SInteger) =
    StepMRange{eltype(a)}(a, step, ofeltype(a, a+step*(len-SOne)))
_rangestyle(::Any, ::Any, a::Real, step::Real, len::SInteger) =
    StepMRangeLen{typeof(a+SZero*step)}(a, step, len)

for B in (SNumber,BaseNumber)
for E in (SNumber,BaseNumber)
for L in (SInteger,BaseInteger)
    if B == BaseNumber && E == BaseNumber && L == BaseInteger
        continue
    elseif B == SNumber && E == SNumber && L == SInteger
        @eval begin
            _range(start::$B, ::Nothing, stop::$E, len::$L) = LinSRange{eltype(start)}(start, stop, len)
        end
    else
        @eval begin
            _range(start::$B, ::Nothing, stop::$E, len::$L) = LinMRange{eltype(start)}(start, stop, len)
        end
    end
end
end
end

for (ST,BT) in SI2BI
    for B in (ST,BT)
    for E in (ST,BT)
    for L in (SInteger,BaseInteger)
        B == BT && E == BT && L == BaseInteger && continue
        @eval begin
            _range(start::$B, ::Nothing, stop::$E, len::$L) =
                linspace(float($BT), start, stop, len)
        end
    end
    end
    end
end

# For Float16, Float32, and Float64, this returns a StepRangeLen
#=
for (ST,BT) in SF2BF
    for B in (ST,BT)
    for E in (ST,BT)
    for L in (SInteger,BaseInteger)
        B == BT && E == BT && L == BaseInteger && continue
        @eval begin
            function _range(start::$B, ::Nothing, stop::$E, len::$L)
                len < 2 && return linspace1(T, start, stop, len)
                if start == stop
                    return srangehp(eltype(start), start, zero(start), SZero, len, SOne)
                end
                # Attempt to find exact rational approximations
                start_n, start_d = rat(start)
                stop_n, stop_d = rat(stop)
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
        end
    end
    end
    end
end
=#
for (SF,BF) in SF2BF
    for B in (SF,BF)
    for S in (SF,BF)
    for L in (SInteger,BaseInteger)
        B == BF && S == SF && L == BaseInteger && continue
        @eval begin
            function _range(start::$B, ::Nothing, stop::$S, len::$L)
                len < 2 && return linspace1($BF, start, stop, len)
                if start == stop
                    return srangehp($BF, start, zero($B), SZero, len, SOne)
                end
                # Attempt to find exact rational approximations
                start_n, start_d = rat(start)
                stop_n, stop_d = rat(stop)
                if start_d != 0 && stop_d != 0
                    den = lcm(start_d, stop_d)
                    m = maxintfloat($B, Int)
                    if den != 0 && abs(den*start) <= m && abs(den*stop) <= m
                        start_n = round(Int, den*start)
                        stop_n = round(Int, den*stop)
                        if $BF(start_n/den) == start && $BF(stop_n/den) == stop
                            return linspace($BF, start_n, stop_n, len, den)
                        end
                    end
                end
                linspace(start, stop, len)
            end

            function _range(a::$B, st::$S, ::Nothing, len::$L)
                start_n, start_d = rat(a)
                step_n, step_d = rat(st)
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
        end
    end
    end
    end
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


for B in (SInteger,BaseInteger)
for E in (SInteger,BaseInteger)
for L in (SInteger,BaseInteger)
    if B == SInteger && E == SInteger && L == SInteger
        eval(:(linspace(::Type{T}, start::$B, stop::$E, len::$L) where T = LinSRange{T}(start, stop, len)))
    else
        eval(:(linspace(::Type{T}, start::$B, stop::$E, len::$L) where T = LinMRange{T}(start, stop, len)))
    end
    # this had to be included to avoid ambiguity
    eval(:(linspace(::Type{T}, start::$B, stop::$E, len::$L) where T<:Union{Float16, Float32, Float64} = linspace(T, start, stop, len, SOne)))
end
end
end



for (S,B) in ((AbstractFloat16, Float16),(AbstractFloat32, Float32),(AbstractFloat64,Float64))
    @eval begin
        @inline function linspace(b::$S, e::$S, l::Integer)
            (isfinite(b) && isfinite(e)) || throw(ArgumentError("start and stop must be finite, got $b and $e"))
            # Find the index that returns the smallest-magnitude element
            Δ, Δfac = l - b, one(l)
            if !isfinite(Δ)   # handle overflow for large endpoints
                Δ, Δfac = e/l - b/l, ofeltype(Int64, l)
            end
            tmin = -(b/Δ)/Δfac            # t such that (1-t)*start + t*stop == 0
            imin = round(Int, tmin*(l-one(l))+one(l))  # index approximately corresponding to t
            if 1 < imin < l
                # The smallest-magnitude element is in the interior
                t = (imin-one(l)/(l-one(l)))
                ref = (one(t)-t)*b + t*e
                step = imin-one(imin) < l-imin ? (ref-b)/(imin-one(imin)) : (e-ref)/(l-imin)
            elseif imin <= 1
                imin = SOne
                ref = b
                step = (Δ/(l-one(l)))*Δfac
            else
                imin = ofeltype(Int64, l)
                ref = e
                step = (Δ/(l-one(l)))*Δfac
            end
            if l == 2 && !isfinite(step)
                # For very large endpoints where step overflows, exploit the
                # split-representation to handle the overflow
                return srangehp($B, b, (-b, e), SZero, SInt(2))
            end
            # 2x calculations to get high precision endpoint matching while also
            # preventing overflow in ref_hi+(i-offset)*step_hi
            m, k = prevfloat(floatmax(b)), max(imin-one(imin), l-imin)
            step_hi_pre = clamp(step, max(-(m+ref)/k, (-m+ref)/k), min((m-ref)/k, (m+ref)/k))
            nb = Base.nbitslen($B, l, imin)
            step_hi = truncbits(step_hi_pre, nb)
            x1_hi, x1_lo = Base.add12((one(imin)-imin)*step_hi, ref)
            x2_hi, x2_lo = Base.add12((l-imin)*step_hi, ref)
            a, c = (b - x1_hi) - x1_lo, (e - x2_hi) - x2_lo
            step_lo = (c - a)/(l - one(l))
            ref_lo = a - (one(imin) - imin)*step_lo
            srangehp($B, (ref, ref_lo), (step_hi, step_lo), SZero, l, imin)
        end
    end
end

# range for rational numbers, start = start_n/den, stop = stop_n/den
# Note this returns a StepRangeLen
function linspace(::Type{T}, b::Integer, e::Integer, l::Integer, d::Integer
                 ) where {T<:Union{Float16, Float32, Float64}}
    l < 2 && return linspace1(T, b/d, e/d, l)
    b == e && return srangehp(T, (b, d), (zero(b), d), SZero, l)
    tmin = -b/(float(e) - float(b))
    imin = round(Int, tmin*(l-one(l))+one(l))
    imin = clamp(imin, SOne, ofeltype(Int, l))
    ref_num = ofeltype(Int128, l-imin) * b + ofeltype(Int128, imin-one(imin)) * e
    ref_denom = ofeltype(Int128, l-one(l)) * d
    srangehp(T, (ref_num, ref_denom), (ofeltype(Int128, e) - ofeltype(Int128, b), ref_denom),
             Base.nbitslen(T, l, imin), ofeltype(Int64, l), imin)
end

# For len < 2
function linspace1(::Type{T}, b::B, e::E, l::Integer) where {B,E,T<:Union{Float16, Float32, Float64}}
    l >= SZero || throw(ArgumentError("srange($(values(b)), stop=$(values(e)), length=$(values(l))): negative length"))
    if l <= 1
        l == 1 && (b == e || throw(ArgumentError("srange($(values(b)), stop=$(values(e)), length=$(values(l))): endpoints differ")))
        # Ensure that first(r)==start and last(r)==stop even for len==0
        # The output type must be consistent with steprangelen_hp
        if T<:Union{Float32,Float16}
            return StaticStepRangeLen{T}(f64(b), f64(b) - f64(e), l, SOne)
        else
            return StaticStepRangeLen(tpval(b, zero(T)), tpval(b, -e), l, SOne)
        end
    end
    throw(ArgumentError("should only be called for len < 2, got $l"))
end

SITuple = Tuple{<:Integer,<:Integer}

srangehp(::Type{Float64}, b::SITuple, s::SITuple, nb::Integer, l::Integer, f::Integer) =
    StaticStepRangeLen(tpval(Float64, b), tpval(Float64, s, nb), l, f)

srangehp(::Type{T}, b::SITuple, s::SITuple, nb::Integer, l::Integer, f::Integer) where T =
    StaticStepRangeLen{T}(b[1]/b[2], s[1]/s[2], ofeltype(Int64, l), f)

srangehp(::Type{Float64}, b::F_or_FF, s::F_or_FF, nb::Integer, l::Integer, f::Integer) =
    StaticStepRangeLen(tp64(b), Base.twiceprecision(tp64(s), nb), ofeltype(Int64, l), f)

srangehp(::Type{T}, b::F_or_FF, s::F_or_FF,
         nb::Integer, l::Integer, f::Integer) where {T<:Union{Float16,Float32}} =
    StaticStepRangeLen{T}(f64(b), f64(s), ofeltype(Int64, l), f)


