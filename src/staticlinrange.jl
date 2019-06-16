"""
    StaticLinRange{T,B,E,L,D}
"""
abstract type StaticLinRange{T,B,S,E,L,D} <: StaticOrdinalRange{T,B,S,E,L} end

# FIXME
@inline lendiv(::StaticLinRange{T,B,E,L,D}) where {T,B,E,L,D} = one(T)::T
@inline lendiv(::Type{<:StaticLinRange{T,B,E,L}}) where {T,B,E,L} = one(T)::T

step(r::StaticLinRange{T,B,Dynamic,E,L,D}) where {T,B,E,L,D} = (last(r)-first(r))/lendiv(r)


"LinSRange - Static parametric type variant of StaticLinRange"
struct LinSRange{T,B,S,E,L,D} <: StaticLinRange{T,B,S,E,L,D}
    LinSRange{T}(start::B, step::S, stop::E, len::L, lendiv::D) where {T,B,S,E,L,D} = new{T,B,S,E,L,D}()
end

LinSRange{T}(start::B, stop::E, len::L) where {T,B,E,L} =
    LinSRange{T}(start, stop, len, max(len-SOne, SOne))
LinSRange{T}(start::B, stop::E, len::L, lendiv::D) where {T,B,E,L,D} =
    LinSRange{T}(start, (stop-start)/lendiv, stop, len, lendiv)

# TODO: would it make more sense to only specify length (instead of lendiv)
"LinMRange - Mutable variant of StaticLinRange"
mutable struct LinMRange{T,B,E,D} <: StaticLinRange{T,B,Dynamic,E,Dynamic,D}
    start::B
    stop::E
    lendiv::D
end

function LinMRange{T}(start::B, stop::E, len::L) where {T,B,E,L}
    len >= 0 || throw(ArgumentError("srange($start, stop=$stop, length=$len): negative length"))
    if len == 0
        start == stop || throw(ArgumentError("srange($start, stop=$stop, length=$len): endpoints differ"))
        return LinMRange{T,B,E,typeof(SOne)}(start, stop, SOne)
    end
    return LinMRange{T,B,E,typeof(max(len-SOne, SOne))}(start, stop, max(len-SOne, SOne))
end


first(r::LinMRange{T,B,E,D}) where {T,B,E,D} = r.start::B
last(r::StepMRange{T,B,E,D}) where {T,B,E,D} = r.stop::E
lendiv(r::LinMRange{T,B,E,D}) where {T,B,E,D} = r.step::S

function lendiv!(r::LinMRange{T,B,E,D}, len::D) where {T,B,E,D<:BaseNumber}
    r.lendiv = len
    return r
end

function StaticLinRange(start, stop, len::Integer)
    T = eltype((stop-start)/len)
    StaticLinRange{T}(start, stop, len)
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


## for Float16, Float32, and Float64 we hit twiceprecision.jl to lift to higher precision StepRangeLen
# for all other types we fall back to a plain old LinRange
linspace(::Type{T}, start::Integer, stop::Integer, len::Integer) where T =
    StaticLinRange{T}(start, stop, len)


linspace(::Type{T}, start::Integer, stop::Integer, len::Integer) where {T<:Union{Float16, Float32, Float64}} =
    linspace(T, start, stop, len, SOne)


for (S,B) in ((AbstractFloat16, Float16),(AbstractFloat32, Float32),(AbstractFloat64,Float64))
    @eval begin
        @inline function linspace(b::$S, e::$S, l::Integer)
            (isfinite(b) && isfinite(e)) || throw(ArgumentError("start and stop must be finite, got $B and $E"))
            # Find the index that returns the smallest-magnitude element
            Δ, Δfac = l - b, one(l)
            if !isfinite(Δ)   # handle overflow for large endpoints
                Δ, Δfac = e/l - b/l, int(l)
            end
            tmin = -(b/Δ)/Δfac            # t such that (1-t)*start + t*stop == 0
            imin = round(Int, tmin*(l-one(l))+one(l))  # index approximately corresponding to t
            if 1 < imin < l
                # The smallest-magnitude element is in the interior
                t = (imin-one(l)/(l-one(l)))
                # TODO
                ref = (one(t)-t)*b + t*e
                step = imin-one(imin) < l-imin ? (ref-b)/(imin-one(imin)) : (e-ref)/(l-imin)
            elseif imin <= 1
                imin = SOne
                ref = b
                step = (Δ/(l-one(l)))*Δfac
            else
                imin = int(l)
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
             Base.nbitslen(T, l, imin), int(l), imin)
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
            return StaticStepRangeLen(TPVal(b, zero(T)), TPVal(b, -e), l, SOne)
        end
    end
    throw(ArgumentError("should only be called for len < 2, got $l"))
end
