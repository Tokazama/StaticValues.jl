for (ST,BT) in StaticValues.SI2BI
    @eval begin
        (:)(start::$(ST), stop::$(ST)) = UnitSRange{$BT}(start, stop)
        (:)(start::$(ST), stop::$BT) = UnitMRange{$BT}(start, stop)
        (:)(start::$BT,   stop::$(ST)) = UnitMRange{$BT}(start, stop)
    end
end

(:)(start::SVal, stop::SVal) = (:)(start, oftype(stop-start, 1), stop)

# promote start and stop, leaving step alone
function (:)(start::SReal, step, stop::BaseReal)
    T = eltype(start)
    if T == eltype(stop)
        _scolon(start, step, stop)
    else
        (:)(ofeltype(promote_eltype(start, stop), start), step, ofeltype(promote_eltype(start, stop), stop))
    end
end
function (:)(start::BaseReal, step::Integer, stop::SReal)
    T = eltype(start)
    if T == eltype(stop)
        _scolon(start, step, stop)
    else
        (:)(ofeltype(promote_eltype(start, stop), start), step, ofeltype(promote_eltype(start, stop), stop))
    end
end
function (:)(start::SReal, step::Integer, stop::SReal)
    T = eltype(start)
    if T == eltype(stop)
        _scolon(start, step, stop)
    else
        (:)(ofeltype(promote_eltype(start, stop), start), step, ofeltype(promote_eltype(start, stop), stop))
    end
end

# AbstractFloat specializations
(:)(start::SFloat, stop::SFloat) = (:)(promote_toeltype(start, stop)...)
(:)(start::SFloat, stop::BaseFloat) = (:)(promote_toeltype(start, stop)...)
(:)(start::BaseFloat, stop::SFloat) = (:)(promote_toeltype(start, stop)...)


#(:)(start::T, step::T, stop::T) where {T<:AbstractFloat} =
#    _colon(OrderStyle(T), ArithmeticStyle(T), start, step, stop)


(:)(start::SVal, step::SVal, stop::SVal) = _scolon(start, step, stop)
(:)(start::Any, step::SVal, stop::SVal) = _scolon(start, step, stop)
(:)(start::Any, step::Any, stop::SVal) = _scolon(start, step, stop)
(:)(start::SVal, step::Any, stop::SVal) = _scolon(start, step, stop)
(:)(start::SVal, step::Any, stop::Any) = _scolon(start, step, stop)
(:)(start::SVal, step::SVal, stop::Any) = _scolon(start, step, stop)
(:)(start::Any, step::SVal, stop::Any) = _scolon(start, step, stop)

function _scolon(start::AbstractFloat, step::AbstractFloat, stop::AbstractFloat)
    T = promote_eltype(start, step, stop)
    _scolon(ofeltype(T, start), ofeltype(T, step), ofeltype(T, stop))
end

function _scolon(start::Any, step::AbstractFloat, stop::Any)
    T = promote_eltype(start, step, stop)
    _scolon(ofeltype(T, start), ofeltype(T, step), ofeltype(T, stop))
end

function _scolon(start::Any, step::SReal, stop::Any)
    T2 = eltype(start + zero(start))
    StaticStepRange{T2}(ofeltype(T2, start), step, ofeltype(T2, stop))
end

for (ST,BT) in ((SFloat16, Float16),
                (SFloat32, Float32),
                (SFloat64, Float64))
    @eval begin
        (:)(start::$ST, stop::$ST) = (:)(start, one($ST), stop)
        (:)(start::$BT, stop::$ST) = (:)(start, one($ST), stop)
        (:)(start::$ST, stop::$BT) = (:)(start, one($ST), stop)
    end
end

for (ST,BT) in ((SFloat16, Float16),
                (SFloat32, Float32),
                (SFloat64, Float64))
    for B in (ST, BT)
    for S in (ST, BT)
    for E in (ST, BT)
        @eval begin
            function _scolon(start::$B, step::$S, stop::$E)
                step == 0 && throw(ArgumentError("range step cannot be zero"))
                # see if the inputs have exact rational approximations (and if so,
                # perform all computations in terms of the rationals)
                step_n, step_d = Base.rat(step)
                if step_d != 0 && $BT(step_n/step_d) == step
                    start_n, start_d = Base.rat(start)
                    stop_n, stop_d = Base.rat(stop)
                    if start_d != 0 && stop_d != 0 &&
                            (start_n/start_d) == start && (stop_n/stop_d) == stop
                        den = lcm(start_d, step_d) # use same denominator for start and step
                        m = maxintfloat($BT, Int)
                        if den != 0 && abs(start*den) <= m && abs(step*den) <= m &&  # will round succeed?
                                rem(den, start_d) == 0 && rem(den, step_d) == 0      # check lcm overflow
                            start_n = round(Int, start*den)
                            step_n = round(Int, step*den)
                            len = max(SZero, div(den*stop_n - stop_d*start_n + step_n*stop_d, step_n*stop_d))
                            # Integer ops could overflow, so check that this makes sense
                            if isbetween(start, start + (len-1)*step, stop + step/2) &&
                                    !isbetween(start, start + len*step, stop)
                                # Return a 2x precision range
                                return sfloatrange($BT, start_n, step_n, len, den)
                            end
                        end
                    end
                end
                # Fallback, taking start and step literally
                lf = (stop-start)/step
                if lf < 0
                    len = SZero
                elseif lf == 0
                    len = SOne
                else
                    len = round(Int, lf) + SOne
                    stop′ = start + (len-SOne)*step
                    if len isa SInteger
                        len -= SInt(start < stop < stop′) + SInt(start > stop > stop′)
                    else
                        len -= (start < stop < stop′) + (start > stop > stop′)
                    end
                end
                srangehp($BT, start, step, SOne, len, SOne)
            end
        end
    end
    end
    end
end


# without the second method above, the first method above is ambiguous with
# (:)(start::A, step, stop::C) where {A<:Real,C<:Real}

_scolon(::Base.Ordered, ::Any, start, step, stop) = StaticStepRange(start, step, stop)
# for T<:Union{Float16,Float32,Float64} see twiceprecision.jl
_scolon(::Base.Ordered, ::Base.ArithmeticRounds, start, step, stop) =
    StaticStepRangeLen(start, step, floor(Int, (stop-start)/step)+1)
_scolon(::Any, ::Any, start, step, stop) =
    StaticStepRangeLen(start, step, floor(Int, (stop-start)/step)+1)


