
# promote start and stop, leaving step alone
#(:)(start::A, step, stop::C) where {A<:Real,C<:Real} =
#    (:)(convert(promote_type(A,C),start), step, convert(promote_type(A,C),stop))

for (ST,BT) in S2B
    for B in (ST,BT)
    for E in (ST,BT)
        B == BT && E == BT && continue
        if BT <: AbstractFloat
            eval(:((:)(a::$B, b::$E) = (:)(a, $(one(ST)), b)))
        elseif BT <: Integer
            if B == ST && E == ST
                eval(:((:)(a::$B, b::$E) = UnitSRange{$BT}(a, b)))
            else
                eval(:((:)(a::$B, b::$E) = UnitMRange{$BT}(a, b)))
            end
        else
            @eval begin
                (:)(start::$B, stop::$E) = (:)(start, ofeltype(stop-start, SOne), stop)
                (:)(a::$B, b::AbstractFloat, c::$E) = (:)(promote(a, b, c)...)
                (:)(a::$B, b::Real, c::$E) = (:)(promote(a, b, c)...)
            end
        end
    end
    end
end

# FIXME
for (ST,BT) in SI2BI
    for B in (ST,BT)
    for S in (ST,BT)
    for E in (ST,BT)
        B == BT && S == BT && E == BT && continue
        @eval begin
            (:)(a::$B, b::$S, c::$E) = _scolon(Base.OrderStyle($BT), Base.ArithmeticStyle($BT), a, b, c)
        end
    end
    end
    end
end
#(:)(start::A, step, stop::C) where {A<:Real,C<:Real} =
#    (:)(convert(promote_type(A,C),start), step, convert(promote_type(A,C),stop))

#(:)(start::SVal, stop::SVal) = (:)(start, oftype(stop-start, SOne), stop)

#=
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
=#
# AbstractFloat specializations
(:)(start::SFloat, stop::SFloat) = (:)(promote_toeltype(start, stop)...)
(:)(start::SFloat, stop::BaseFloat) = (:)(promote_toeltype(start, stop)...)
(:)(start::BaseFloat, stop::SFloat) = (:)(promote_toeltype(start, stop)...)


#(:)(start::T, step::T, stop::T) where {T<:AbstractFloat} =
#    _colon(OrderStyle(T), ArithmeticStyle(T), start, step, stop)

#=
for B in (SVal,BaseAny)
for S in (SVal,BaseAny)
for E in (SVal,BaseAny)
    B == BaseAny && S == BaseAny && E == BaseAny && continue
    eval(:((:)(start::$B, step::$S, stop::$E) = _scolon(start, step, stop)))
end
end
end
=#
for (ST,BT) in S2B
    for B in (ST,BT)
    for S in (SVal,BaseAny)
    for E in (ST,BT)
        if B == ST && S == SVal && E == ST
            @eval begin
                function _scolon(start::$B, step::$S, stop::$E)
                    T′ = typeof(start+zero(step))
                    StepSRange(ofeltype(T′,start), step, ofeltype(T′,stop))
                end
            end
        else
            @eval begin
                function _scolon(start::$B, step::$S, stop::$E)
                    T′ = typeof(start+zero(step))
                    StepMRange(ofeltype(T′,start), step, ofeltype(T′,stop))
                end
            end
            #=
           =#
        end
        B == BT && S == BaseAny && E == BT && continue
        @eval begin
            (:)(a::$B, b::$S, c::$E) = (:)(promote(a,b,c)...)
        end
    end
    end
    end
end

function _scolon(start::T, step::SVal, stop::T) where T
    T′ = typeof(start+zero(step))
    StepSRange(ofeltype(T′,start), step, ofeltype(T′,stop))
end
#=
for (ST,BT) in S2B
    for B in (ST,BT)
    for S in (SReal,BaseReal)
    for E in (ST,BT)  (Colon())(SFloat(1.0), SFloat(0.2), SFloat(2.0))
        B == BT && S == BaseReal && E == BT && continue
        @eval begin
            @eval begin
            end
        end
    end
    end
    end
end

start = SVal(1.)
s = SVal(.2)
stop = SVal(2.)
isbetween(a, x, b) = (a <= x <= b || b <= x <= a)
=#
const STwo = SInt(2)
for (ST,BT) in SF2BF
    for B in (ST, BT)
    for S in (ST, BT)
    for E in (ST, BT)
        B == BT && S == BT && E == BT && continue
        @eval begin
            function (:)(start::$B, s::$S, stop::$E)
                s == 0 && throw(ArgumentError("range step cannot be zero"))
                # see if the inputs have exact rational approximations (and if so,
                # perform all computations in terms of the rationals)
                step_n, step_d = Base.rat(s)
                if step_d != 0 && $BT(step_n/step_d) == s
                    start_n, start_d = Base.rat(start)
                    stop_n, stop_d = Base.rat(stop)
                    if start_d != 0 && stop_d != 0 &&
                            (start_n/start_d) == start && (stop_n/stop_d) == stop
                        den = lcm(start_d, step_d) # use same denominator for start and step
                        m = maxintfloat($ST, Int)
                        if den != 0 && abs(start*den) <= m && abs(s*den) <= m &&  # will round succeed?
                                rem(den, start_d) == 0 && rem(den, step_d) == 0      # check lcm overflow
                            start_n = round(Int, start*den)
                            step_n = round(Int, s*den)
                            len = max(SZero, div(den*stop_n - stop_d*start_n + step_n*stop_d, step_n*stop_d))
                            # Integer ops could overflow, so check that this makes sense
                            if isbetween(start, start + (len-SOne)*s, stop + s/STwo) && !isbetween(start, start + len*s, stop)
                                # Return a 2x precision range
                                return sfloatrange($BT, start_n, step_n, len, den)
                            end
                        end
                    end
                end
                # Fallback, taking start and step literally
                lf = (stop-start)/s
                if lf < 0
                    len = SZero
                elseif lf == 0
                    len = SOne
                else
                    len = round(Int, lf) + SOne
                    stop′ = start + (len-SOne)*s
                    if len isa SInteger
                        len -= SInt(start < stop < stop′) + SInt(start > stop > stop′)
                    else
                        len -= (start < stop < stop′) + (start > stop > stop′)
                    end
                end
                srangehp($BT, start, s, SOne, len, SOne)
            end
        end
    end
    end
    end
end
