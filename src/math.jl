
import Base: sin, cos, sincos, tan, sinh, cosh, tanh, asin, acos, atan, asinh, acosh,
             atanh, sec, csc, cot, asec, acsc, acot, sech, csch, coth, asech, acsch, acoth,
             sinpi, cospi, sinc, cosc, cosd, cotd, cscd, secd, sind, tand, acosd, acotd,
             acscd, asecd, asind, atand, cbrt

one2one = [:sin, :cos, :tan, :sinh, :cosh, :tanh, :asin, :acos, :atan, :asinh, :acosh,
           :atanh, :sec, :csc, :cot, :asec, :acsc, :acot, :sech, :csch, :coth, :asech,
           :acsch, :acoth, :sinpi, :cospi, :sinc, :cosc, :cosd, :cotd, :cscd, :secd, :sind,
           :tand, :acosd, :acotd, :acscd, :asecd, :asind, :atand, :cbrt]

#=
       log, log2, log10, log1p, exponent, exp, exp2, exp10, expm1,
       cbrt, sqrt, significand,
       hypot, max, min, minmax, ldexp, frexp,
clamp, clamp!, modf, ^, mod2pi, rem2pi,
# one2two = sincos,
=#


for (S,B) in SF2BF
    for f in one2one
        @eval begin
            $(f)(::$S{X}) where X = $S{$(f)(X::$B)}()
        end
    end

end



