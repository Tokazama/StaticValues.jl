#=
struct SRational{N,D,T<:Integer} <: Real
    function Rational{T}(num::Integer, den::Integer) where T<:Integer
        num == den == zero(T) && __throw_rational_argerror(T)
        num2, den2 = (sign(den) < 0) ? divgcd(-num, -den) : divgcd(num, den)
        new(num2, den2)
    end
end
=#
