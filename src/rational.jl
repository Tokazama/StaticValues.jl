"""
    SRational

# Examples
```jldoctest
julia> SInt(3) // SInt(5)
3//5

julia> (SInt(3) // SInt(5)) // (SInt(2) // SInt(1))
3//10
```
"""
struct SRational{N<:SInteger,D<:SInteger} <: Real end

SRational(num::SIntegerZeroType, den::SIntegerZeroType) =
    throw("invalid rational: zero($(eltype(num))//zero($(eltype(den))")

for (S,B) in SI2BI
    @eval begin
        function SRational(num::$S, den::$S)
            num2, den2 = (sign(den) < 0) ? Base.divgcd(-num, -den) : Base.divgcd(num, den)
            SRational{typeof(num),typeof(den)}()
        end
    end
end

SRational(n::SInteger, d::SInteger) = SRational(promote(n,d)...)
SRational(n::SInteger) = SRational(n,one(n))


Base.numerator(::SRational{N,D}) where {N<:SInteger,D<:SInteger} = N()
Base.denominator(::SRational{N,D}) where {N<:SInteger,D<:SInteger} = D()

Base.decompose(x::SRational) = numerator(x), SZero, denominator(x)

Base.:(//)(n::SInteger,  d::SInteger) = SRational(n,d)

function Base.:(//)(x::SRational, y::SInteger)
    xn, yn = Base.divgcd(numerator(x), y)
    xn//(denominator(x) * yn)
end

function Base.:(//)(x::SInteger,  y::SRational)
    xn, yn = Base.divgcd(x, numerator(y))
    (xn * denominator(y))//yn
end
function Base.:(//)(x::SRational, y::SRational)
    xn, yn = Base.divgcd(numerator(x), numerator(y))
    xd, yd = Base.divgcd(denominator(x), denominator(y))
    (xn * yd)//(xd * yn)
end

#//(x::SComplex,  y::SReal) = complex(real(x)//y,imag(x)//y)
#//(x::SNumber, y::SComplex) = x*conj(y)//abs2(y)


#//(X::AbstractArray, y::SNumber) = X .// y

#=
Base.one(::SRational{<:Any,T}) where {V,T<:BaseInteger} = SRational{Tuple{T(1)::T,T(1)::T},T}()
Base.zero(::SRational{<:Any,T}) where {V,T<:BaseInteger} = SRational{Tuple{T(0)::T,T(1)::T},T}()
Base.one(::Type{<:SRational{<:Any,T}}) where {V,T<:BaseInteger} = SRational{Tuple{T(1)::T,T(1)::T},T}()
Base.zero(::Type{<:SRational{<:Any,T}}) where {V,T<:BaseInteger} = SRational{Tuple{T(0)::T,T(1)::T},T}()
=#

# TODO
promote_rule(::Type{SRational{<:Any,T}}, ::Type{S}) where {T<:BaseInteger,S<:SInteger} = SRational{Any,promote_type(T,S)}
promote_rule(::Type{SRational{<:Any,T}}, ::Type{SRational{S}}) where {T<:BaseInteger,S<:BaseInteger} = SRational{Any,promote_type(T,S)}
promote_rule(::Type{SRational{<:Any,T}}, ::Type{S}) where {T<:BaseInteger,S<:SFloat} = promote_type(T, S)
