function Base.divgcd(x::SInteger, y::SInteger)
    g = gcd(x,y)
    div(x,g), div(y,g)
end

struct SRational{V<:Tuple{N where N,D where D},T} <: Real end

function SRational(num::T, den::T) where T<:SInteger
    num == den == SZero && throw("invalid rational: zero($(eltype(num))//zero($(eltype(den))")
    num2, den2 = (sign(den) < SZero) ? divgcd(-num, -den) : divgcd(num, den)
    SRational{Tuple{values(num2),values(den2)},T}()
end

SRational(n::SInteger, d::SInteger) = SRational(promote(n,d)...)
SRational(n::SInteger) = SRational(n,one(n))


SRational(n::T, d::T) where T<:Real = new{Tuple{n,d},T}()

Base.numerator(::SRational{Tuple{N,D},T}) where {N,D,T<:Integer} = SInteger{N::T,T}()
Base.denominator(::SRational{Tuple{N,D},T}) where {N,D,T<:Integer} = SInteger{D::T,T}()


//(n::SInteger,  d::SInteger) = SRational(n,d)

function //(x::SRational{Tuple{N,D},T}, y::SInteger) where {N,D,T}
    xn, yn = divgcd(numerator(x), y)
    xn//(denominator(x) * yn)
end

function //(x::SInteger,  y::SRational)
    xn, yn = divgcd(x, numerator(y))
    (xn * denominator(y))//yn
end
function //(x::SRational, y::SRational)
    xn, yn = divgcd(numerator(x), numerator(y))
    xd, yd = divgcd(denominator(x), denominator(y))
    (xn * yd)//(xd * yn)
end

#//(x::SComplex,  y::SReal) = complex(real(x)//y,imag(x)//y)
#//(x::SNumber, y::SComplex) = x*conj(y)//abs2(y)


#//(X::AbstractArray, y::SNumber) = X .// y

Base.one(::SRational{<:Any,T}) where {V,T<:BaseInteger} = SRational{Tuple{T(1)::T,T(1)::T},T}()
Base.zero(::SRational{<:Any,T}) where {V,T<:BaseInteger} = SRational{Tuple{T(0)::T,T(1)::T},T}()
Base.one(::Type{<:SRational{<:Any,T}}) where {V,T<:BaseInteger} = SRational{Tuple{T(1)::T,T(1)::T},T}()
Base.zero(::Type{<:SRational{<:Any,T}}) where {V,T<:BaseInteger} = SRational{Tuple{T(0)::T,T(1)::T},T}()


# TODO
promote_rule(::Type{SRational{<:Any,T}}, ::Type{S}) where {T<:BaseInteger,S<:SInteger} = SRational{Any,promote_type(T,S)}
promote_rule(::Type{SRational{<:Any,T}}, ::Type{SRational{S}}) where {T<:BaseInteger,S<:BaseInteger} = SRational{Any,promote_type(T,S)}
promote_rule(::Type{SRational{<:Any,T}}, ::Type{S}) where {T<:BaseInteger,S<:SFloat} = promote_type(T, S)
