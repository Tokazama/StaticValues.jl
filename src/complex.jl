"""
    SComplex{I,R}
"""
struct SComplex{R,I} <: Number
    SComplex(r::T, i::T) where T<:Real = new{r,i}()
end

@pure Base.real(::SComplex{R,I}) where {R,I} = R()
@pure Base.imag(::SComplex{R,I}) where {R,I} = I()
@pure Base.reim(::SComplex{R,I}) where {R,I} = (R(), I())
