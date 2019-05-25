# TODO: SComplex
struct SComplex{V<:Tuple{I where I,R where R},T} <: Number
    SComplex(i::T, r::T) where T<:Real = new{Tuple{i,i},T}()
end
