#promote_rule(::Type{S}, ::Type{T}) where {S<:AbstractIrrational,T<:SNumber} =
#    promote_type(promote_type(S, real(T)), T)
