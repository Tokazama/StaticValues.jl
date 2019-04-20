mutable struct MVal{T} <: StaticValue{T}
    val::T

    MVal(val::T) where T = new{T}(val)
end

(::Type{MVal{SVal{V,T}}})() where {V,T} = MVal(SVal{V::T,T}())
(::Type{MVal{HPSVal{T,H,L}}})() where {T,H,L} = MVal(HPSVal{T,H::T,L::T}())


Base.get(v::MVal{SVal{V,T}}) where {V,T} = SVal{V::T,T}()
Base.get(v::MVal{HPSVal{T,H,L}}) where {T,H,L} = HPSVal{T,H::T,L::T}()
Base.get(v::MVal{T}) where T = v.val::T

(::Type{T})(x::MVal) where T<:Number = T(get(x))::T
(::Type{MVal{T}})(x) where T = MVal(T(x))
(::Type{MVal{T}})(x::SVal{V,T}) where {T,V} = x






