struct SChar{C} <: AbstractChar
    function SChar{C}() where C
        !(C isa Char) && throw(ArgumentError("SChar only supports static Char storage, got $(typeof(C))"))
        return new{C}()
    end
end
SChar(c::Char) = SChar{c}()
Base.values(::SChar{C}) where C = C::Char

Base.isvalid(::SChar{C}) where C = isvalid(C::Char)
Base.show(io::IO, x::SChar{C}) where C = print(io, C::Char)

struct SSymbol{S}
    function SSymbol{S}() where S
        !(S isa Symbol) && throw(ArgumentError("SSymbol only supports static Symbol storage, got $(typeof(S))"))
        return new{S}()
    end
end

SSymbol(s::Symbol) = SSymbol{s}()
Base.values(::SSymbol{S}) where S = S::Symbol

Base.show(io::IO, x::SSymbol{S}) where S = print(io, S::Symbol)

const AbstractSymbol = Union{SSymbol{<:Any},Symbol}
