struct SChar{V} <: AbstractChar
    function SChar{V}() where V
        !(V isa Char) && throw(ArgumentError("SChar only supports static Char storage, got $(typeof(V))"))
        return new{V}()
    end
end

Base.show(io::IO, x::SChar{V}) where V = print(io, V::Char)
