struct SChar{V} <: AbstractChar
    function SChar{V}() where V
        !(V === Char) && throw(ArgumentError("SChar only supports static Char storage, got $(typeof(V))"))
        return new{V}()
    end
end

