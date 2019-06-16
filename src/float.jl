struct SFloat64{V} <: AbstractFloat
    function SFloat64{V}() where V
        !(V isa Float64) && throw(ArgumentError("SFloat64 only supports static Float64 storage, got $(typeof(V))"))
        new{V}()
    end
end

struct SFloat32{V} <: AbstractFloat
    function SFloat32{V}() where V
        !(V isa Float32) && throw(ArgumentError("SFloat32 only supports static Float32 storage, got $(typeof(V))"))
        new{V}()
    end
end

struct SFloat16{V} <: AbstractFloat
    function SFloat16{V}() where V
        !(V isa Float16) && throw(ArgumentError("SFloat16 only supports static Float16 storage, got $(typeof(V))"))
        new{V}()
    end
end

const SFloat{V} = Union{SFloat16{V},SFloat32{V},SFloat64{V}}


function SFloat(val::Val{V}) where V
    if V isa Float16
        SFloat16{V}()
    elseif V isa Float32
        SFloat32{V}()
    else
        SFloat64{V}()
    end
end

Base.show(io::IO, ::SFloat{V}) where V = show(io, V)

const AbstractFloat64 = Union{SFloat64,Float64}
const AbstractFloat32 = Union{SFloat32,Float32}
const AbstractFloat16 = Union{SFloat16,Float16}


SF2BF = Dict(SFloat64 => Float64,
             SFloat32 => Float32,
             SFloat16 => Float16)

BF2SF = Dict(Float64 => SFloat64,
             Float32 => SFloat32,
             Float16 => SFloat16)


