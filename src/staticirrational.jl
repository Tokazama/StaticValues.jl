# TODO: StaticIrrational
abstract type StaticIrrational{T} <: AbstractIrrational end
struct SIrrational{V,T} <: StaticIrrational{T} end
struct MIrrational{T} <: StaticIrrational{T} end
