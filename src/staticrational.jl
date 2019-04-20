# TODO: StaticRational
abstract type StaticRational{T} <: Real end
struct SRational{V,T} <: StaticRational{T} end
struct MRational{T} <: StaticRational{T} end


