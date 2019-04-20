## Unsigned ##
abstract type StaticUnsigned{T} <: Unsigned end

struct SUnsigned{V,T<:Unsigned} <: StaticUnsigned{V}
    function SUnsigned{V,T}() where {V,T<:Unsigned}
        !(typeof(V) === T) && throw(ArgumentError("value must be of type $T"))
        new{V,T}()
    end
end

mutable struct MUnsigned{T} <: StaticUnsigned{T}
    value::T
end

SUnsigned(v::T) where T<:Unsigned = SUnsigned{v,T}()
SUnsigned{V}() where V = SUnsigned{V,typeof(V)}()
SUnsigned(v::SUnsigned) = v
SUnsigned(v::MUnsigned{T}) where T = SUnsigned(v.value::T)
SUnsigned(::Val{V}) where V = SUnsigned{V}()

MUnsigned(v::Unsigned) = MUnsigned{typeof(v)}(v)
MUnsigned(v::SUnsigned{V,T}) where {V,T<:Unsigned} = MUnsigned{SUnsigned{V::T,T}}(v)
MUnsigned(v::Val{V}) where V = MUnsigned(SUnsigned(v))


## Signed ##
abstract type StaticSigned{T} <: Signed end

struct SSigned{V,T<:Signed} <: StaticSigned{V}
    function SSigned{V,T}() where {V,T<:Signed}
        !(typeof(V) === T) && throw(ArgumentError("value must be of type $T"))
        new{V,T}()
    end
    SSigned(v::T) where T<:Signed= new{v,T}()
end
SSigned{V}() where {V} = SSigned{V,typeof(V)}()
SSigned(::Val{V}) where V = SSigned{V}()

mutable struct MSigned{T} <: StaticSigned{T}
    value::T

    MSigned(v::T) where T<:Signed = new{T}(v)
    MSigned(v::SSigned{V,T}) where {V,T<:Signed} = new{SSigned{V::T,T}}(v)
end
MSigned(v::Val{V}) where V = MSigned(SSigned(v))


"""
    SInteger{V,T}
"""
const SInteger{V,T} = Union{SUnsigned{V,T},SSigned{V,T},SBool{V,T}}

SInteger(v::Unsigned) = SUnsigned(v)
SInteger(v::Signed) = SSigned(v)
SInteger(v::Bool) = SBool(v)
SInteger(v::StaticBool) = SBool(v)

function SInteger(v::Val{V}) where V
    if isa(V, Unsigned)
        SUnsigned(v)
    elseif isa(V, Signed)
        SSigned(v)
    elseif isa(V, Bool)
        SBool(v)
    else
        throw(ArgumentError("value must be of type Integer"))
    end
end

"""
MInteger{T}
"""
const MInteger{T} = Union{MUnsigned{T},MSigned{T},MBool{T}}

Base.values(v::MInteger{T}) where {T<:Integer} = v.value::T
@pure Base.values(v::MInteger{SInteger{V,T}}) where {V,T<:Integer} = V::T

Base.leading_zeros(::SUnsigned{V,T}) where {V,T} = leading_zeros(V::T)

"""
    StaticInteger{T}
"""
const StaticInteger{T} = Union{StaticUnsigned{T},StaticSigned{T},StaticBool{T}}



#=
SUnsigned{V}() where V = SUnsigned{V,typeof(V)}()

(::Type{SUnsigned{V,T}})(x::T) where {V,T<:Unsigned} = SUnsigned{x,T}()
(::Type{SUnsigned{V,T1}})(x::T2) where {V,T1<:Unsigned,T2<:Unsigned} = SUnsigned{T1(x)::T1,T1}()


struct SSigned{V,T<:Signed} <: Signed end
SSigned(value::T) where T<:Signed = SSigned{value::T,T}()

(::Type{SSigned{V,T}})(x::T) where {V,T<:Signed} = SSigned{x,T}()
(::Type{SSigned{V,T1}})(x::T2) where {V,T1<:Signed,T2<:Signed} = SSigned{T1(x)::T1,T1}()

Base.values(::SUnsigned{V,T}) where {V,T<:Unsigned} = V::T
Base.values(::SSigned{V,T}) where {V,T<:Signed} = V::T


const SUInt128{V} = SUnsigned{V,UInt128}
const SUInt64{V} = SUnsigned{V,UInt64}
const SUInt{V} = SUInt64{V}
const SUInt32{V} = SUnsigned{V,UInt32}
const SUInt16{V} = SUnsigned{V,UInt16}
const SUInt8{V} = SUnsigned{V,UInt8}

const SBigInt{V} = SSigned{V,BigInt}
const SInt128{V} = SSigned{V,Int128}
const SInt16{V} = SSigned{V,Int16}
const SInt32{V} = SSigned{V,Int32}
const SInt64{V} = SSigned{V,Int64}
const SInt{V} = SInt64{V}
const SInt8{V} = SSigned{V,Int8}
const SInteger{V,T} = Union{SSigned{V,T},SUnsigned{V,T},SBool{V,T}}

function SInteger{V}()
end

Base.values(::SSigned{V,T}) where {V,T<:SSigned} = V::T


# immitate base values
const SBitSigned32_types      = (SInt8, SInt16, SInt32)
const SBitUnsigned32_types    = (SUInt8, SUInt16, SUInt32)
const SBitInteger32_types     = (SBitSigned32_types..., SBitUnsigned32_types...)

const SBitSigned64_types      = (SBitSigned32_types..., SInt64)
const SBitUnsigned64_types    = (SBitUnsigned32_types..., SUInt64)
const SBitInteger64_types     = (SBitSigned64_types..., SBitUnsigned64_types...)

const SBitSigned_types        = (SBitSigned64_types..., SInt128)
const SBitUnsigned_types      = (SBitUnsigned64_types..., SUInt128)
const SBitInteger_types       = (SBitSigned_types..., SBitUnsigned_types...)

const SBitSignedSmall_types   = SInt === SInt64 ? ( SInt8,  SInt16,  SInt32) : ( SInt8,  SInt16)
const SBitUnsignedSmall_types = SInt === SInt64 ? (SUInt8, SUInt16, SUInt32) : (SUInt8, SUInt16)
const SBitIntegerSmall_types  = (SBitSignedSmall_types..., SBitUnsignedSmall_types...)

const SBitSigned32      = Union{SBitSigned32_types...}
const SBitUnsigned32    = Union{SBitUnsigned32_types...}
const SBitInteger32     = Union{SBitInteger32_types...}

const SBitSigned64      = Union{SBitSigned64_types...}
const SBitUnsigned64    = Union{SBitUnsigned64_types...}
const SBitInteger64     = Union{SBitInteger64_types...}

const SBitSigned        = Union{SBitSigned_types...}
const SBitUnsigned      = Union{SBitUnsigned_types...}
const SBitInteger       = Union{SBitInteger_types...}

const SBitSignedSmall   = Union{SBitSignedSmall_types...}
const SBitUnsignedSmall = Union{SBitUnsignedSmall_types...}
const SBitIntegerSmall  = Union{SBitIntegerSmall_types...}

const SBitSigned64T     = Union{Type{SInt8}, Type{SInt16}, Type{SInt32}, Type{SInt64}}
const SBitUnsigned64T   = Union{Type{SUInt8}, Type{SUInt16}, Type{SUInt32}, Type{SUInt64}}

const SBitIntegerType = Union{map(T->Type{T}, SBitInteger_types)...}


Base.promote_rule(::Type{SInteger}, ::Type{SInteger}) = 
Base.promote_rule(::Type{SInt16}, ::Union{Type{SInt8}, Type{SUInt8}}) = SInt16
Base.promote_rule(::Type{SInt32}, ::Union{Type{SInt16}, Type{SInt8}, Type{SUInt16}, Type{SUInt8}}) = SInt32
Base.promote_rule(::Type{SInt64}, ::Union{Type{SInt16}, Type{SInt32}, Type{SInt8}, Type{SUInt16}, Type{SUInt32}, Type{SUInt8}}) = SInt64
Base.promote_rule(::Type{SInt128}, ::Union{Type{SInt16}, Type{SInt32}, Type{SInt64}, Type{SInt8}, Type{SUInt16}, Type{SUInt32}, Type{SUInt64}, Type{SUInt8}}) = SInt128
Base.promote_rule(::Type{SUInt16}, ::Union{Type{SInt8}, Type{SUInt8}}) = SUInt16
Base.promote_rule(::Type{SUInt32}, ::Union{Type{SInt16}, Type{SInt8}, Type{SUInt16}, Type{SUInt8}}) = SUInt32
Base.promote_rule(::Type{SUInt64}, ::Union{Type{SInt16}, Type{SInt32}, Type{SInt8}, Type{SUInt16}, Type{SUInt32}, Type{SUInt8}}) = SUInt64
Base.promote_rule(::Type{SUInt128}, ::Union{Type{SInt16}, Type{SInt32}, Type{SInt64}, Type{SInt8}, Type{SUInt16}, Type{SUInt32}, Type{SUInt64}, Type{SUInt8}}) = SUInt128
# with mixed signedness and same size, Unsigned wins
Base.promote_rule(::Type{SUInt8},   ::Type{SInt8}  ) = SUInt8
Base.promote_rule(::Type{SUInt16},  ::Type{SInt16} ) = SUInt16
Base.promote_rule(::Type{SUInt32},  ::Type{SInt32} ) = SUInt32
Base.promote_rule(::Type{SUInt64},  ::Type{SInt64} ) = SUInt64
Base.promote_rule(::Type{SUInt128}, ::Type{SInt128}) = SUInt128

# TODO
# - turns these into SInteger 
# - allow differnt vallows (not just T)
*(::SInteger{V1,T}, ::SInteger{V2,T}) where {V1,V2,T<:Integer} = SUnsigned{V1::T*V2::T,T}()

/(::SUnsigned{V1,T}, ::SUnsigned{V2,T}) where {V1,V2,T<:Unsigned} = SUnsigned{V1::T/V2::T,T}()
+(::SUnsigned{V1,T}, ::SUnsigned{V2,T}) where {V1,V2,T<:Unsigned} = SUnsigned{V1::T+V2::T,T}()
-(::SUnsigned{V1,T}, ::SUnsigned{V2,T}) where {V1,V2,T<:Unsigned} = SUnsigned{V1::T-V2::T,T}()
#(:&)(::SUnsigned{V1,T}, ::SUnsigned{V2,T}) where {V1,V2,T<:Unsigned} = SUnsigned{V1::T&V2::T,T}()
|(::SUnsigned{V1,T}, ::SUnsigned{V2,T}) where {V1,V2,T<:Unsigned} = SUnsigned{V1::T|V2::T,T}()
xor(::SUnsigned{V1,T}, ::SUnsigned{V2,T}) where {V1,V2,T<:Unsigned} = SUnsigned{xor(V1::T, V2::T),T}()

*(::SSigned{V1,T}, ::SSigned{V2,T}) where {V1,V2,T<:Signed} = SSigned{V1::T*V2::T,T}()
/(::SSigned{V1,T}, ::SSigned{V2,T}) where {V1,V2,T<:Signed} = SSigned{V1::T/V2::T,T}()
+(::SSigned{V1,T}, ::SSigned{V2,T}) where {V1,V2,T<:Signed} = SSigned{V1::T+V2::T,T}()
-(::SSigned{V1,T}, ::SSigned{V2,T}) where {V1,V2,T<:Signed} = SSigned{V1::T-V2::T,T}()
#(:&)(::SSigned{V1,T}, ::SSigned{V2,T}) where {V1,V2,T<:Signed} = SSigned{V1::T&V2::T,T}()
|(::SSigned{V1,T}, ::SSigned{V2,T}) where {V1,V2,T<:Signed} = SSigned{V1::T|V2::T,T}()
xor(::SSigned{V1,T}, ::SSigned{V2,T}) where {V1,V2,T<:Signed} = SSigned{xor(V1::T, V2::T),T}()



=#
#=
const SOne = SVal{1,Int}()
SOne(::Type{T}) where T = SVal{T(1),T}()
SOne(::SVal{V,T}) where {V,T} = SVal{T(1),T}()
SOne(::Type{SVal{V,T}}) where {V,T} = SVal{T(1),T}()
SOne(x::T) where T = SVal{T(1),T}()

const SZero = SVal{0,Int}()
SZero(::Type{T}) where T = SVal{T(0),T}()
SZero(::SVal{V,T}) where {V,T} = SVal{T(0),T}()
SZero(::Type{SVal{V,T}}) where {V,T} = SVal{T(0),T}()
SZero(x::T) where T = SVal{T(0),T}()
=#
