const BitSSigned32{V}      = Union{SInt8{V},SInt16{V},SInt32{V}}
const BitSUnsigned32{V}    = Union{SUInt8{V},SUInt16{V},SUInt32{V}}
const BitSInteger32{V}     = Union{BitSSigned32{V},BitSUnsigned32{V}}

const BitSSigned64{V}      = Union{BitSSigned32{V},SInt{V}}
const BitSUnsigned64{V}    = Union{BitSUnsigned32{V},SUInt{V}}
const BitSInteger64{V}     = Union{BitSSigned64{V},BitSUnsigned64{V}}

const BitSSigned{V}        = Union{BitSSigned64{V},SInt128{V}}
const BitSUnsigned{V}      = Union{BitSUnsigned64{V},SUInt128{V}}
const BitSInteger{V}       = Union{BitSSigned{V},BitSUnsigned{V}}

if Int === Int64
    const BitSSignedSmall{V} = Union{SInt8{V},SInt16{V},SInt32{V}}
else
    const BitSSignedSmall{V} = Union{SInt8{V},SInt16{V}}
end

if UInt === UInt64
    const BitSUnsignedSmall{V} = Union{SUInt8{V},SUInt16{V},SUInt32{V}}
else
    const BitSUnsignedSmall{V} = Union{SUInt8{V},SUInt16{V}}
end
const BitSIntegerSmall{V}  = Union{BitSSignedSmall{V},BitSUnsignedSmall{V}}

const BitSSigned64T{V} = Union{Type{SInt8{V}},Type{SInt16{V}},Type{SInt32{V}},Type{SInt64{V}}}
const BitSUnsigned64T{V} = Union{Type{SUInt8{V}},Type{SUInt16{V}},Type{SUInt32{V}},Type{SUInt64{V}}}
const BitIntegerT{V} = Union{Type{SInt8{V}},Type{SInt16{V}},Type{SInt32{V}},Type{SInt64{V}},
                             Type{SUInt8{V}},Type{SUInt16{V}},Type{SUInt32{V}},Type{SUInt64{V}}}
    

    >>(::$ST{V1}, ::$ST{V2}) where {V1,V2} = $ST{>>(V1::$BT, V2::$BT)}()
    <<(::$ST{V1}, ::$ST{V2}) where {V1,V2} = $ST{<<(V1::$BT, V2::$BT)}()
    >>>(::$ST{V1}, ::$ST{V2}) where {V1,V2} = $ST{>>>(V1::$BT, V2::$BT)}()

for ST in static_integer
    @eval begin
        
        (*)(::$ST{V1}, ::$ST{V2}) where {V1,V2} = $ST{(*)(V1::$BT, V2::$BT)}()
        (+)(::$ST{V1}, ::$ST{V2}) where {V1,V2} = $ST{(+)(V1::$BT, V2::$BT)}()
        (-)(::$ST{V1}, ::$ST{V2}) where {V1,V2} = $ST{(-)(V1::$BT, V2::$BT)}()
        (-)(::$ST{V}) where V = $ST{-V::$BT}()
        (|)(::$ST{V1}, ::$ST{V2}) where {V1,V2} = $ST{(|)(V1::$BT, V2::$BT)}()
        (&)(::$ST{V1}, ::$ST{V2}) where {V1,V2} = $ST{(&)(V1::$BT, V2::$BT)}()
        Base.xor(::$ST{V1}, ::$ST{V2}) where {V1,V2} = $ST{(|)(V1::$BT, V2::$BT)}()
    end
end
function Base.fld(x::SInteger, y::SInteger)
    d = div(x, y)
    return d - (signbit(x ⊻ y) & (d * y != x))
end

function cld(x::SInteger, y::SInteger)
    d = div(x, y)
    return d + (((x > 0) == (y > 0)) & (d * y != x))
end



Base.fld(x::SSigned, y::SUnsigned) = div(x, y) - (signbit(x) & (rem(x, y) != 0))
Base.fld(x::SUnsigned, y::SSigned) = div(x, y) - (signbit(y) & (rem(x, y) != 0))

for ST in static_unsigned
    @eval begin
        Base.fld(x::$ST, y::$ST) = div(x,y)
        function cld(x::$ST, y::$ST) where T
            d = div(x, y)
            return d + (d * y != x)
        end
    end
end

Base.mod(x::BitSSigned, y::SUnsigned) = rem(y + unsigned(rem(x, y)), y)
Base.mod(x::SUnsigned, y::SSigned) = rem(y + signed(rem(x, y)), y)
Base.mod(x::SUnsigned, y::SUnsigned) = rem(x, y)


Base.cld(x::SSigned, y::SUnsigned) = div(x, y) + (!signbit(x) & (rem(x, y) != 0))
Base.cld(x::SUnsigned, y::SSigned) = div(x, y) + (!signbit(y) & (rem(x, y) != 0))
