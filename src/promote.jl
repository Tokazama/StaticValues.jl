promote_rule(::Type{SInt16}, ::Union{Type{SInt8}, Type{SUInt8}}) = SInt16
promote_rule(::Type{SInt32}, ::Union{Type{SInt16}, Type{SInt8}, Type{SUInt16}, Type{SUInt8}}) = SInt32
promote_rule(::Type{SInt64}, ::Union{Type{SInt16}, Type{SInt32}, Type{SInt8}, Type{SUInt16}, Type{SUInt32}, Type{SUInt8}}) = SInt64
promote_rule(::Type{SInt128}, ::Union{Type{SInt16}, Type{SInt32}, Type{SInt64}, Type{SInt8}, Type{SUInt16}, Type{SUInt32}, Type{SUInt64}, Type{SUInt8}}) = SInt128
promote_rule(::Type{SUInt16}, ::Union{Type{SInt8}, Type{SUInt8}}) = SUInt16
promote_rule(::Type{SUInt32}, ::Union{Type{SInt16}, Type{SInt8}, Type{SUInt16}, Type{SUInt8}}) = SUInt32
promote_rule(::Type{SUInt64}, ::Union{Type{SInt16}, Type{SInt32}, Type{SInt8}, Type{SUInt16}, Type{SUInt32}, Type{SUInt8}}) = SUInt64
promote_rule(::Type{SUInt128}, ::Union{Type{SInt16}, Type{SInt32}, Type{SInt64}, Type{SInt8}, Type{SUInt16}, Type{SUInt32}, Type{SUInt64}, Type{SUInt8}}) = SUInt128

static_tuple = (static_integers..., static_float...)

base_unsigned = (UInt128,UInt16,UInt32,UInt64,UInt8)

base_signed = (Int128,Int16,Int32,Int64,Int8)

base_integers = (base_unsigned..., base_signed...)

base_tuple = (base_integers..., base_float...)

notin_tuple = (Union{SSigned,SUInt64,SUInt32,SUInt16,SUInt8,SFloat},
               Union{SSigned,SUInt128,SUInt32,SUInt16,SUInt8,SFloat},
               Union{SSigned,SUInt128,SUInt64,SUInt16,SUInt8,SFloat},
               Union{SSigned,SUInt128,SUInt64,SUInt32,SUInt8,SFloat},
               Union{SSigned,SUInt128,SUInt64,SUInt32,SUInt16,SFloat},
               Union{SUnsigned,SInt64,SInt32,SInt16,SInt8,SFloat},
               Union{SUnsigned,SInt128,SInt32,SInt16,SInt8,SFloat},
               Union{SUnsigned,SInt128,SInt64,SInt16,SInt8,SFloat},
               Union{SUnsigned,SInt128,SInt64,SInt32,SInt8,SFloat},
               Union{SUnsigned,SInt128,SInt64,SInt32,SInt16,SFloat},
               Union{SInteger,SFloat32,SFloat64},
               Union{SInteger,SFloat32,SFloat16},
               Union{SInteger,SFloat16,SFloat64})
