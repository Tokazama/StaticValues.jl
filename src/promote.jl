
static_real = (static_integers..., static_float...)
const SReal{V} = Union{SInteger{V},SFloat{V}}

base_real = (base_integers..., base_float...)
const BaseReal = Union{base_real...}

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
