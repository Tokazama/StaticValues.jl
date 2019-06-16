const SReal{V} = Union{SInteger{V},SFloat{V},SRational{V}}

SReal(x::BaseInteger) = SInteger(x)
SReal(x::BaseFloat) = SFloat(x)
SReal(x::Rational) = SRational(x)

Base.promote_eltype(x::SReal, y::BaseNumber) = promote_type(eltype(x), eltype(y))
Base.promote_eltype(x::BaseNumber, y::SReal) = promote_type(eltype(x), eltype(y))
Base.promote_eltype(x::SReal, y::SReal) = promote_type(eltype(x), eltype(y))

function SReal(::Val{V}) where V
    if V isa Integer
        if V isa Unsigned
            if V isa UInt8
                SUInt8{V}()
            elseif V isa UInt16
                SUInt16{V}()
            elseif V isa UInt32
                SUInt32{V}()
            elseif V isa UInt128
                SUInt128{V}()
            else
                SUInt64{V}()
            end
        elseif V isa Bool
            SBool{V}()
        elseif V isa Signed
            if V isa Int8
                SInt8{V}()
            elseif V isa Int16
                SInt16{V}()
            elseif V isa Int32
                SInt32{V}()
            elseif V isa Int128
                SInt128{V}()
            else
                SInt64{V}()
            end
        end
    elseif V isa AbstractFloat
        if V isa Float16
            SFloat16{V}()
        elseif V isa Float32
            SFloat32{V}()
        else
            SFloat64{V}()
        end
    elseif V isa Rational
        SRational(val)
    end
end

S2B = Dict(SUInt128 => UInt128,
           SUInt16 => UInt16,
           SUInt32 => UInt32,
           SUInt64 => UInt64,
           SUInt8 => UInt8,

           SInt128 => Int128,
           SInt16 => Int16,
           SInt32 => Int32,
           SInt64 => Int64,
           SInt8 => Int8,

           SFloat64 => Float64,
           SFloat32 => Float32,
           SFloat16 => Float16)

B2S = Dict(UInt128 => SUInt128,
           UInt16 => SUInt16,
           UInt32 => SUInt32,
           UInt64 => SUInt64,
           UInt8 => SUInt8,

           Int128 => SInt128,
           Int16 => SInt16,
           Int32 => SInt32,
           Int64 => SInt64,
           Int8 => SInt8,

           Float64 => SFloat64,
           Float32 => SFloat32,
           Float16 => SFloat16)
