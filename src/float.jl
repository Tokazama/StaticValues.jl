import Base: sin, cos, sincos, tan, sinh, cosh, tanh, asin, acos, atan, asinh, acosh,
             atanh, sec, csc, cot, asec, acsc, acot, sech, csch, coth, asech, acsch, acoth,
             sinpi, cospi, sinc, cosc, cosd, cotd, cscd, secd, sind, tand, acosd, acotd,
             acscd, asecd, asind, atand,
             cbrt,
             log, log2, log10, log1p,
             exp, frexp, expm1, exp2, exponent

import Base: prevfloat, floatmax, floatmin, maxintfloat, significand

float_one2one = (:sin, :cos, :tan, :sinh, :cosh, :tanh, :asin, :acos, :atan, :asinh,
                 :acosh, :atanh, :sec, :csc, :cot, :asec, :acsc, :acot, :sech, :csch,
                 :coth, :asech, :acsch, :acoth, :sinpi, :cospi, :sinc, :cosc, :cosd, :cotd,
                 :cscd, :secd, :sind, :tand, :acosd, :acotd, :acscd, :asecd, :asind,
                 :atand, :cbrt, :exp, :frexp, :expm1, :exp2, :significand)

function deffloat(::Type{ST}, ::Type{BT}) where {ST,BT<:BaseFloat}
    for f in float_one2one
        @eval begin
            $(f)(::$ST{X}) where X = $ST{$(f)(X::$BT)}()
        end
    end

    @eval begin
        Base.rem2pi(::$ST{X}, r::RoundingMode) where X = $ST{rem2pi(X::$BT, r)}()

        @generated function Base.sqrt(::$ST{X}) where X
            x = $ST{sqrt(X::$BT)::$BT}()
            :($x)
        end
#=
        @generated function Base.rat(::$ST{X}) where X
            n, d = Base.rat(X::$BT)
            :($(SInt{n}()), $(SInt{d}()))
        end
=#
        @generated function log(::$ST{X}) where X
            x = $ST{log(X::$BT)::$BT}()
            :($x)
        end

        @generated function log2(::$ST{X}) where X
            x = $ST{log2(X::$BT)::$BT}()
            :($x)
        end

        @generated function log10(::$ST{X}) where X
            x = $ST{log10(X::$BT)::$BT}()
            :($x)
        end

        @generated function log1p(::$ST{X}) where X
            x = $ST{log1p(X::$BT)::$BT}()
            :($x)
        end

        exponent(::$ST{X}) where X = SInt{exponent(X::$BT)}()

        function sincos(::$ST{V}) where V
            x, y = sincos(V::$BT)
            return $ST{x}(), $ST{y}()
        end

        maxintfloat(::Type{<:$ST}) = $ST{maxintfloat($BT)}()
        maxintfloat(::$ST{X}) where X = $ST{maxintfloat(X::$BT)}()


        (/)(::$ST{V1}, ::$ST{V2}) where {V1,V2} = $ST{(/)(V1::$BT, V2::$BT)}()

        Base.fma(::$ST{V1}, ::$ST{V2}, ::$ST{V3}) where {V1,V2,V3} = $ST{fma(V1::$BT, V2::$BT, V3::$BT)}()


        prevfloat(x::$ST{V}) where V = $ST{prevfloat(V::$BT)}()
        prevfloat(x::$ST{V}, n::Integer) where V = $ST{prevfloat(V::$BT, n)}()

        floatmax(x::$ST) = $ST{floatmax($BT)}()
        floatmax(::Type{$ST}) = $ST{floatmax($BT)}()

        floatmin(x::$ST) = $ST{floatmin($BT)}()
        floatmin(::Type{$ST}) = $ST{floatmin($BT)}()

        function Base.mul12(x::$ST, y::$ST)
            Base.@_inline_meta
            h = x * y
            ifelse(iszero(h) | !isfinite(h), (h, h), Base.canonicalize2(h, fma(x, y, -h)))
        end

        function Base.div12(x::$ST, y::$ST)
            xs, xe = frexp(x)
            ys, ye = frexp(y)
            r = xs / ys
            rh, rl = canonicalize2(r, -fma(r, ys, -xs)/ys)
            ifelse(iszero(r) | !isfinite(r), (r, r), (ldexp(rh, xe-ye), ldexp(rl, xe-ye)))
        end
    end
end



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

SFloat(x::Float16) = SFloat16{x}()
SFloat(x::Float32) = SFloat32{x}()
SFloat(x::Float64) = SFloat64{x}()

function SFloat(val::Val{V}) where V
    if V isa Float16
        SFloat16{V}()
    elseif V isa Float32
        SFloat32{V}()
    else
        SFloat64{V}()
    end
end

Base.show(io::IO, v::SFloat) = print(io, "SFloat($(values(v)))")

const AbstractFloat64 = Union{SFloat64,Float64}
const AbstractFloat32 = Union{SFloat32,Float32}
const AbstractFloat16 = Union{SFloat16,Float16}


SF2BF = Dict(SFloat64 => Float64,
             SFloat32 => Float32,
             SFloat16 => Float16)

BF2SF = Dict(Float64 => SFloat64,
             Float32 => SFloat32,
             Float16 => SFloat16)

for (S,B) in SF2BF
    defbasics(S,B)
    defbool(S,B)
    deffloat(S,B)
end

SFloat64One, SFloat64Zero = defmath(SFloat64, Float64)
SFloat32One, SFloat32Zero = defmath(SFloat32, Float32)
SFloat16One, SFloat16Zero = defmath(SFloat16, Float16)

function Base.decompose(::SFloat{X}) where X
    s, e, d = Base.decompose(X)
    return SInt{s}(), SInt{e}(), SInt{d}()
end

for (S,B) in SI2BI
    @eval begin
        Base.trunc(::Type{$B}, x::SFloat{X}) where X = $S{trunc($B, X)}()
    end
end

Base.trunc(::Type{Integer}, x::SFloat{X}) where X = SInt{trunc(Integer, X)}()

