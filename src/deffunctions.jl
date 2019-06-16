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

function defmath(::Type{ST}, ::Type{BT}) where {ST,BT}
    STOne = ST(BT(1))
    STOneType = typeof(STOne)
    STZero = ST(BT(0))
    STZeroType = typeof(STZero)

    @eval begin
        Base.:(-)(::$ST{V}) where V = $ST{-V::$BT}()
        Base.:(-)(x::$ST{V1}, y::$ST{V2}) where {V1,V2} = $ST{(-)(V1::$BT, V2::$BT)}()
        Base.:(+)(x::$ST{V1}, y::$ST{V2}) where {V1,V2} = $ST{(+)(V1::$BT, V2::$BT)}()
        Base.:(*)(x::$ST{V1}, y::$ST{V2}) where {V1,V2} = $ST{(*)(V1::$BT, V2::$BT)}()

        Base.div(x::$ST{V1}, y::$ST{V2}) where {V1,V2} = $ST{div(V1::$BT, V2::$BT)}()
        Base.rem(x::$ST{V1}, y::$ST{V2}) where {V1,V2} = $ST{rem(V1::$BT, V2::$BT)}()
        Base.mod(x::$ST{V1}, y::$ST{V2}) where {V1,V2} = $ST{mod(V1::$BT, V2::$BT)}()
        Base.cld(x::$ST{V1}, y::$ST{V2}) where {V1,V2} = $ST{cld(V1::$BT, V2::$BT)}()
        Base.fld(x::$ST{V1}, y::$ST{V2}) where {V1,V2} = $ST{fld(V1::$BT, V2::$BT)}()

        function add12(x::$ST, y::$ST)
            x, y = ifelse(abs(y) > abs(x), (y, x), (x, y))
            Base.canonicalize2(x, y)
        end
        function Base.muladd(x::$ST{X}, y::$ST{Y}, z::$ST{Z}) where {X,Y,Z}
            $ST{muladd(X::$BT,Y::$BT,Z::$BT)}()
        end

        Base.max(x::$ST{X}, y::$ST{Y}) where {X,Y} = X::$BT > Y::$BT ? x : y
        Base.min(x::$ST{X}, y::$ST{Y}) where {X,Y} = X::$BT > Y::$BT ? y : x
        Base.minmax(x::$ST{X}, y::$ST{Y}) where {X,Y} = X::$BT > Y::$BT ? (y, x) : (x, y)

        @pure Base.one(::$ST) = $STOne
        @pure Base.one(::Type{<:$ST}) = $STOne 
        @pure Base.oneunit(::$ST) = $STOne
        @pure Base.oneunit(::Type{<:$ST}) = $STOne

        @pure Base.isone(::$STOneType) = true
        @pure Base.isone(::$ST{T}) where T = false

        @pure Base.zero(::$ST) = $STZero
        @pure Base.zero(::Type{<:$ST}) = $STZero

        @pure Base.iszero(::$STZeroType) = true
        @pure Base.iszero(::$ST{T}) where T = false
    end

    return STOne, STZero
end

function defbool(::Type{ST}, ::Type{BT}) where {ST,BT}
    @eval begin
        @pure Base.:(==)(::$ST{V}, ::$ST{V}) where V = true
        @pure Base.:(==)(::$ST{V1}, ::$ST{V2}) where {V1,V2} = false

        @pure Base.:(!=)(::$ST{V}, ::$ST{V}) where V = false
        @pure Base.:(!=)(::$ST{V1}, ::$ST{V2}) where {V1,V2} = true

        Base.:(>)(::$ST{V1}, ::$ST{V2}) where {V1,V2} = V1::$BT > V2::$BT
        Base.:(>=)(::$ST{V1}, ::$ST{V2}) where {V1,V2} = V1::$BT >= V2::$BT

        Base.:(<)(::$ST{V1}, ::$ST{V2}) where {V1,V2} = V1::$BT < V2::$BT
        Base.:(<=)(::$ST{V1}, ::$ST{V2}) where {V1,V2} = V1::$BT <= V2::$BT

        Base.round(::$ST{X}, r::RoundingMode) where X = $ST{round(X::$BT, r)}()
        Base.isless(::$ST{V1}, ::$ST{V2}) where {V1,V2} = isles(V1::$BT, V2::$BT)
    end
end

function defint(::Type{ST}, ::Type{BT}) where {ST,BT<:BaseInteger}
    @eval begin
        Base.:(~)(::$ST{X}) where X = $ST{~X}()
        Base.:(|)(::$ST{X}, ::$ST{Y}) where {X,Y} = $ST{(|)(X::$BT, Y::$BT)}()
        Base.xor(::$ST{X}, ::$ST{Y}) where {X,Y} = $ST{xor(X::$BT, Y::$BT)}()
        Base.:(&)(::$ST{X}, ::$ST{Y}) where {X,Y} = $ST{(&)(X::$BT, Y::$BT)}()

        Base.trailing_zeros(::$ST{X}) where X = SInt{trailing_zeros(X::$BT)}()
        Base.trailing_ones(::$ST{X}) where X = SInt{trailing_ones(X::$BT)}()
        Base.count_ones(::$ST{X}) where X = SInt{count_ones(x::$BT)}()
        Base.leading_zeros(::$ST{X}) where X = SInt{leading_zeros(X::$BT)}()
        Base.leading_ones(::$ST{X}) where X = SInt{leading_ones(X::$BT)}()

        Base.flipsign(::$ST{X}, ::$ST{Y}) where {X,Y} = $ST{flipsign(X::$BT, Y::$BT)}()
        Base.flipsign(::$ST{X}, ::SSigned{Y}) where {X,Y}= $ST{flipsign(X::$BT, Y)}()

        Base.rem(x::$ST, ::Type{<:$ST{<:Any}}) = x

        Base.powermod(::$ST{X}, ::$ST{P}, ::$ST{M}) where {X,P,M} = $ST{powermod(X::$BT, P::$BT, M::$BT)}()

        @generated function Base.binomial(::$ST{N}, ::$ST{K}) where {N,K}
            x = $ST{binomial(N::$BT, K::$BT)}()
            :($x)
        end

        @generated function Base.factorial(n::$ST{N}) where N
            x = $ST{factorial(N::$BT)}()
            :($x)
        end
        @generated function Base.sqrt(::$ST{X}) where X
            x = SFloat64{sqrt(X::$BT)}()
            :($x)
        end
    end
end

function deffloat(::Type{ST}, ::Type{BT}) where {ST,BT<:BaseFloat}
    for f in float_one2one
        @eval begin
            $(f)(::$ST{X}) where X = $ST{$(f)(X::$BT)}()
        end
    end

    @eval begin
        @generated function Base.sqrt(::$ST{X}) where X
            x = $ST{sqrt(X::$BT)::$BT}()
            :($x)
        end

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

        maxintfloat(::Type{<:$ST{X}}) where X = $ST{maxintfloat($BT)}()

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

#=
function defsigned(::Type{ST}, ::Type{BT}) where {ST,BT}
    flipsign(::$ST{X}, ::$ST{Y}) where {X,Y} = flipsign_int(x, y)
end
flipsign(x::T, y::T)

# these require SInt to be set
=#
