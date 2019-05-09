seek_static_val(::Type{T}, val::Val) where T =
    error("elements of type $T are not currently supported")

sval(val::Val{V}) where V = seek_static_val(typeof(V), val)

for (ST,BT) in zip(static_tuple, base_tuple)

    # f(static) --> val
    @eval begin
        Base.eltype(::$ST) = $BT
        Base.values(::$ST{V}) where V = V::$BT
        Base.log10(::$ST{V}) where V = $ST{log(V::$BT)/log(10)}()
        Base.isfinite(::$ST{V}) where V = isfinite(V::$BT)
        Base.iszero(::$ST{V}) where V = iszero(V::$BT)

        Base.zero(::$ST) = $ST{zero($BT)}()
        Base.zero(::Type{<:$ST}) = $ST{zero($BT)}()

        Base.one(::$ST) = $ST{one($BT)}()
        Base.one(::Type{<:$ST}) = $ST{one($BT)}()

        Base.fma(::$ST{X}, ::$ST{Y}, ::$ST{Z}) where {X,Y,Z} =
            sval(fma(X::$BT, Y::$BT, Z::$BT))

        Base.muladd(::$ST{X}, ::$ST{Y}, ::$ST{Z}) where {X,Y,Z} =
            sval(muladd(X::$BT, Y::$BT, Z::$BT))

        Base.div(::$ST{X}, ::$ST{Y}) where {X,Y} = $ST{div(X::$BT, Y::$BT)}()

        Base.fld(::$ST{X}, ::$ST{Y}) where {X,Y} = $ST{fld(X::$BT, Y::$BT)}()

        Base.cld(::$ST{X}, ::$ST{Y}) where {X,Y} = $ST{cld(X::$BT, Y::$BT)}()

        Base.rem(::$ST{X}, ::$ST{Y}) where {X,Y} = $ST{rem(X::$BT, Y::$BT)}()

        Base.max(x::$ST{X}, y::$ST{Y}) where {X,Y} = X::$BT > Y::$BT ? x : y

        Base.min(x::$ST{X}, y::$ST{Y}) where {X,Y} = X::$BT > Y::$BT ? y : x

        Base.minmax(x::$ST{X}, y::$ST{Y}) where {X,Y} = X::$BT > Y::$BT ? (y, x) : (x, y)

        (*)(::$ST{V1}, ::$ST{V2}) where {V1,V2} = $ST{(*)(V1::$BT, V2::$BT)}()
        (+)(::$ST{V1}, ::$ST{V2}) where {V1,V2} = $ST{(+)(V1::$BT, V2::$BT)}()
        (-)(::$ST{V1}, ::$ST{V2}) where {V1,V2} = $ST{(-)(V1::$BT, V2::$BT)}()
        (-)(::$ST{V}) where V = $ST{-V::$BT}()


        # TODO: figure out return type inference for these (if possible)
        (\)(::$ST{V1}, ::$ST{V2}) where {V1,V2} = sval((\)(V1::$BT, V2::$BT))
        (^)(::$ST{V1}, ::$ST{V2}) where {V1,V2} = sval((^)(V1::$BT, V2::$BT))
        Base.mod(::$ST{X}, ::$ST{Y}) where {X,Y} = sval(mod(X::$BT, Y::$BT))
        Base.mod1(::$ST{X}, ::$ST{Y}) where {X,Y} = sval(mod1(X::$BT, Y::$BT))
        Base.mod1(::$ST{X}, ::$ST{Y}) where {X,Y} = sval(mod1(X::$BT, Y::$BT))
        Base.fld1(::$ST{X}, ::$ST{Y}) where {X,Y} = sval(fld1(X::$BT, Y::$BT))


        Base.add12(x, y) = add12(promote(x, y)...)


        (::Type{<:$ST})(val::Val{V}) where V = convert_static_val($BT, typeof(V), val)
        seek_static_val(::Type{$BT}, val::Val{V}) where V = $ST{$BT}()
    end

    # f(static) --> Bool
    for f in (:(==), :<, :<=, :(!=), :isless)
        @eval begin
            $f(::$ST{V1}, ::$ST{V2}) where {V1,V2} = V1::$BT === V2::$BT
        end
    end

    # f(static, static) --> static
    for f in (:*, :^, :\, :+, :-)
        @eval begin
            $f(::$ST{V1}, ::$ST{V2}) where {V1,V2} = $ST{$f(V1::$BT, V2::$BT)}()
        end
    end

    for (ST2,BT2) in zip(static_tuple, base_tuple)
        if BT == BT2
            @eval begin
                (::Type{<:$ST{<:Any}})(val::$ST2) = val
                (::Type{<:$ST{<:Any}})(val::$BT2) = $ST{val}()

                Base.promote_rule(::Type{<:$ST}, ::Type{$BT2}) = $BT
                Base.flipsign(::$ST{V1}, ::$ST2{V2}) where {V1,V2} = flipsign(V1::$BT,V2::$BT2)

                (::Type{$BT2})(::$ST{V}) where V = V::$BT
                convert_static_val(::Type{$BT}, ::Type{$BT2}, val::Val{V}) where V = $ST{V::$BT2}()
                convert_static_val(::Type{$BT}, ::$ST2{V}) where V = $ST{V::$BT2}()
            end
        else
            @eval begin
                (::Type{<:$ST{<:Any}})(::$ST2{V}) where V = $ST{$BT(V::$BT2)}()
                (::Type{<:$ST{<:Any}})(val::$BT2) = $ST{$BT(val)}()

                Base.promote_rule(::Type{<:$ST}, ::Type{$BT2}) = promote_type($BT, $BT2)
                Base.flipsign(::$ST{V1}, ::$ST2{V2}) where {V1,V2} = flipsign(V1::$BT,V2::$BT2)


                (::Type{$BT2})(::$ST{V}) where V = $BT2(V::$BT)

                # Given Val of different type convert to SVal
                convert_static_val(::Type{$BT}, ::Type{$BT2}, val::Val{V}) where V = $ST{$BT(V::$BT2)}()
                convert_static_val(::Type{$BT}, ::$ST2{V}) where V = $ST{$BT(V::$BT2)}()
            end
        end
    end

    # only iterate over <:Integer
    for (ST2,BT2) in zip(static_integers, base_integers)
        eval(:(Base.round(::Type{$BT2}, ::$ST{V}) where V = $ST2{round($BT2, V::$BT)}()))
    end

end

for ST in static_tuple
    @eval begin
        function Base.add12(x::$ST, y::$ST)
            x, y = ifelse(abs(y) > abs(x), (y, x), (x, y))
            Base.canonicalize2(x, y)
        end
    end
end
