function defbasics(::Type{ST}, ::Type{BT}) where {ST,BT}
    @eval begin
        Base.@pure Base.values(::$ST{V}) where V = V::$BT
        Base.@pure Base.values(::Type{$ST{V}}) where V = V::$BT

        (::Type{<:$ST})(val::Val{V}) where V = $ST{$BT(V)}()

        Base.eltype(::$ST) = $BT
        Base.eltype(::Type{<:$ST}) = $BT

        Base.typemax(::$ST) = $ST{Base.typemax($BT)}()
        Base.typemax(::Type{$ST}) = $ST{Base.typemax($BT)}()

        Base.typemin(::$ST) = $ST{typemin($BT)}()
        Base.typemin(::Type{$ST}) = $ST{typemin($BT)}()

        promote_rule(::Type{<:$ST}, ::Type{$BT}) = $BT

        (::Type{$BT})(::$ST{X}) where X = X::$BT
        (::Type{<:$ST{<:Any}})(x::$ST) = x
        (::Type{<:$ST{<:Any}})(x::$BT) = $ST{x}()
        (::Type{<:$ST{<:Any}})(x::BaseNumber) = $ST($BT(x))
    end
end

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

        Base.isless(::$ST{V1}, ::$ST{V2}) where {V1,V2} = isles(V1::$BT, V2::$BT)
    end
end

function defint(::Type{ST}, ::Type{BT}) where {ST,BT}
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
    end
end

#=
function defsigned(::Type{ST}, ::Type{BT}) where {ST,BT}
    flipsign(::$ST{X}, ::$ST{Y}) where {X,Y} = flipsign_int(x, y)
end
flipsign(x::T, y::T)

# these require SInt to be set
=#
