#static_unsigned = (SUInt128,SUInt16,SUInt32,SUInt64,SUInt8)

#static_signed = (SInt128,SInt16,SInt32,SInt64,SInt8)

#static_integer = (static_unsigned..., static_signed...)


const SNumber{V} = Union{SComplex{V},SReal{V}}

SNumber(x::BaseReal) = SReal(x)
SNumber(x::Complex) = SComplex(x)

function SNumber(val::Val{V}) where V
    if V isa Real
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
    elseif V isa Complex
        SComplex(val)
    end
end

"""
    TPVal{H,L}
"""
struct TPVal{H,L}
    function TPVal{H,L}() where {H,L}
        if eltype(H) == eltype(L)
            return new{H,L}()
        else
            error("high and low precision values must be of the same type but got,
                  $(eltype(H)) and $(eltype(L)).")
        end
    end
end


const SVal{V} = Union{SNumber{V},SChar{V},TPVal{V}}

SVal(x::BaseNumber) = SNumber(x)
SVal(x::AbstractChar) = SChar(x)
SVal(x::TwicePrecision) = TPVal(x)

function SVal(val::Val{V}) where V
    if V isa Number
        SNumber(val)
    elseif V isa AbstractChar
        SChar(val)
    elseif V isa TwicePrecision
        TPVal(val)
    end
end


# static to base dict


for (SIT,BIT) in SI2BI
    BFT = float(BIT)
    SFT = BF2SF[BFT]
    @eval begin
        Base.AbstractFloat(::$SIT{X}) where X = $SFT{$BFT(X::$BIT)}()
    end
end

Base.AbstractFloat(::SBool{X}) where X = SFloat64{Float64(X::Bool)}()

for (SI,BI) in SSI2BSI
    BUI = unsigned(BI)
    SUI = BUI2SUI[BUI]
    @eval begin
        Base.unsigned(::$SI{X}) where X = $SUI{Base.bitcast($BUI, X::$BI)::$BUI}()
        Base.unsigned(::Type{<:$SI{X}}) where X = $SUI

        Base.signed(::$SUI{X}) where X = $SI{$BI(X::$BUI)::$BI}()
        Base.signed(::Type{<:$SUI{X}}) where X = $SI
    end
end

#Base.signed(x::Unsigned) = reinterpret(typeof(convert(Signed, zero(x))), x


#Base.signed(x::Unsigned) = reinterpret(typeof(convert(Signed, zero(x))), x

for (ST1,BT1) in S2B
    for (ST2,BT2) in S2B
        if BT1 != BT2
            BASERULE = promote_rule(BT1, BT2)
            if BASERULE != Union{}
                NEWRULE = B2S[BASERULE]
                @eval begin
                    promote_rule(::Type{<:$ST1{<:Any}}, ::Type{$BT2}) = $BASERULE
                    promote_rule(::Type{<:$ST2{<:Any}}, ::Type{$BT1}) = $BASERULE
                    promote_rule(::Type{<:$ST1{<:Any}}, ::Type{<:$ST2}) = $NEWRULE
                end
            end
        end
    end
end

promote_rule(::Type{<:AbstractIrrational}, ::Type{SFloat16}) = SFloat16
promote_rule(::Type{<:AbstractIrrational}, ::Type{SFloat32}) = SFloat32
promote_rule(::Type{<:AbstractIrrational}, ::Type{T}) where {T<:SReal} = promote_type(SFloat64, T)

for (ST1,BT1) in S2B
    for (ST2,BT2) in S2B
        if BT1 != BT2
            @eval begin
                @generated function (::Type{<:$ST2{<:Any}})(::$ST1{X}) where X
                    ret = $ST2{$BT2(X::$BT1)}()
                    :($ret)
                end
                (::Type{$BT2})(::$ST1{X}) where X = $BT2(X::$BT1)
                ofeltype(::Type{$BT1}, val::$ST2{V}) where V = $ST1{$BT1(V::$BT2)}()

                ofeltype(::Type{$BT1}, val::$BT2) = $BT1(val)
                ofeltype(::$BT1, val::$ST2{V}) where V = $ST1{$BT1(V::$BT2)}()
                ofeltype(::$BT1, val::$BT2) = $BT1(val)

                ofeltype(::Type{<:$ST1}, val::$ST2{V}) where V = $ST1{$BT1(V::$BT2)}()
                ofeltype(::Type{<:$ST1}, val::$BT2) = $BT1(val)
                ofeltype(::$ST1, val::$ST2{V}) where V = $ST1{$BT1(V::$BT2)}()
                ofeltype(::$ST1, val::$BT2) = $BT1(val)

                Base.flipsign(::$ST1{V1}, ::$ST2{V2}) where {V1,V2} = $ST1{flipsign(V1::$BT1,V2::$BT2)}()
                Base.copysign(::$ST1{V1}, ::$ST2{V2}) where {V1,V2} = $ST1{copysign(V1::$BT1,V2::$BT2)}()
            end
        end
    end
end


