import Base: intersect, reverse


function getindex(r::StaticUnitRange, s::Union{StepRange{<:Integer},StaticStepRange{<:Integer}})
    Base.@_inline_meta
    @boundscheck checkbounds(r, s)
    st = ofeltype(first(r), first(r) + first(s)-SOne)
    range(st, step=step(s), length=length(s))
end

function getindex(r::StaticStepRange, s::AbstractRange{<:Integer})
    Base.@_inline_meta
    @boundscheck checkbounds(r, s)
    st = oftype(first(r), first(r) + (first(s)-SOne)*step(r))
    range(st, step=step(r)*step(s), length=length(s))
end

function getindex(r::StaticStepRangeLen{T},
                  s::Union{OrdinalRange{<:Integer},StaticOrdinalRange{<:Integer}}) where {T}
    Base.@_inline_meta
    @boundscheck checkbounds(r, s)
    # Find closest approach to offset by s
    ind = LinearIndices(s)
    f = max(min(SOne + round(Int, (offset(r) - first(s))/step(s)), last(ind)), first(ind))
    ref = _getindex_hiprec(r, first(s) + (f-SOne)*step(s))
    return StaticStepRangeLen{T}(ref, step(r)*step(s), length(s), f)
end

function getindex(
    r::StaticLinRange,
    s::Union{OrdinalRange{<:Integer},StaticOrdinalRange{<:Integer}})
    Base.@_inline_meta
    @boundscheck checkbounds(r, s)
    return StaticLinRange(unsafe_getindex(r, first(s)), vlast,
                          unsafe_getindex(r, last(s)))
end

function getindex(r::Union{StaticStepRangeLen,StaticLinRange}, i::Integer)
    Base.@_inline_meta
    @boundscheck checkbounds(r, i)
    unsafe_getindex(r, i)
end

# TODO ensure this computes the right thing on LinMRange
unsafe_getindex(r::StaticLinRange{T}, i::I) where {T,I<:Integer} =
    lerpi(i-one(i), lendiv(r), first(r), last(r))

function lerpi(j::Integer, d::Integer, a::A, b::B) where {A,B}
    Base.@_inline_meta
    t = j/d
    ofeltype(a, (one(t) - t) * a + t * b)
end

@inline function unsafe_getindex(
    r::StaticStepRangeLen{T,B}, i::Integer) where {T,B<:Union{<:TPVal,TwicePrecision}}
    Base.@_inline_meta
    u = i - offset(r)
    shift_hi, shift_lo = u*getstephi(r), u*getsteplo(r)
    x_hi, x_lo = add12(getrefhi(r), shift_hi)
    ofeltype(T, x_hi + (x_lo + (shift_lo + getreflo(r))))
end

@inline function unsafe_getindex(r::StaticStepRangeLen{T,B}, i::Integer) where {T,B}
    Base.@_inline_meta
    ofeltype(T, reference(r) + (i - offset(r)) * step(r))
end

# TODO: just for ordinal range in base, make sure it doesn't do anything crazy
Base.iterate(r::StaticRange) = isempty(r) ? nothing : (first(r), first(r))

function Base.iterate(r::StaticRange{T}, i::I) where {T,I}
    Base.@_inline_meta
    i == last(r) && return nothing
    next = ofeltype(T, i + step(r))
    (next, next)
end

reverse(r::StaticRange) = (:)(last(r), -step(r), first(r))
function reverse(r::StaticStepRangeLen)
    # If `r` is empty, `length(r) - r.offset + 1 will be nonpositive hence
    # invalid. As `reverse(r)` is also empty, any offset would work so we keep
    # `r.offset`
    offset = isempty(r) ? offset(r) : length(r)-offset(r)+One
    StaticStepRangeLen(first(r), -step(r), length(r), offset)
end

reverse(r::StaticLinRange) = StaticLinRange(last(r), first(r), length(r))

function in(x::Real, r::StaticRange{T}) where {T<:Integer}
    isa(x, Integer) && !isempty(r) && x >= minimum(r) && x <= maximum(r) &&
        (mod(ofeltype(T,x), step(r)) - mod(first(r), step(r)) == 0)
end

function in(x::AbstractChar, r::StaticRange{<:AbstractChar})
    !isempty(r) && x >= minimum(r) && x <= maximum(r) &&
        (mod(Int(x) - Int(first(r)), step(r)) == 0)
end

in(x::Integer, r::StaticUnitRange{<:Integer}) = (first(r) <= x) & (x <= last(r))

function in(x, r::StaticRange)
    anymissing = false
    for y in itr
        v = (y == x)
        if ismissing(v)
            anymissing = true
        elseif v
            return true
        end
    end
    return anymissing ? missing : false
end

intersect(r::StaticUnitRange{<:Integer}, s::StaticUnitRange{<:Integer}) =
    max(first(r),first(s)):min(last(r),last(s))

intersect(i::Integer, r::StaticUnitRange{<:Integer}) =
    i < first(r) ? (first(r):i) :
    i > last(r) ? (i:last(r)) : (i:i)

intersect(r::StaticUnitRange{<:Integer}, i::Integer) = intersect(i, r)


function intersect(r::StaticUnitRange{<:Integer},
                   s::Union{StaticStepRange{<:Integer},StepRange{<:Integer}})
    if isempty(s)
        range(first(r), length=0)
    elseif step(s) == 0
        intersect(first(s), r)
    elseif step(s) < 0
        intersect(r, reverse(s))
    else
        sta = first(s)
        ste = step(s)
        sto = last(s)
        lo = first(r)
        hi = last(r)
        i0 = max(sta, lo + mod(sta - lo, ste))
        i1 = min(sto, hi - mod(hi - sta, ste))
        i0:ste:i1
    end
end

function intersect(r::StaticStepRange{<:Integer}, s::Union{AbstractUnitRange{<:Integer},<:StaticUnitRange{<:Integer}})
    if step(r) < 0
        reverse(intersect(s, reverse(r)))
    else
        intersect(s, r)
    end
end

function intersect(r::StaticStepRange, s::Union{<:StepRange,<:StaticStepRange})
    if isempty(r) || isempty(s)
        return range(first(r), step=step(r), length=0)
    elseif step(s) < 0
        return intersect(r, reverse(s))
    elseif step(r) < 0
        return reverse(intersect(reverse(r), s))
    end

    start1 = first(r)
    step1 = step(r)
    stop1 = last(r)
    start2 = first(s)
    step2 = step(s)
    stop2 = last(s)
    a = lcm(step1, step2)

    # if a == 0
    #     # One or both ranges have step 0.
    #     if step1 == 0 && step2 == 0
    #         return start1 == start2 ? r : AbstractRange(start1, 0, 0)
    #     elseif step1 == 0
    #         return start2 <= start1 <= stop2 && rem(start1 - start2, step2) == 0 ? r : AbstractRange(start1, 0, 0)
    #     else
    #         return start1 <= start2 <= stop1 && rem(start2 - start1, step1) == 0 ? (start2:step1:start2) : AbstractRange(start1, step1, 0)
    #     end
    # end

    g, x, y = gcdx(step1, step2)

    if rem(start1 - start2, g) != 0
        # Unaligned, no overlap possible.
        return range(start1, step=a, length=SZero)
    end

    z = div(start1 - start2, g)
    b = start1 - x * z * step1
    # Possible points of the intersection of r and s are
    # ..., b-2a, b-a, b, b+a, b+2a, ...
    # Determine where in the sequence to start and stop.
    m = max(start1 + mod(b - start1, a), start2 + mod(b - start2, a))
    n = min(stop1 - mod(stop1 - b, a), stop2 - mod(stop2 - b, a))
    m:a:n
end

Base.map(::Type{T}, r::StaticStepRange) where T =
    ofeltype(T, first(r)):ofeltype(T, setp(r)):ofeltype(T, last(r))
Base.map(::Type{T}, r::StaticUnitRange) where T =
    ofeltype(T, first(r)):ofeltype(T, last(r))
Base.map(::Type{T}, r::StaticStepRangeLen) where T =
    StaticStepRangeLen(ofeltype(T, first(r)), ofeltype(T, step(r)), length(r), offset(r))
Base.map(::Type{T}, r::StaticLinRange) where T =
    StaticLinRange(ofeltype(T, first(r)), ofeltype(T, last(r)), length(r))

Base.minimum(r::StaticRange) = min(first(r), last(r)) 
Base.maximum(r::StaticRange) = max(first(r), last(r)) 

Base.extrema(r::StaticRange) = (minimum(r), maximum(r))

function Base.minimum(r::StaticUnitRange)
    Base.@_inline_meta
    isempty(r) ? throw(ArgumentError("range must be non-empty")) : first(r)
end

function maximum(r::StaticUnitRange)
    Base.@_inline_meta
    isempty(r) ? throw(ArgumentError("range must be non-empty")) : last(r)
end

minimum(r::AbstractRange)  = isempty(r) ? throw(ArgumentError("range must be non-empty")) : min(first(r), last(r))
maximum(r::AbstractRange)  = isempty(r) ? throw(ArgumentError("range must be non-empty")) : max(first(r), last(r))

-(r1::StaticStepRangeLen, r2::StaticStepRangeLen) = +(r1, -r2)
-(r::StaticStepRange) = range(-first(r), step=-step(r), length=length(r))

-(r::StaticStepRangeLen) = StaticStepRangeLen(-first(r), -step(r), length(r), offset(r))
-(r::StaticLinRange) = StaticLinRange(-first(r), -last(r), length(r))

+(r1::StaticStepRangeLen{T,B1,S1,E1,L1}, r2::StaticStepRangeLen{T,B2,S2,E2,L2}) where {T,B1,S1,E1,L1<:SInteger,B2,S2,E2,L2<:SInteger} =
    throw(DimensionMismatch("argument dimensions must match"))
function +(r1::StaticStepRangeLen{T,B1,S1,E1,L}, r2::StaticStepRangeLen{T,B2,S2,E2,L}) where {T,B1,S1,E1,B2,S2,E2,L<:SInteger}
    StaticStepRangeLen(first(r1)+first(r2), step(r1)+step(r2), L())
end

function +(r1::StaticStepRangeLen{T}, r2::StaticStepRangeLen{T}) where {T}
    len = length(r1)
    (len == length(r2) || throw(DimensionMismatch("argument dimensions must match")))
    StaticStepRangeLen(first(r1)+first(r2), step(r1)+step(r2), len)
end

for f in (:+, :-)
    @eval begin
        function $f(r1::StaticStepRange, r2::StaticStepRange)
            r1l = length(r1)
            (r1l == length(r2) ||
             throw(DimensionMismatch("argument dimensions must match")))
            range($f(first(r1), first(r2)), step=$f(step(r1), step(r2)), length=r1l)
        end

        function $f(r1::StaticLinRange{T}, r2::StaticLinRange{T}) where T
            len = length(r1)
            (len == length(r2) ||
             throw(DimensionMismatch("argument dimensions must match")))
            StaticLinRange{T}(convert(T, $f(first(r1), first(r2))),
                              convert(T, $f(last(r1), last(r2))), len)
        end

        $f(r1::Union{StaticStepRangeLen, StaticOrdinalRange, StaticLinRange},
           r2::Union{StaticStepRangeLen, StaticOrdinalRange, StaticLinRange}) =
               $f(promote(r1, r2)...)
    end
end

function +(
    r1::StaticRange{T,L1},
    r2::StaticRange{T,L2}) where {T,L1,L2}
    throw(DimensionMismatch("argument dimensions must match"))
end

Base.issorted(r::StaticUnitRange) = true

Base.sort(r::StaticUnitRange) = r
Base.sort!(r::StaticUnitRange) = r

Base.sort(r::StaticRange) = issorted(r) ? r : reverse(r)

Base.sortperm(r::StaticUnitRange) = SOne:length(r)
Base.sortperm(r::StaticRange) = issorted(r) ? (SOne:SOne:length(r)) : (length(r):-SOne:SOne)

# TODO these can easily be optimized with static params
Base.isempty(r::StaticStepRange) = (first(r) != last(r)) & ((step(r) > zero(step(r))) != (last(r) > first(r)))
Base.isempty(r::StaticUnitRange) = first(r) > last(r)
Base.isempty(r::StaticStepRangeLen) = length(r) == 0
Base.isempty(r::StaticLinRange) = length(r) == 0

Base.map(::Type{T}, r::StaticStepRange) where {T<:Real} = ofeltype(T, first(r)):ofeltype(T, step(r)):ofeltype(T, last(r))
Base.map(::Type{T}, r::StaticUnitRange) where {T<:Real} = ofeltype(T, first(r)):ofeltype(T, last(r))
Base.map(::Type{T}, r::StaticStepRangeLen) where {T<:AbstractFloat} =
    StaticStepRangeLen{T}(ofeltype(T, first(r)), ofeltype(T, step(r)), length(r), offset(r))
Base.map(::Type{T}, r::StaticLinRange) where T<:AbstractFloat =
    StaticLinRange{T}(ofeltype(T, first(r)), ofeltype(T, last(r)), length(r))

==(r::Union{StaticStepRangeLen{T},StaticLinRange{T}}, s::Union{StaticStepRangeLen{T},StaticLinRange{T}}) where T =
    (first(r) == first(s)) & (length(r) == length(s)) & (last(r) == last(s))
==(r::Union{StepRangeLen{T},LinRange{T}}, s::Union{StaticStepRangeLen{T},StaticLinRange{T}}) where T =
    (first(r) == first(s)) & (length(r) == length(s)) & (last(r) == last(s))
==(r::Union{StaticStepRangeLen{T},StaticLinRange{T}}, s::Union{StepRangeLen{T},LinRange{T}}) where T =
    (first(r) == first(s)) & (length(r) == length(s)) & (last(r) == last(s))
