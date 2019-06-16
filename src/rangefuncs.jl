const OverflowSafe = Union{Bool,Int8,Int16,Int32,Int64,Int128,
                           UInt8,UInt16,UInt32,UInt64,UInt128}

_in_unit_range(v::StaticUnitRange, val, i::Integer) =
    i > 0 && val <= last(v) && val >= first(v)

function getindex(v::StaticUnitRange{T}, i::Integer) where {T<:OverflowSafe}
    Base.@_inline_meta
    val = first(v) + (i - one(i))
    @boundscheck _in_unit_range(v, val, i) || Base.throw_boundserror(v, i)
    val % T
end

@inline function _getindex_hiprec(
    r::StaticStepRangeLen{T},
    i::I) where {T,B<:Union{<:TPVal,TwicePrecision},I<:Integer}
    u = i - offset(r)
    shift_hi, shift_lo = u*getstephi(r), u*getsteplo(r)
    x_hi, x_lo = Base.add12(getstarthi(r), shift_hi)
    x_hi, x_lo = Base.add12(x_hi, x_lo + (shift_lo + getstartlo(r)))
    tpval(x_hi, x_lo)
end

function getindex(r::StaticUnitRange, s::Union{AbstractUnitRange{<:Integer},StaticUnitRange{<:Integer}})
    Base.@_inline_meta
    @boundscheck checkbounds(r, s)
    f = first(r)
    st = oftype(f, f + first(s)-1)
    srange(st, length=length(s))
end


function getindex(r::StaticUnitRange, s::Union{StepRange{<:Integer},StaticStepRange{<:Integer}})
    Base.@_inline_meta
    @boundscheck checkbounds(r, s)
    st = ofeltype(first(r), first(r) + first(s)-SOne)
    srange(st, step=step(s), length=length(s))
end

function getindex(r::StaticStepRange, s::AbstractRange{<:Integer})
    Base.@_inline_meta
    @boundscheck checkbounds(r, s)
    st = oftype(first(r), first(r) + (first(s)-SOne)*step(r))
    srange(st, step=step(r)*step(s), length=length(s))
end

function getindex(r::StaticStepRangeLen{T},
                  s::Union{OrdinalRange{<:Integer},StaticOrdinalRange{<:Integer}}) where {T}
    Base.@_inline_meta
    @boundscheck checkbounds(r, s)
    # Find closest approach to offset by s
    ind = LinearIndices(s)
    offset = max(min(1 + round(Int, (offset(r) - first(s))/step(s)), last(ind)), first(ind))
    ref = _getindex_hiprec(r, first(s) + (offset-1)*step(s))
    return StaticStepRangeLen{T}(ref, step(r)*step(s), length(s), offset)
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

unsafe_getindex(r::StaticLinRange{T}, i::I) where {T,I<:Integer} =
    lerpi(i-one(i), lendiv(r), start(r), last(r))

function lerpi(j::Integer, d::Integer, a::A, b::B) where {A,B}
    Base.@_inline_meta
    t = j/d
    oftype(a, (one(t) - t) * a + t * b)
end

@inline function unsafe_getindex(
    r::StaticStepRangeLen{T,B}, i::Integer) where {T,B<:Union{<:TPVal,TwicePrecision}}
    Base.@_inline_meta
    u = i - offset(r)
    shift_hi, shift_lo = u*getstephi(r), u*getsteplo(r)
    x_hi, x_lo = Base.add12(getstarthi(r), shift_hi)
    ofeltype(T, x_hi + (x_lo + (shift_lo + getstartlo(r))))
end

@inline function unsafe_getindex(
    r::StaticStepRangeLen{T,B}, i::I) where {T,B,I<:Integer}
    Base.@_inline_meta
    ofeltype(T, first(r) + (i - offset(r)) * step(r))
end

@inline function Base.iterate(r::Union{StaticLinRange,StaticStepRangeLen}, i::Integer=firstindex(r))
    Base.@_inline_meta
    length(r) < i && return nothing
    unsafe_getindex(r, i), i + one(i)
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

function _in_range(x, r::StaticRange)
    if step(r) == 0
        return !isempty(r) && first(r) == x
    else
        n = round(Integer, (x - first(r)) / step(r)) + 1
        return n >= 1 && n <= length(r) && r[n] == x
    end
end

in(x::Real, r::StaticRange{<:Real}) = _in_range(x, r)
# This method needs to be defined separately since -(::T, ::T) can be implemented
# even if -(::T, ::Real) is not
in(x::T, r::StaticRange{T}) where {T} = _in_range(x, r)

function in(x::Real, r::StaticRange{T}) where {T<:Integer}
    isa(x, Integer) && !isempty(r) && x >= minimum(r) && x <= maximum(r) &&
        (mod(ofeltype(T,x), step(r)) - mod(first(r), step(r)) == 0)
end
function in(x::AbstractChar, r::StaticRange{<:AbstractChar})
    !isempty(r) && x >= minimum(r) && x <= maximum(r) &&
        (mod(Int(x) - Int(first(r)), step(r)) == 0)
end

function in(x::Integer, r::StaticUnitRange)
    ( first(r) <= x) & (x <= last(r))
end

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
        return srange(start1, step=a, length=SZero)
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
-(r::StaticStepRange) = ssrange(-first(r), step=-step(r), length=length(r))

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
Base.issorted(r::StaticRange) = length(r) <= 1 || step(r) >= zero(step(r))

Base.sort(r::StaticUnitRange) = r
Base.sort!(r::StaticUnitRange) = r

Base.sort(r::StaticRange) = issorted(r) ? r : reverse(r)

Base.sortperm(r::StaticUnitRange) = SOne:length(r)
Base.sortperm(r::StaticRange) = issorted(r) ? (SOne:SOne:length(r)) : (length(r):-SOne:SOne)
