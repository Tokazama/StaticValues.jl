function Base.sum(r::StaticStepRangeLen)
    l = length(r)
    # Compute the contribution of step over all indices.
    # Indexes on opposite side of r.offset contribute with opposite sign,
    #    r.step * (sum(1:np) - sum(1:nn))
    np, nn = l - offset(r), offset(r) - SOne  # positive, negative
    # To prevent overflow in sum(1:n), multiply its factors by the step
    sp, sn = Base.sumpair(np), Base.sumpair(nn)
    tp = _tp_prod(step(r), sp[1], sp[2])
    tn = _tp_prod(step(r), sn[1], sn[2])
    s_hi, s_lo = add12(gethi(tp), -gethi(tn))
    s_lo += getlo(tp) - getlo(tn)
    # Add in contributions of ref
    ref = reference(r) * l
    sm_hi, sm_lo = add12(s_hi, gethi(ref))
    add12(sm_hi, sm_lo + getlo(ref))[1]
end

function Base._tp_prod(t::TPVal, x, y...)
    Base.@_inline_meta
    Base._tp_prod(t * x, y...)
end
Base._tp_prod(t::TPVal) = t


function +(r1::StaticStepRangeLen{T,R1}, r2::StaticStepRangeLen{T,R2}
          ) where {T,R1<:Union{<:TPVal,TwicePrecision},R2<:Union{<:TPVal,TwicePrecision}}
    len = length(r1)
    (len == length(r2) || throw(DimensionMismatch("argument dimensions must match")))
    if offset(r1) == offset(r2)
        imid = offset(r1)
        ref = reference(r1) + reference(r2)
    else
        imid = round(Int, (offset(r1)+offset(r2))/ofeltype(2))
        ref1mid = _getindex_hiprec(r1, imid)
        ref2mid = _getindex_hiprec(r2, imid)
        ref = ref1mid + ref2mid
    end
    step = twiceprecision(step(r1) + step(r2), nbitslen(T, len, imid))
    StaticStepRangeLen{T,typeof(ref),typeof(step)}(ref, step, len, imid)
end

Base.sumpair(n::SInteger) = iseven(n) ? (n+SOne, n>>SOne) : (n, (n+SOne)>>SOne)
