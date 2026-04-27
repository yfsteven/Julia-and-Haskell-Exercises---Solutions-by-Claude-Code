module MathUtils

using Dates

export sphere_volume, quadratic_equation, number_of_zeros, draw_pascal,
       euler, days, remove_consecutive_dups, remove_dups, replicate_list,
       split_list, min_max_median, bc, subsets, max_subarray

"""Volume of a sphere with radius r."""
function sphere_volume(r)
    return (4 / 3) * π * r^3
end

"""Real roots of a*x^2 + b*x + c = 0. Returns empty array when discriminant < 0."""
function quadratic_equation(a, b, c)
    disc = b^2 - 4 * a * c
    if disc > 0
        return [(-b + sqrt(disc)) / (2a), (-b - sqrt(disc)) / (2a)]
    elseif disc == 0
        return [-b / (2a)]
    else
        return Float64[]
    end
end

"""Count zeros in lst."""
function number_of_zeros(lst)
    return count(==(0), lst)
end

"""Print the first n rows of Pascal's triangle."""
function draw_pascal(n)
    row = [1]
    for _ in 1:n
        println(join(row, " "))
        row = vcat([1], [row[i] + row[i + 1] for i in 1:length(row)-1], [1])
    end
end

"""Unique positive integer whose square has form 1_2_3_4_5_6_7_8_9_0.
   Searches n divisible by 10 (required since square ends in 0) in [10^9, ~3.16*10^9]."""
function euler()
    for n in 1_000_000_000:10:3_162_277_660
        sq = n * n
        s  = string(sq)
        if length(s) == 19 &&
           s[1]  == '1' && s[3]  == '2' && s[5]  == '3' &&
           s[7]  == '4' && s[9]  == '5' && s[11] == '6' &&
           s[13] == '7' && s[15] == '8' && s[17] == '9' &&
           s[19] == '0'
            return n
        end
    end
end

"""Days from date1 to date2, inclusive. Dates must be strings in "YYYY-MM-DD" format."""
function days(date1, date2)
    d1 = Date(date1)
    d2 = Date(date2)
    return Dates.value(d2 - d1) + 1
end

"""Remove consecutive duplicates from lst."""
function remove_consecutive_dups(lst)
    isempty(lst) && return []
    result = [lst[1]]
    for x in lst[2:end]
        x != result[end] && push!(result, x)
    end
    return result
end

"""Remove all duplicates from lst, preserving first-occurrence order."""
function remove_dups(lst)
    seen   = Set()
    result = []
    for x in lst
        if x ∉ seen
            push!(seen, x)
            push!(result, x)
        end
    end
    return result
end

"""Replicate each element of lst n times."""
function replicate_list(lst, n)
    return vcat([fill(x, n) for x in lst]...)
end

"""Split lst into two parts: the first n elements and the remainder."""
function split_list(lst, n)
    return (lst[1:n], lst[n+1:end])
end

"""Min, max, and median of lst. Median uses quickselect for expected O(n) time."""
function min_max_median(lst)
    mn = minimum(lst)
    mx = maximum(lst)
    n  = length(lst)
    if isodd(n)
        med = _quickselect(copy(lst), (n + 1) ÷ 2)
    else
        lo  = _quickselect(copy(lst), n ÷ 2)
        hi  = _quickselect(copy(lst), n ÷ 2 + 1)
        med = (lo + hi) / 2
    end
    return [mn, mx, med]
end

function _quickselect(arr, k)
    length(arr) == 1 && return arr[1]
    pivot  = arr[length(arr) ÷ 2]
    lows   = filter(<(pivot), arr)
    pivots = filter(==(pivot), arr)
    highs  = filter(>(pivot), arr)
    if k <= length(lows)
        return _quickselect(lows, k)
    elseif k <= length(lows) + length(pivots)
        return pivot
    else
        return _quickselect(highs, k - length(lows) - length(pivots))
    end
end

"""Binomial coefficient n choose k.
   Computes C(n,1), C(n,2), ... C(n,k) iteratively so each step stays an integer,
   avoiding the large intermediate products of the naive formula."""
function bc(n, k)
    (k < 0 || k > n) && return 0
    k = min(k, n - k)
    result = 1
    for i in 1:k
        result = result * (n - i + 1) ÷ i
    end
    return result
end

"""All n-element subsets of collection s (returned as a vector of vectors)."""
function subsets(s, n)
    s      = collect(s)
    m      = length(s)
    result = []
    for bits in 0:(2^m - 1)
        subset = [s[i] for i in 1:m if (bits >> (i - 1)) & 1 == 1]
        length(subset) == n && push!(result, subset)
    end
    return result
end

"""Contiguous subarray of arr with the largest sum (Kadane's algorithm)."""
function max_subarray(arr)
    isempty(arr) && return []
    cur_sum    = arr[1]
    cur_start  = 1
    best_sum   = arr[1]
    best_start = 1
    best_end   = 1
    for i in 2:length(arr)
        if cur_sum + arr[i] >= arr[i]
            cur_sum += arr[i]
        else
            cur_sum   = arr[i]
            cur_start = i
        end
        if cur_sum > best_sum
            best_sum   = cur_sum
            best_start = cur_start
            best_end   = i
        end
    end
    return arr[best_start:best_end]
end

end  # module MathUtils
