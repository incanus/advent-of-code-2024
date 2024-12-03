#!/usr/bin/env janet

(def reports
  (->> (slurp "inputs/day2.txt")
       (string/trim)
       (peg/match ~{:main (split "\n" (group :line))
                    :line (some (+ (number :d+) :s))})))

(defn unsafe? [report]
  (or
    # has repeats?
    (->> (frequencies report)
         (values)
         (filter |(> $ 1))
         (length)
         (< 0))
    # isn't unidirectional?
    (let [sorted-report (sorted report)]
      (not (or (deep= report sorted-report)
               (deep= report (reverse sorted-report)))))
    # has a delta of 0 or >3? 
    (label bad-delta
      (var previous nil)
      (each level report
        (if previous
          (let [delta (math/abs (- level previous))]
            (if (or (= delta 0)
                    (> delta 3))
              (return bad-delta true))))
        (set previous level)))))

(print "Safe reports: " (- ;(map length [reports (filter unsafe? reports)])))

########################################################################

(defn dampened-safe? [report]
  (if (unsafe? report)
    (label safe
      (loop [i :range [0 (length report)]]
        (if (not (unsafe? (array/remove (array/concat @[] report) i)))
          (return safe true))))
    true))

(print "Safe reports (dampened): " (length (filter dampened-safe? reports)))

#
# Tough spots / learning opportunities:
#
# 1. Line 4: Learning from Jordan's PEG approach[1], I will likely keep
#    using this pattern. I wanted to use a threading macro, but it took
#    me a minute to realize that for single-argument forms, thread-last
#    is the same as thread-first and works well here for the PEG call.
#
# 2. Line 16: Similarly, it took me a bit to figure out the last check
#    here to get thread-last to answer the question I want: what *does*
#    have repeats? It's weird seeing comparison operators with a single
#    argument in the code.
#
# 3. Line 19: Had to be reminded that mutable arrays don't have normal
#    equality and require deep comparison.
#
# 4. Line 22: Is using a label "bad"? It looks good to me. It's nice in
#    this case that `each` returns `nil` by default as once all levels
#    are compared, the condition just falls out of the `or`.
#
# 5. Line 40: The big hitch here was realizing that I was modifying the
#    reports in the original `array/remove`, leading to bad index
#    errors. Figured out that I could concatenate to an empty array and
#    use *that* to test each level removal.
#
# [1] https://github.com/jordanekay/AdventOfCode/blob/main/2024/day1.janet
#