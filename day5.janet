#!/usr/bin/env janet

(def input (slurp "inputs/day5.txt"))

(def [rules updates]
  (let [[a b] (string/split "\n\n" input)]
    [(do
       (def rule-pairs
         (->>
           a
           (string/trim)
           (string/split "\n")
           (map |(string/split "|" $))))
       (var rule-map @{})
       (each pair rule-pairs
         (if (rule-map (pair 0))
           (array/push (rule-map (pair 0)) (pair 1))
           (put rule-map (pair 0) @[(pair 1)])))
       (def _ rule-map))
     (->>
       b
       (string/trim)
       (string/split "\n")
       (map |(string/split "," $)))]))

(defn compare-page [a b]
  (cond
    (= a b) false
    (and (rules a) (index-of b (rules a))) true
    false))

(var total 0)

(each u updates
  (if (deep= u (sorted u compare-page))
    (let [len (length u)
          idx (math/floor (/ len 2))
          val (scan-number (u idx))]
      (+= total val))))

(print "Correctly-ordered middle-page totals: " total)

########################################################################

(set total 0)

(each u updates
  (let [sorted-u (sorted u compare-page)]
    (if (not (deep= u sorted-u))
      (let [len (length sorted-u)
            idx (math/floor (/ len 2))
            val (scan-number (sorted-u idx))]
        (+= total val)))))

(print "Fixed middle-page totals: " total)

#
# Tough spots / learning opportunities:
#
# 1. Line 5: I think I spent more time on reading the input and building
#    the data structures than solving the algorithm. It could be cleaner,
#    but I continue to enjoy the threading operators for this sort of thing.
#
# 2. Line 26: The sort comparator took a while as I had to infer what values
#    to return. At first I was doing -1/1/0, but then realized that this was
#    supposed to behave like `<`and needed to return `true` or `false`. Then
#    I added on the first equality condition to save a little time since
#    print-debugging revealed that `sorted` does an identity comparison.
#
# 3. Line 35: Forgot at first that I needed to deep-compare arrays again.
#
# 4. Line 47: It was trivial to solve Part Two. I thought about combining
#    the work of both into one algorithm, but wanted to keep the formatting
#    the same as other days, where each has their own separate section.
#
# 5. Lines 38 & 52: I realized that since I was doing a custom sort based
#    on array indexing anyway, I could leave things as strings and just
#    scan them into numbers once I needed to sum them.
#