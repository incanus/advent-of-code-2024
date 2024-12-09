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