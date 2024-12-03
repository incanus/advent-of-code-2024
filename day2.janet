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