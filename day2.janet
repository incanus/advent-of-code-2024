#!/usr/bin/env janet

(def reports
  (->> (string/trim (slurp "inputs/day2.txt"))
       (peg/match ~{:main (split "\n" (group :line))
                    :line (some (+ (number :d+) :s))})))

(var unsafe 0)

(each report reports
  (if (or
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
        (set previous level))))
    (++ unsafe)))

(print "Safe reports: " (- (length reports) unsafe))