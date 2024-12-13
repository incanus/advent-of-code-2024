#!/usr/bin/env janet

(def stones
  (->> (slurp "inputs/day11.txt")
       (peg/match ~{:main (some (+ (number :d+) :s))})))

(defn blink [stones]
  (var i 0)
  (while (< i (length stones))
    (let [stone (stones i)]
      (cond
        (zero? stone) (put stones i 1)
        (even? (+ (math/floor (math/log10 stone)) 1))
        (let [str   (string stone)
              mid   (/ (length str) 2)
              left  (string/slice str 0 mid)
              right (string/slice str mid)]
          (put stones i (scan-number left))
          (array/insert stones (++ i) (scan-number right)))
        (put stones i (* stone 2024))))
    (++ i))
  (print (length stones)))

(repeat 25 (blink stones))