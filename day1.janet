#!/usr/bin/env janet

(var distance 0)
(var one @[])
(var two @[])

(with [input (file/open "day1.txt")]
  (each line (file/lines input)
    (let [chunks (string/split " " (string/trim line))
          [x y] (filter number? (map scan-number chunks))]
      (array/push one x)
      (array/push two y))))

(eachp [x y] (zipcoll (sort one) (sort two))
  (+= distance (math/abs (- x y))))

(print distance)