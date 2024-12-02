#!/usr/bin/env janet

(var distance 0)
(var list-one @[])
(var list-two @[])

(with [input (file/open "day1.txt")]
  (each line (file/lines input)
    (let [chunks (string/split " " (string/trim line))
          [x y] (filter number? (map scan-number chunks))]
      (array/push list-one x)
      (array/push list-two y))))

(eachp [x y] (zipcoll (sort list-one) (sort list-two))
  (+= distance (math/abs (- x y))))

(print "Total distance: " distance)

########################################################################

(var similarity 0)

(each n list-one
  (let [occurences (filter |(= n $) list-two)
        count (length occurences)]
    (+= similarity (* count n))))

(print "Similarity score: " similarity)