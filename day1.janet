#!/usr/bin/env janet

(var distance 0)
(var list-one @[])
(var list-two @[])

(with [input (file/open "./inputs/day1.txt")]
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

#
# Tough spots / learning opportunities:
#
# 1. Line 10: Being sure to filter the input lines by scannable numbers
#    since splitting on a single space produces multiple empty string values.
#    Splitting on a fixed number of spaces or a regex would also solve this,
#    but I'm trying to use functional programming idioms more often.
#
# 2. Line 14: Learning to think about zipping the lists together to get an
#    iterable collection for destructuring.
#
# 3. Searching for list 1 items in list 2, not just combining the lists and
#    finding numbers for which there were 2+ occurrences in the big list.
#
# 4. Line 24: Learning to write the terse form of the function that finds
#    occurrences.
#