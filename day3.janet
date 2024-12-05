(def input (slurp "inputs/day3.txt"))

(defn multiply-and-sum [operands] (sum (map |(* ;$) operands)))

(print "Valid instructions sum: "
  (->> input
       (peg/match '{:operand (number (between 1 3 :d))
                    :instruction (* "mul(" :operand "," :operand ")")
                    :garbage (any (if-not :instruction 1))
                    :main (any (* :garbage (group :instruction)))})
       (multiply-and-sum)))

########################################################################

(print "Conditional instructions sum: "
  (->> input
       (peg/match '{:operand (number (between 1 3 :d))
                    :instruction (* "mul(" :operand "," :operand ")")
                    :ignorable (* "don't()" (thru "do()"))
                    :main (any (+ (group :instruction) (+ :ignorable 1)))})
       (multiply-and-sum)))