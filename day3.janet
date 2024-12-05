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

#
# Tough spots / learning opportunities:
#
# 1. This was all about the PEGs. Plenty of learning to be had here to
#    navigate the intricacies of the matching language. These could be made
#    better, but I'm pretty happy to have gotten this far on Day 3.