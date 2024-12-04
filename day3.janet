(def input (slurp "inputs/day3.txt"))

(print "Valid instructions sum: "
  (->> input
       (peg/match '{:operand (number (between 1 3 :d))
                    :instruction (* "mul(" :operand "," :operand ")")
                    :garbage (any (if-not :instruction 1))
                    :main (any (* :garbage (group :instruction) :garbage))})
       (map |(* ;$))
       (sum)))