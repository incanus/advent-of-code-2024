#!/usr/bin/env janet

(def input (slurp "inputs/day23.txt"))

(def connections
  (->>
    input
    (peg/match '(some (group (* (<- (2 :w)) "-" (<- (2 :w)) :s*))))
    (map sort)))

(defn group [connections]
  (def groups @[])
  (each connection connections
    (let [[a b]     connection
          a-matches (filter |(index-of a $) connections)
          b-matches (filter |(index-of b $) connections)
          a-bridges (flatten (map |(filter |(not= $ a) $) a-matches))
          b-bridges (flatten (map |(filter |(not= $ b) $) b-matches))
          bridges   (filter |(index-of $ a-bridges) b-bridges)]
      (each bridge bridges
        (array/push groups (tuple ;(sorted [a b bridge]))))))
  (var total 0)
  (each group (distinct groups)
    (if (not (empty? (filter |(string/has-prefix? "t" $) group)))
      (do
        (prin (inc total) ": ")
        (pp group)
        (++ total)))))

(group connections)
