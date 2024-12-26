#!/usr/bin/env janet

(def input (slurp "inputs/day24.txt"))

(def wires
  (table
    ;(->>
      input
      (string/trim)
      (string/split "\n\n")
      (first)
      (peg/match
        ~{:wire (* (/ (<- (3 :w)) ,keyword))
          :value (/ (<- (range "01")) ,scan-number)
          :main (some (* :wire ": " :value :s*))}))))

(def connections
  (do
    (defn op [gate]
      (case gate
        "AND" |(if (= 1 $0 $1) 1 0)
        "OR"  |(if (or (one? $0) (one? $1)) 1 0)
        "XOR" |(if (not= $0 $1) 1 0)))
    (->>
      input
      (string/trim)
      (string/split "\n\n")
      (last)
      (peg/match
        ~{:wire (3 :w)
          :gate (+ "AND" "OR" "XOR")
          :main (some (group (* (/ (<- :wire) ,keyword) " "
                                (/ (<- :gate) ,op) " "
                                (/ (<- :wire) ,keyword) " -> "
                                (/ (<- :wire) ,keyword) :s*)))}))))

(defn run [wires connections]
  (while (> (length connections) 0)
    (eachp [i connection] connections
      (let [lhs (wires (connection 0))
            rhs (wires (connection 2))
            op  (connection 1)
            out (connection 3)]
        (if (and (not (nil? lhs)) (not (nil? rhs)))
          (do
            (array/remove connections i)
            (put wires out (op lhs rhs))))))))

(run wires connections)

(def result
  (->>
    wires
    (keys)
    (sort)
    (reverse)
    (filter |(string/has-prefix? "z" (string $)))
    (map |(wires $))
    (map string)
    (string/join)
    (string/format "2r%s")
    (scan-number)))
