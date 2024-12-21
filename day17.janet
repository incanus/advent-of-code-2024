#!/usr/bin/env janet

(def input (slurp "inputs/day17.txt"))

(def [A B C program]
  (peg/match
    '{:program (* "Program: " (group (some (* (number :d) (between 0 1 ",")))))
      :register (* "Register " :a ": " (number :d+))
      :main (* :register :s :register :s :register :s :s :program)}
    input))

(defn run [program a b c]
  (var A a)
  (var B b)
  (var C c)
  (var ip 0)
  (var output @[])
  (defn combo [n]
    (if (<= n 3)
      n
      (case n
        4 A
        5 B
        6 C
        nil)))
  (defn advance [] (+= ip 2))
  (while (< ip (length program))
    (let [instruction (program ip)
          operand (program (inc ip))]
      (case instruction
        0 (do
            (set A (div A (math/pow 2 (combo operand))))
            (advance))
        1 (do
            (set B (bxor B operand))
            (advance))
        2 (do
            (set B (mod (combo operand) 8))
            (advance))
        3 (if (zero? A)
            (advance)
            (set ip operand))
        4 (do
            (set B (bxor B C))
            (advance))
        5 (do
            (array/push output (mod (combo operand) 8))
            (advance))
        6 (do
            (set B (div A (math/pow 2 (combo operand))))
            (advance))
        7 (do
            (set C (div A (math/pow 2 (combo operand))))
            (advance)))))
  output)

(def output (run program A B C))

(print "Output: " (string/join (map string output) ","))
