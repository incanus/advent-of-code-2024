#!/usr/bin/env janet

(def input (slurp "inputs/day22.txt"))

(var buyers
  (->>
    input
    (peg/match '(some (* (number :d+) :s*)))
    (map int/u64)))

(defn evolve [n]
  (def p 16777216)

  (var r1 (* n 64))
  (var out (mod (bxor r1 n) p))

  (var r2 (div out 32))
  (set out (mod (bxor r2 out) p))

  (var r3 (* out 2048))
  (set out (mod (bxor r3 out) p)))

(defn sum-evolutions [buyers evolutions]
  (var result 0)
  (each buyer buyers
    (var i buyer)
    (repeat evolutions (set i (evolve i)))
    (+= result i))
  (print "Sum: " result))

(sum-evolutions buyers 2000)
