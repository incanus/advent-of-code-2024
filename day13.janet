#!/usr/bin/env janet

(def input (slurp "inputs/day13.txt"))

#
# e.g. {:a @[75 30] :b @[57 75] :prize @[9885 8130]}
#
(def machines
  (do
    (defn keywordize [str]
      (keyword (string/ascii-lower str)))
    (->>
      input
      (peg/match
        ~{:distance (number :d+)
          :move (* (+ "X" "Y") "+" :distance)
          :button (* "Button " (/ (<- (+ "A" "B")) ,keywordize) ": " (group (* :move ", " :move)))
          :target (* (+ "X" "Y") "=" :distance)
          :prize (* (/ (<- "Prize") ,keywordize) ": " (group (* :target ", " :target)))
          :machine (* :button :s :button :s :prize :s*)
          :main (any (group :machine))})
      (map |(struct ;$)))))

(def [cost-x cost-y] [3 1])

#
# make function to solve B count for a given A count on one axis
#
(defn make-solver-fn [a-dist b-dist prize-dist]
  (fn [a] (/ (- prize-dist (* a-dist a)) b-dist)))

(defn solve-machine [machine max-presses]
  (let [[a-x a-y] (machine :a)
        [b-x b-y] (machine :b)
        [prize-x prize-y] (machine :prize)
        x-solver (make-solver-fn a-x b-x prize-x)
        y-solver (make-solver-fn a-y b-y prize-y)]
    #
    # for x-axis, solve for B count for A counts of 1-100
    #
    (var x-solves @[])
    (each a-count (range (inc max-presses))
      (let [b-count (x-solver a-count)]
        (if (and
              (int? b-count)
              (>= b-count 0)
              (<= b-count max-presses))
          (array/push x-solves [a-count b-count]))))
    #
    # given x-axis solves, see which also solve for y-axis
    #
    (var solves @[])
    (each x-solve x-solves
      (let [[a-count b-count] x-solve]
        (if (= b-count (y-solver a-count))
          (array/push solves [a-count b-count]))))
    solves))

(defn find-minimum-tokens [machines max-presses]
  (var tokens 0)
  (each machine machines
    (var minimum math/int32-max)
    (each solve (solve-machine machine max-presses)
      (let [cost (+ (* (solve 0) cost-x) (* (solve 1) cost-y))]
        (if (< cost minimum)
          (set minimum cost))))
    (if (< minimum math/int32-max)
      (+= tokens minimum)))
  tokens)

(print "Minimum token cost: " (find-minimum-tokens machines 100))


