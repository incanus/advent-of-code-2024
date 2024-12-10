#!/usr/bin/env janet

(def input (string/trim (slurp "inputs/day6.txt")))

(def rows
  (->>
    input
    (string/split "\n")
    (map |(array/concat @[] (string/bytes $)))))
(def num-rows (length rows))
(def num-cols (length (first rows)))

(def [available visited blocked] [(chr ".") (chr "X") (chr "#")])
(def dirs [:north :east :south :west])

(var next-space nil)
(var dir :north)
(var pos
  (label start
    (each row rows
      (if-let [n (index-of (chr "^") row)]
        (do
          (put row n visited)
          (return start @{:row (index-of row rows) :col n}))))))

(defn peek-next-space []
  (cond
    (and (= dir :north) (> (pos :row) 0))
      (get (rows (dec (pos :row))) (pos :col))
    (and (= dir :east) (< (pos :col) (dec num-cols)))
      (get (rows (pos :row)) (inc (pos :col)))
    (and (= dir :south) (< (pos :row) (dec num-rows)))
      (get (rows (inc (pos :row))) (pos :col))
    (and (= dir :west) (> (pos :col) 0))
      (get (rows (pos :row)) (dec (pos :col)))
    nil))

(defn move []
  (case dir
    :north (-- (pos :row))
    :east  (++ (pos :col))
    :south (++ (pos :row))
    :west  (-- (pos :col))))

(while (set next-space (peek-next-space))
  (case next-space
    visited
      (do
        (move)
        (print "Moving to already-visited " (pos :row) "," (pos :col)))
    available
      (do
        (move)
        (put (rows (pos :row)) (pos :col) visited)
        (print "Moving to new " (pos :row) "," (pos :col)))
    blocked
      (do
        (def next-dir-idx (mod (inc (index-of dir dirs)) (length dirs)))
        (set dir (dirs next-dir-idx))
        (print "Blocked! Turning " dir))))

(print "Off-map! Distinct positions visited: "
  (length (filter |(= visited $) (array/join @[] ;rows))))