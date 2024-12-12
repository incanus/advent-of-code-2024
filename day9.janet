#!/usr/bin/env janet

(def input (string/trim (slurp "inputs/day9.txt")))

(defn expand-map [block-map]
  (def len (length block-map))
  (var expanded @[])
  (var id 0)
  (var i 0)
  (while (< i len)
    (let [c (string/slice block-map i (inc i))]
      (++ i)
      (repeat (scan-number c)
        (array/push expanded id))
      (++ id)
      (if (< i (dec len))
        (let [g (string/slice block-map i (inc i))]
          (++ i)
          (repeat (scan-number g)
            (array/push expanded nil))))))
  expanded)

(defn compact-blocks [block-map]
  (let [num-gaps (length (filter |(nil? $) block-map))
        len (length block-map)]
    (defn compacted? [check-map]
      (nil? (check-map (- len num-gaps))))
    (var i (dec len))
    (var next-gap (index-of nil block-map))
    (while (not (compacted? block-map))
      (def tail (block-map i))
      (if (not (nil? tail))
        (do
          (while (not (nil? (block-map next-gap))) (++ next-gap))
          (put block-map next-gap tail)
          (put block-map i nil)))
      (-- i))
    block-map))

(defn checksum-blocks [block-map]
  (let [blocks (filter |(not (nil? $)) block-map)]
    (var checksum 0)
    (eachp [i block] blocks
      (+= checksum (* i block)))
    checksum))

(print "Checksum: " (-> input (expand-map) (compact-blocks) (checksum-blocks)))