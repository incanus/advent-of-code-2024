#!/usr/bin/env janet

(def input (slurp "inputs/day14.txt"))

(def robots
  (->>
    input
    (peg/match
      '{:number (number (* (between 0 1 "-") :d+))
        :pair (* :number "," :number)
        :main (any (group (* "p=" :pair " v=" :pair :s)))})
    (map |(table :x ($ 0) :y ($ 1) :dx ($ 2) :dy ($ 3)))))

(def limits {:x 101 :y 103})

(defn move [robot]
  (each dir [:x :y]
    (+= (robot dir) (robot (keyword :d dir)))
    (cond
      (< (robot dir) 0)
        (+= (robot dir) (limits dir))
      (>= (robot dir) (limits dir))
        (-= (robot dir) (limits dir)))))

(defn count-robots [robots]
  (var counts @[])
  (let [half-x (div (limits :x) 2)
        half-y (div (limits :y) 2)]
    (def quadrants
      [{:x [0 half-x]
        :y [0 half-y]}
       {:x [(inc half-x) (limits :x)]
        :y [0 half-y]}
       {:x [0 half-x]
        :y [(inc half-y) (limits :y)]}
       {:x [(inc half-x) (limits :x)]
        :y [(inc half-y) (limits :y)]}])
    (defn in-quadrant? [robot quadrant]
      (and
        (>= (robot :x) ((quadrant :x) 0))
        (<  (robot :x) ((quadrant :x) 1))
        (>= (robot :y) ((quadrant :y) 0))
        (<  (robot :y) ((quadrant :y) 1))))
    (each quadrant quadrants
      (->>
        robots
        (filter |(in-quadrant? $ quadrant))
        (length)
        (array/push counts)))
    counts))

(each robot robots
  (repeat 100
    (move robot)))

(print "Safety factor: " (* ;(count-robots robots)))