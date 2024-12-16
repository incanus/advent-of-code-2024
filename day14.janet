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

(def robots-1 (array ;robots))

(each robot robots-1
  (repeat 100
    (move robot)))

(print "Safety factor: " (* ;(count-robots robots-1)))

########################################################################

(def robots-2 (array ;robots))

(var moves 0)

(defn illustrate-grid [robots]
  (var grid
    (buffer/new-filled
      (* (limits :x) (limits :y))
      (chr ".")))
  (each robot robots
    (buffer/push-at
      grid
      (+ (robot :x) (* (robot :y) (limits :x)))
      (chr "x")))
  (var output @"")
  (each row (range (limits :y))
    (let [start (* row (limits :x))
          end   (+ start (limits :x))]
      (buffer/push-string
        output
        (buffer/slice grid start end)
        "\n")))
  output)

(let [adjacents (string/repeat "x" 8)]
  (while
    (let [grid (illustrate-grid robots-2)]
      (not (string/find adjacents grid)))
    (each robot robots-2
      (move robot))
    (++ moves)))

(print (illustrate-grid robots-2))
(print "Minimum seconds: " moves)

#
# Tough spots / learning opportunities:
#
# 1. Line 8: Feels like I'm getting pretty good at PEGs for input parsing.
#
# 2. Lines 52 & 62: In keeping with my previous solves, I wanted to keep
#    each part separate in code. But needing to use the robots' starting
#    positions both times, I had to figure out a compact way to copy the
#    original input over for each part. Had fun both here and Line 58 with
#    the splicing reader macro.
#
# 3. I will fully admit to not having a clue how to solve Part Two. I did
#    some digging online for what to even look for and multiple people
#    online came up with the idea of looking for a number of robots in a
#    row to indicate the picture. I don't love this, as what a Christmas
#    tree looks like in text is very arbitrary (is it solid? is it
#    outlined? etc.) but the approach worked. I looked for a string of
#    eight in a row in the formatted textual output, as opposed to doing
#    numerical comparison. This required I make a second, better version
#    of my outputting function. The first went through all of the grid
#    positions and filtered the robots by matching coordinates.
#    Unsurprisingly, this was quite slow. The final version starts with
#    a dot-filled buffer and then iterates through each robot, pushing a
#    marker to the buffer offset indicated by their position, with no
#    searching through the robots of any sort. Plus, it's nice to output
#    the solve at the end!
#