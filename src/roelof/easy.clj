(ns roelof.easy
  (:require [hyperfiddle.rcf :as rcf]
            [clojure.string :as str]))

(rcf/enable!)

(comment
 (defn process-value
     [value]
     (if (and (number? value) (> value 10))
       :pretty-big
       (if (and (number? value) (< value 0))
         :negative
         (if (and (number? value) (zero? value))
           :zero
           (if (number? value)
             :small-number
             (if (string? value)
               :a-string
               :something-else)))))))

(defn process-value
  [value]
  (cond
    (string? value) :a-string 
    (not (number? value)) :something-else
    (zero?  value) :zero
    (> value 10) :pretty-big
    (< value 0) :negative
    :else :small-number ))


(defn triangle?
  "Write a function to determine if some 3 side lengths are sufficient to make a triangle."
  [a b c]
  (and
   (> a 0) (> b 0) (> c 0)
   (>= (+ a b) c)
   (>= (+ a c) b)
   (>= (+ b c) a)))

(defn classify-triangle
  "classify a triangle"
  [a b c ]
  (cond
     (not (triangle? a b c)) :invalid
     (= a b c) :equilateral
     (or (= a b) (= a c) (= b c)) :isosceles
     :else :scalene))

(defn clamp
  "restrict a value to a given range"
  [x min max]
  (cond
    (< min x max) x
    (> x max) max
    :else min))

(comment
  (defn process-number [n] (* (+ (inc (/ n 1.5)) 2) 10))
)

(defn process-number
 [n]
  (-> n 
    (/ 1.5)
    (* 2)
    (* 10)))

(comment
  (defn process-list
  [coll]
  (map (fn [x]
         (* x 10))
       (filter even? (map inc coll))))
)

(defn process-list 
  [coll]
  (->> coll
    (map inc)
    (filter even?)
    (map (fn[x] (* x 10 )))))

(defn largest 
  [nums]
  (apply max nums))


(defn score-letter [l]
  (condp contains? (str/upper-case l)
    #{"A" "E" "I" "O" "U" "L" "N" "R" "S" "T"}  1
    #{"D" "G"}                                  2
    #{"B" "C" "M" "P"}                          3
    #{"F" "H" "V" "W" "Y"}                      4
    #{"K"}                                      5
    #{"J" "X"}                                  8
    #{"Q" "Z"}                                  10
    0))

(defn score [w]
  (->> w
       (map #(score-letter %))
       (apply +)))

(rcf/tests
 "valid triangle"
  (triangle? 3 4 5 ) := true
  (triangle? 1 1 2) := true
  "non valid triangle"
  (triangle? 3 1 1 ) := false
  "valid triangle on the sides"
  (triangle? 1 1 1) := true
  "non-valid on one side"
  (triangle? 1 1 0) := false
  (triangle? 1 0 1) := false
  (triangle? 0 1  1) := false
  (triangle? 0 0 0) := false
  "a  triangle"
  (classify-triangle 1 1 1) := :equilateral
  (classify-triangle 0 0 0) := :invalid
  (classify-triangle 1 1 2) := :isosceles
  (classify-triangle 4 4 5) := :isosceles
  (classify-triangle 3 4 5) := :scalene
  "test clamp"
  (clamp 2 1 4) := 2
  (clamp 0 1 4) := 1
  (clamp 5 1 4) := 4
  "largest"
   (largest [1 2 -1 3 3 2 1 0]) := 3
   "scrabble score"
   (score "hello") := 8

)