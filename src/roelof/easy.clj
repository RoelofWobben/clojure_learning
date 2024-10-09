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
    (neg? value) :negative
    :else :small-number ))


(defn triangle?
  "Write a function to determine if some 3 side lengths are sufficient to make a triangle."
  [a b c]
  (and
   (pos? a) (pos? b) (pos? c)
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

(defn clamp [x min-x max-x]
  (min max-x (max min-x x)))

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


(defn score-letter
  "looks the value of a letter in scrabble"
 [l]
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
"calculates the scrabble score of a word"
  (->> w
       (map #(score-letter %))
       (apply +)))

(defn count-if [pred? coll]
   (count (filter pred? coll)))


(def lookup
  {:mercury 0.2408467
   :venus 0.61519726
   :earth 1
   :mars 1.8808158
   :jupiter 11.862615
   :saturn 29.447498
   :uranus 84.016846
   :neptune 164.79132})

(defn convert-space-age 
[age source-planet target-planet]
  (int (* age (/ (lookup source-planet) (lookup target-planet)))))
  
(def game-state
  {:current-player "X"
   :board [nil "X" "O" nil nil "X" "O" nil nil]
   :history [{:player "X"
              :location 1}
             {:player "O"
              :location 2}
             {:player "X"
              :location 5}
             {:player "O"
              :location 6}]})

  (defn move-played
    "counts how many moves are played"
    []
    (count (game-state :history)))
 
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
   (score "question") := 17
   (score "quizzed") := 35
   (count-if even? [1 2 3 4 5 6])
   (count-if keyword? ["foo" :bar (quote baz)])
   (count-if (fn [x]
               (= x (reverse x)))
             [[1 2 1] [1 2 3 4] [1]])
    (convert-space-age 30 :earth :saturn) := 1
    (convert-space-age 10 :mars :mercury) := 78
    (move-played) := 4
)