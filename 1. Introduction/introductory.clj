;----------------------------------------------------------
; Problem Set #1: Introductory Exercises
; Date: February 21, 2025.
; Authors:
;          A01747352 Diego Carreón
;          A01800593 Emilio De León
;----------------------------------------------------------

(ns introductory
    (:require [clojure.test :refer [deftest is run-tests]])
    (:require [clojure.math.numeric-tower :refer [sqrt]]))

;----------------------------------------------------------
"Converts gibibytes to bytes"
(defn gibibytes->bytes [x]
      (* x 1024 1024 1024))

(deftest test-gibibytes->bytes
         (is (= 0 (gibibytes->bytes 0)))
         (is (= 1073741824 (gibibytes->bytes 1)))
         (is (= 5368709120 (gibibytes->bytes 5)))
         (is (= 26415122612224 (gibibytes->bytes 24601))))

;----------------------------------------------------------
"Converts fahrenheit to celsius"
(defn fahrenheit->celsius [F]
      (/ (* 5 (- F 32)) 9))

(deftest test-fahrenheit->celsius
         (is (= 100.0 (fahrenheit->celsius 212.0)))
         (is (= 0.0 (fahrenheit->celsius 32.0)))
         (is (= -40.0 (fahrenheit->celsius -40.0))))

;----------------------------------------------------------
"Returns a value if a number is negative, positive or zero"
(defn sign [n]
      (if (< n 0) -1
                  (if (> n 0) 1
                              0)))

(deftest test-sign
         (is (= -1 (sign -5)))
         (is (= 1 (sign 10)))
         (is (= 0 (sign 0))))

;----------------------------------------------------------
"Solves quadratic equations given its three coefficients"
(defn roots [a b c]
      (let [raiz (sqrt (- (* b b) (* 4 a c)))]
           (if (< raiz 0)
             nil
             (let [r1 (/ (+ (- b) raiz) (* 2 a))
                   r2 (/ (- (- b) raiz) (* 2 a))]
                  [r1 r2]))))

(deftest test-roots
         (is (= [-1 -1] (roots 2 4 2)))
         (is (= [0 0] (roots 1 0 0)))
         (is (= [-1/4 -1] (roots 4 5 1))))

;----------------------------------------------------------
"Calculates the BMI based on height an weight, returns a classification"
(defn bmi [weight height]
      (let [BMI (/ weight (* height height))]
           (cond
             (< BMI 20) 'underweight
             (and (>= BMI 20) (< BMI 25)) 'normal
             (and (>= BMI 25) (< BMI 30)) 'obese1
             (and (>= BMI 30) (< BMI 40)) 'obese2
             (>= BMI 40) 'obese3)))

(deftest test-bmi
         (is (= 'underweight (bmi 45 1.7)))
         (is (= 'normal (bmi 55 1.5)))
         (is (= 'obese1 (bmi 76 1.7)))
         (is (= 'obese2 (bmi 81 1.6)))
         (is (= 'obese3 (bmi 120 1.6))))

;----------------------------------------------------------
"Returns the type of triangle based on 3 given sides"
(defn type-of-triangle [a b c]
      (cond
        (and (== a b) (== c b)) 'equilateral
        (or (== a b) (== b c) (== a c)) 'isosceles
        :else 'scalene))

(deftest test-type-of-triangle
         (is (= 'equilateral (type-of-triangle 3 3 3)))
         (is (= 'equilateral (type-of-triangle 4.2 4.2 4.2)))
         (is (= 'isosceles (type-of-triangle 4 4 3)))
         (is (= 'isosceles (type-of-triangle 4 3 4)))
         (is (= 'isosceles (type-of-triangle 3 4 4)))
         (is (= 'scalene (type-of-triangle 1 2 3)))
         (is (= 'scalene (type-of-triangle 7.1 6.4 9.2))))

(run-tests)
