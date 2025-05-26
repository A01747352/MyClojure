;----------------------------------------------------------
; Problem Set #4: Higher-Order Functions
; Date: March 31, 2025.
; Authors:
;          A01747352 Diego Carreón Aguirre
;          A01800593 Emilio de León Vives
;----------------------------------------------------------
(ns higher-order-functions
    (:require [clojure.test :refer [deftest is run-tests]])
    (:require [clojure.algo.generic.math-functions
               :refer [approx=]]))
;-----------------------------------------------------------
(defn argswap [f]
      (fn [x y] (f y x)))

(deftest test-argswap
         (is (= '(2 1)
                ((argswap list) 1 2)))
         (is (= -7
                ((argswap -) 10 3)))
         (is (= 1/4
                ((argswap /) 8 2)))
         (is (= '((4 5 6) 1 2 3)
                ((argswap cons) '(1 2 3) '(4 5 6))))
         (is (= '(1 0 4 25 100)
                ((argswap map) '(-1 0 2 5 10) #(* % %)))))
;-----------------------------------------------------------
(defn there-exists-one [pred? s]
      (= 1 (count (filter pred? s))))

(deftest test-there-exists-one
         (is (not (there-exists-one pos?
                                    ())))
         (is (there-exists-one pos?
                               '(-1 -10 4 -5 -2 -1)))
         (is (there-exists-one neg?
                               '(-1)))
         (is (not (there-exists-one symbol?
                                    '(4 8 15 16 23 42))))
         (is (there-exists-one symbol?
                               '(4 8 15 sixteen 23 42))))
;-----------------------------------------------------------
(defn linear-search [vct x eq-fun]
      (loop [i 0]
            (cond
              (= i (count vct)) nil
              (eq-fun x (vct i)) i
              :else (recur (inc i)))))

(deftest test-linear-search
         (is (nil? (linear-search [] 5 =)))
         (is (= 0 (linear-search [5] 5 =)))
         (is (= 4 (linear-search
                    [48 77 30 31 5 20 91 92
                     69 97 28 32 17 18 96]
                    5
                    =)))
         (is (= 3 (linear-search
                    ["red" "blue" "green" "black" "white"]
                    "black"
                    identical?)))
         (is (nil? (linear-search
                     [48 77 30 31 5 20 91 92
                      69 97 28 32 17 18 96]
                     96.0
                     =)))
         (is (= 14 (linear-search
                     [48 77 30 31 5 20 91 92
                      69 97 28 32 17 18 96]
                     96.0
                     ==)))
         (is (= 8 (linear-search
                    [48 77 30 31 5 20 91 92
                     69 97 28 32 17 18 96]
                    70
                    #(<= (abs (- %1 %2)) 1)))))
;-----------------------------------------------------------
(defn deriv
      [f h]
      (fn [x]
          (/ (- (f (+ x h))
                (f x))
             h)))

(defn f [x] (* x x x))
(def df (deriv f 0.001))
(def ddf (deriv df 0.001))
(def dddf (deriv ddf 0.001))

(deftest test-deriv
         (is (approx= 75 (df 5) 0.05))
         (is (approx= 30 (ddf 5) 0.05))
         (is (approx= 6  (dddf 5) 0.05)))

;-----------------------------------------------------------
(defn newton
      [f n]
      (let [df (deriv f 0.0001)]
           (loop [x 0
                  i 0]
                 (if (>= i n)
                   x
                   (recur (- x (/ (f x) (df x)))
                          (inc i))))))

(deftest test-newton
         (is (approx= 10.0
                      (newton (fn [x] (- x 10))
                              1)
                      0.00001))
         (is (approx= -0.5
                      (newton (fn [x] (+ (* 4 x) 2))
                              1)
                      0.00001))
         (is (approx= -1.0
                      (newton (fn [x] (+ (* x x x) 1))
                              50)
                      0.00001))
         (is (approx= -1.02987
                      (newton (fn [x] (+ (Math/cos x)
                                         (* 0.5 x)))
                              5)
                      0.00001)))

;-----------------------------------------------------------
(defn integral [a b n f]
      (let [n (if (even? n) n (inc n))
            h (/ (- b a) n)]
           (loop [k 1
                  r (+ (f a) (f b))]
                 (if (< k n)
                   (let [yk (+ a (* k h))
                         coeff (if (odd? k) 4
                                            2)]
                        (recur (inc k)
                               (+ r (* coeff (f yk)))))
                   (* (/ h 3) r)))))

(deftest test-integral
         (is (= 1/4 (integral 0 1 10 (fn [x] (* x x x)))))
         (is (= 21/4
                (integral 1 2 10
                          (fn [x]
                              (integral 3 4 10
                                        (fn [y]
                                            (* x y))))))))

;-----------------------------------------------------------
(defn binary-search [vct x lt-fun]
      (loop [left 0
             right (dec(count vct))]
            (if (<= left right)
              (let [mid (quot (+ left right) 2)
                    mid_value (vct mid)]
                   (cond
                     (not (or (lt-fun x mid_value) (lt-fun mid_value x))) mid
                     (lt-fun x mid_value) (recur left (dec mid))
                     :else (recur (inc mid) right)))
              nil)))

(def small-list [4 8 15 16 23 42])

(def big-list [0 2 5 10 11 13 16 20 24 26
               29 30 31 32 34 37 40 43 44
               46 50 53 58 59 62 63 66 67
               70 72 77 79 80 83 85 86 94
               95 96 99])

(def animals ["dog" "dragon" "horse" "monkey" "ox"
              "pig" "rabbit" "rat" "rooster" "sheep"
              "snake" "tiger"])
(defn str<
      "Returns true if a is less than b, otherwise
       returns false. Designed to work with strings."
      [a b]
      (< (compare a b) 0))

(deftest test-binary-search
         (is (nil? (binary-search [] 5 <)))
         (is (= 3 (binary-search small-list 16 <)))
         (is (= 0 (binary-search small-list 4 <)))
         (is (= 5 (binary-search small-list 42 <)))
         (is (nil? (binary-search small-list 7 <)))
         (is (nil? (binary-search small-list 2 <)))
         (is (nil? (binary-search small-list 99 <)))
         (is (= 17 (binary-search big-list 43 <)))
         (is (= 0 (binary-search big-list 0 <)))
         (is (= 39 (binary-search big-list 99 <)))
         (is (nil? (binary-search big-list 12 <)))
         (is (nil? (binary-search big-list -1 <)))
         (is (nil? (binary-search big-list 100 <)))
         (is (= 5 (binary-search animals "pig" str<)))
         (is (= 0 (binary-search animals "dog" str<)))
         (is (= 11 (binary-search animals "tiger" str<)))
         (is (nil? (binary-search animals "elephant" str<)))
         (is (nil? (binary-search animals "alligator" str<)))
         (is (nil? (binary-search animals "unicorn" str<))))

(binary-search small-list 16 <)

(run-tests)
