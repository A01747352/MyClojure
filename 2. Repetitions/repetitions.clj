;----------------------------------------------------------
; Problem Set #2: Repetitions
; Date: March 3, 2025.
; Authors:
;          A01747352 Diego Carreón
;          A01800593 Emilio de León
;----------------------------------------------------------
(ns repetitions
    (:require [clojure.test :refer [deftest is run-tests]])
    (:require [clojure.math.numeric-tower :refer [sqrt]])
    (:require [clojure.algo.generic.math-functions
               :refer [sqr approx=]]))
;----------------------------------------------------------
; Problema 1 Versión Recursiva
;(def enlist
;  (fn [s]
;    (if (empty? s)
;      ()
;      (cons (list (first s))
;            (enlist (rest s))))

; Problema 1 Versión loop/recur
;(def enlist
;  (fn [s]
;    (loop [s s
;           result []
;      (if (empty? s) ;result
;        (recur (rest s)
;               (conj result
;                     (list (first s))]]))

; Problema 1 Versión API de secuencia
; Convierte cada elemento de la secuencia en una lista individual.
(def enlist
  (fn [s]
      (map list s)))

(deftest test-enlist
         (is (= () (enlist ())))
         (is (= '((a) (b) (c)) (enlist '(a b c))))
         (is (= '((1) (2) (3) (4)) (enlist [1 2 3 4])))
         (is (= '(((1 2 3)) (4) ((5)) (7) (8))
                (enlist '((1 2 3) 4 (5) 7 8)))))
;----------------------------------------------------------
; Problema 2
; Filtra y devuelve solo los números positivos de una lista.
(def positives
  (fn [s]
      (filter pos? s)))

(deftest test-positives
         (is (= () (positives ())))
         (is (= () (positives [-4 -1 -10 -13 -5])))
         (is (= [3 6] (positives [-4 3 -1 -10 -13 6 -5])))
         (is (= [4 3 1 10 13 6 5] (positives [4 3 1 10 13 6 5]))))
;----------------------------------------------------------
; Problema 3
; Calcula la suma de los cuadrados de los elementos de una lista.
(def add-squares
  (fn [s]
      (reduce + (map sqr s))))

(deftest test-add-squares
         (is (= 0 (add-squares [])))
         (is (= 25 (add-squares [5])))
         (is (= 30 (add-squares [2 4 1 3])))
         (is (= 385 (add-squares [1 2 3 4 5 6 7 8 9 10]))))
;----------------------------------------------------------
; Problema 4 Versión Recursiva
;(def duplicate
;  (fn [s]
;    (if (empty? s)
;      ()
;      (cons (first s)
;            (cons (first s)
;                  (duplicate (rest s))))))

; Problema 4 Versión loop / recur
;(def duplicate
;  (fn [s]
;   (loop [new-s s
;          result [])
;     (if (empty? new-s)
;       result
;       (recur (rest new-s)
;              (conj (conj result
;                          (first new-s)
;                    (first new-s)))))

; Problema 4 Versión API secuencias
; Duplica cada elemento de la secuencia en su misma posición.
(def duplicate
  (fn [s]
      (interleave s s)))

(deftest test-duplicate
         (is (= [1 1 2 2 3 3 4 4 5 5]
                (duplicate [1 2 3 4 5])))
         (is (= ()
                (duplicate ())))
         (is (= '(a a)
                (duplicate '(a))))
         (is (= '(a a b b c c d d e e f f g g h h)
                (duplicate '(a b c d e f g h)))))
;----------------------------------------------------------
; Problema 5
; Calcula el número de Fibonacci de forma recursiva.
(def fib
  (fn [n]
      (if (<= n 1)
        n
        (+ (fib (- n 1)) (fib (- n 2))))))


(deftest test-fib
         (is (= 0
                (fib 0)))
         (is (= 1
                (fib 1)))
         (is (= 1
                (fib 2)))
         (is (= 5
                (fib 5)))
         (is (= [0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610
                 987 1597 2584 4181 6765]
                (map fib (range 21))))
         (is (= 267914296
                (fib 42))))
;----------------------------------------------------------
; Problema 6
; Calcula la potencia de un número de forma recursiva.
(def pow
  (fn [a b]
      (cond
        (= b 0) 1
        (= b 1) a
        :else
        (*' a (pow a (- b 1))))))

(deftest test-pow
         (is (= 1 (pow 0 0)))
         (is (= 0 (pow 0 1)))
         (is (= 1 (pow 5 0)))
         (is (= 5 (pow 5 1)))
         (is (= 125 (pow 5 3)))
         (is (= 25 (pow -5 2)))
         (is (= -125 (pow -5 3)))
         (is (= 1024 (pow 2 10)))
         (is (= 525.21875 (pow 3.5 5)))
         (is (= 129746337890625 (pow 15 12)))
         (is (= 3909821048582988049 (pow 7 22)))
         (is (= 1267650600228229401496703205376N (pow 2 100))))
;----------------------------------------------------------
; Problema 7
; Verifica si una secuencia contiene solo símbolos.
(def only-symbols?
  (fn [s]
      (if (empty? (filter false? (map symbol? s)))
        true
        false)))


(deftest test-only-symbols?
         (is (= true (only-symbols? [])))
         (is (= true (only-symbols? '(a))))
         (is (= true (only-symbols? '(a b c d e))))
         (is (= false (only-symbols? '(a b c d 42 e))))
         (is (= false (only-symbols? '(42 a b c))))
         (is (= false (only-symbols? [4 8 15 16 23 42]))))
;----------------------------------------------------------
; Problema 8
; Invierte los pares en una secuencia de listas de dos elementos.
(def invert-pairs
  (fn [s]
      (if (empty? s)
        ()
        (cons [(second (first s)) (first (first s))] (invert-pairs (rest s))))))

(deftest test-invert-pairs
         (is (= () (invert-pairs ())))
         (is (= '([y x]) (invert-pairs '([x y]))))
         (is (= '([1 a][2 a][1 b][2 b])
                (invert-pairs '([a 1][a 2][b 1][b 2]))))
         (is (= '([1 January][2 February][3 March])
                (invert-pairs '([January 1][February 2][March 3])))))
;----------------------------------------------------------
; Problema 9
; Repite cada elemento de una secuencia n veces.
(def replic
  (fn [n s]
      (mapcat #(repeat n %) s)))

(deftest test-replic
         (is (= () (replic 7 [])))
         (is (= () (replic 0 '(a b c))))
         (is (= '(a a a) (replic 3 '(a))))
         (is (= [1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4]
                (replic 4 [1 2 3 4]))))
;----------------------------------------------------------
; Problema 10
; Calcula el producto punto de dos vectores.
(def dot-product
  (fn [a b]
      (reduce + (map * a b))))




(deftest test-dot-product
         (is (= 0 (dot-product [] [])))
         (is (= 42 (dot-product [6] [7])))
         (is (= 32 (dot-product [1 2 3] [4 5 6])))
         (is (= 21.45 (dot-product [1.3 3.4 5.7 9.5 10.4]
                                   [-4.5 3.0 1.5 0.9 0.0]))))
;----------------------------------------------------------
; Problema 11
; Calcula el promedio de una secuencia de números.
(def average
  (fn [s]
      (if (empty? s)
        nil
        (/ (reduce + s) (count s)))))

(deftest test-average
         (is (nil? (average [])))
         (is (= 4
                (average [4])))
         (is (= 3
                (average [5 6 1 6 0 1 2])))
         (is (= 2.5
                (average [1.7 4.5 0.0 2.0 3.4 5.0 2.5 2.2 1.2]))))
;----------------------------------------------------------
; Problema 12
; Calcula la desviación estándar, utiliza algunas de las funciones previamente hechas
(def standard-deviation
  (fn [s]
      (if (empty? s)
        nil
        (sqrt (average (map #(pow (- % (average s)) 2 ) s))))))

(deftest test-standard-deviation
         (is (nil? (standard-deviation [])))
         (is (approx= 1.87
                      (standard-deviation [6 2 3 1])
                      0.01))
         (is (approx= 12.3153
                      (standard-deviation [4 8 15 16 23 42])
                      0.0001))
         (is (approx= 7.07106
                      (standard-deviation [110 105 90 100 95])
                      0.00001))
         (is (approx= 2.983
                      (standard-deviation [9 2 5 4 12 7 8 11
                                           9 3 7 4 12 5 4 10
                                           9 6 9 4])
                      0.001)))

(run-tests)