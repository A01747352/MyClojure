;----------------------------------------------------------
; Problem Set #9: Turing Machines
; Date: June 9, 2025.
; Authors:
;          A01747352 Diego Carreón Aguirre
;          A01800593 Emilio De León Vives
;----------------------------------------------------------
(ns turing-machines
  (:require [clojure.test :refer [deftest is run-tests]])
  (:import (java.io Writer)))

(defrecord TM [initial-state accept-states transitions])

(defrecord Tape [left head right]
  Object
  (toString [_] (format "%s[%s]%s" left head right)))

(defmethod print-method Tape
  [self ^Writer writer]
  (.write writer (str self)))

(defn make-tape
  "Creates a Tape object from a string or from left, head, and right components.
   Trims leading and trailing underscores for a clean representation."
  ([s]
   (let [result (drop-while #(= % \_) s)]
     (make-tape ""
                (if (empty? result) \_ (first result))
                (rest result))))
  ([left head right]
   (let [new-left (drop-while #(= % \_) left)
         new-right (reverse (drop-while #(= % \_) (reverse right)))]
     (->Tape (apply str new-left)
             head
             (apply str new-right)))))

(defn write-tape
  "Writes a symbol to the current head position of the tape."
  [{:keys [left right]} value]
  (make-tape left value right))

(defn shift-head
  "Shifts the tape head left or right, updating the tape structure accordingly.
   Throws an exception if the direction is invalid."
  [{:keys [left head right]} direction]
  (case direction
    :left (make-tape (or (butlast left) ())
                     (or (last left) \_)
                     (str head right))
    :right (make-tape (str left head)
                      (or (first right) \_)
                      (rest right))
    (throw (ex-info (str "Bad direction: " direction) {}))))

(defn accepts
  "Simulates a Turing Machine on the given input.
   Returns the final tape as string if input is accepted, otherwise nil."
  [{:keys [initial-state accept-states transitions]} input]
  (loop [tape (make-tape input)
         current-state initial-state]
    (if (contains? accept-states current-state)
      (str tape)
      (if-let [[write-symbol direction new-state]
               ((transitions current-state) (.head tape))]
        (recur (shift-head (write-tape tape write-symbol) direction)
               new-state)
        nil))))

; Problema 1
(def tm-1 (->TM :q0
                #{:q2}
                {:q0 {\a [\a :right :q1]
                      \_ [\_ :left :q2]}
                 :q1 {\a [\a :right :q0]}}))

(deftest test-problem1
  (is (= "[_]"
         (accepts tm-1 "")))
  (is (= "a[a]"
         (accepts tm-1 "aa")))
  (is (= "aaaaaaa[a]"
         (accepts tm-1 "aaaaaaaa")))
  (is (= "aaaaaaaaaaaaaaaaaaaaaaaaa[a]"
         (accepts tm-1 "aaaaaaaaaaaaaaaaaaaaaaaaaa")))
  (is (nil? (accepts tm-1 "a")))
  (is (nil? (accepts tm-1 "aaa")))
  (is (nil? (accepts tm-1 "aaaaaaa")))
  (is (nil? (accepts tm-1 "aaaaaaaaaaaaaaaaaaaaaaaaa"))))

; Problema 2
(def tm-2 (->TM :q0
                #{:q3}
                {:q0 {\0 [\0 :right :q1]
                      \1 [\1 :right :q2]}
                 :q1 {\1 [\1 :right :q1]
                      \_ [\_ :left :q3]}
                 :q2 {\0 [\0 :right :q2]
                      \_ [\_ :left :q3]}}))

(deftest test-problem2
  (is (= "[0]"
         (accepts tm-2 "0")))
  (is (= "[1]"
         (accepts tm-2 "1")))
  (is (= "1[0]"
         (accepts tm-2 "10")))
  (is (= "0111111111[1]"
         (accepts tm-2 "01111111111")))
  (is (nil? (accepts tm-2 "")))
  (is (nil? (accepts tm-2 "00")))
  (is (nil? (accepts tm-2 "100000000001")))
  (is (nil? (accepts tm-2 "10011010100101011"))))

; Problema 3
(def tm-3 (->TM :q0
                #{:q3}
                {:q0 {\0 [\0 :right :q0]
                      \1 [\1 :right :q0]
                      \_ [\_ :left :q1]}
                 :q1 {\0 [\1 :right :q2]
                      \1 [\0 :left :q1]
                      \_ [\1 :right :q2]}
                 :q2 {\0 [\0 :right :q2]
                      \_ [\_ :left :q3]}}))

(deftest test-problem3
  (is (= "[1]"
         (accepts tm-3 "0")))
  (is (= "1[0]"
         (accepts tm-3 "1")))
  (is (= "1[1]"
         (accepts tm-3 "10")))
  (is (= "10[0]"
         (accepts tm-3 "11")))
  (is (= "100[1]"
         (accepts tm-3 "1000")))
  (is (= "10101011[0]"
         (accepts tm-3 "101010101")))
  (is (= "000000000[1]"
         (accepts tm-3 "0000000000")))
  (is (= "11111000[0]"
         (accepts tm-3 "111101111")))
  (is (= "101001101[1]"
         (accepts tm-3 "1010011010")))
  (is (= "1000000000000000[0]"
         (accepts tm-3 "1111111111111111"))))

; Problema 4

6

(run-tests)

