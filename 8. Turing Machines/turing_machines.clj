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
    [{:keys [left right]} value]
    (make-tape left value right))

(defn shift-head
    [{:keys [left head right]} direction]
    (case direction
        :left (make-tape (or (butlast left) ())
                         (or (last left) \_)
                         (str head right))
        :right (make-tape (str left head)
                          (or (first right) \_)
                          (rest right))
        (throw (ex-info (str "Bad direction : " direction) {}))))

;;(make-tape "___aaaa" \b "cccc____")
(def exampl (make-tape "aaaa" \b "ccccc"))
(shift-head exampl :right)

(run-tests)
