(ns exam)

;Problem 3
(defn contains-all-digits? [n]
  (let [digits (set (str n))]
    (every? digits "0123456789")))

(contains-all-digits? 11023456789)
(contains-all-digits? 1236)

;Problem 4
(defn mystery
  ([x] (mystery x #(* 2 %) #(inc %)))
  ([a b c] (c (b a))))

(defn wierd
  [& t]
  (let [n (count t)]
    (cond
      (zero? n) 1
      :else (inc (apply wierd (rest t))))))

(mystery 5)
(wierd 4 8 15 16 23 42)
(mystery 2 dec inc)
(mystery 10 wierd (fn [z] (* z z)))

;Problem 5
(defn how-many-div-3 [s]
  (count (filter (fn [x]
                   (= 0 (rem x 3))) s )))

;(defn how-many-div-3 [coll]
;  (count (filter #(zero? (rem % 3)) coll)))

(how-many-div-3 ())
(how-many-div-3 [6 2 3 12 4])
(how-many-div-3 '(2 5 7 11 19))
(how-many-div-3 [0 33 66 99])

;Problem 6
;
(defn divs [n]
  (loop [result ()
         i 1]
    (if (= i n)
      result
      (if (zero? (rem n i))
        (recur (cons i result) (inc i)) (recur result (inc i))))))

(divs 6)

(defn perfect? [n]
  (= n (reduce + (divs n))))

(perfect? 10)