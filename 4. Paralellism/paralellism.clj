;----------------------------------------------------------
; Problem Set #5: Parallel Programming
; Date: April 9, 2025.
; Authors:
;          A01747352 Diego Carreón Aguirre
;          A01800593 Emilio de León Vives
;----------------------------------------------------------
(ns parallelism)

; -----------------------------------------------------
; Problema 1
;(def n 150000)
; Versión secuencial
; Run #1 5334.2988
; Run #2 4925.0406
; Run #3 4897.3989
; Run #4 5248.9839
; Run #5 5256.6108
; Promedio: 5132.4666

; Versión paralela
; Run #1 418.9111
; Run #2 334.9051
; Run #3 329.148
; Run #4 354.3902
; Run #5 330.668
; Promedio: 353.60448

; Sp = T1 / Tp
; p = 16
; S16 = 5132.4666 / 353.60448 = 14.51

(defn bits
      "Returns the number of set bits (1's) in the binary representation of x"
      [x]
      (.bitCount (biginteger x)))

(defn fact-seq
      "Calculates the factorial of n sequentially and returns the number of set bits in the result"
      [n]
      (loop [i 2
             r 1]
            (if (> i n)
              (bits r)
              (recur (inc i)
                     (*' r i)))))

(defn fact-ranges
      "Divides the range [1, n] into p approximately equal chunks for parallel processing"
      [n p]
      (partition 2
                 1
                 (concat (range 1 n (quot n p))
                         [(inc n)])))

(defn fact-partial
      "Computes the product of all numbers in the range [start, end)"
      [[start end]]
      (loop [i start
             r 1]
            (if (= i end)
              r
              (recur (inc i)
                     (*' r i)))))

(defn fact-par
      "Calculates the factorial of n in parallel and returns the number of set bits in the result"
      [n]
      (let [p (.availableProcessors (Runtime/getRuntime))]
           (bits (reduce *'
                         (pmap fact-partial
                               (fact-ranges n p))))))

(time (fact-seq n))
(time (fact-par n))

; -----------------------------------------------------
; Problema 2
;(def n 60000000)
; Versión secuencial
; Run #1 7375.8781
; Run #2 7039.7924
; Run #3 7165.2623
; Run #4 7369.5702
; Run #5 7167.6072
; Promedio: 7223.622

; Versión paralela
; Run #1 1437.8706
; Run #2 1430.4687
; Run #3 1474.388
; Run #4 1440.1443
; Run #5 1494.8835
; Promedio: 1455.551

; Sp = T1 / Tp
; p = 16
; S16 = 7223.622 / 1455.551 = 4.96

(defn compute-pi-seq
      "Approximates Pi sequentially using numerical integration with n rectangles"
      [n]
      (let [width (/ 1 n)]
           (loop [i 0 sum 0]
                 (if (< i n)
                   (let [mid (* width (+ 0.5 i))
                         height (/ 4 (+ 1 (* mid mid)))]
                        (recur (inc i) (+ sum height)))
                   (*' sum width)))))

;(time(compute-pi-seq 1000))

(defn pi-ranges
      "Divides the range [0, n) into p approximately equal chunks for parallel processing"
      [n p]
      (partition 2
                 1
                 (concat (range 0 n (quot n p))
                         [n])))

(defn pi-partial
      "Computes the partial sum for a given range of rectangles [start, end)"
      [[start end] width]
      (loop [i start accum 0]
            (if (= i end)
              accum
              (let [mid (* width (+ 0.5 i))
                    height (/ 4 (+ 1 (* mid mid)))]
                   (recur (inc i) (+ accum height))))))

(defn compute-pi-par
      "Approximates Pi in parallel using numerical integration with n rectangles"
      [n]
      (let [p (.availableProcessors (Runtime/getRuntime))
            width (/ 1 n)
            ranges (pi-ranges n p)
            partials (pmap #(pi-partial % width) ranges)]
           (*' width (reduce + partials))))


(time(compute-pi-seq n))
(time(compute-pi-par n))

;-----------------------------------------------------
;Problema 3
;(def n 22)
; Versión secuencial
; Run #1 5189.7004
; Run #2 5181.4984
; Run #3 5433.548
; Run #4 5214.3489
; Run #5 5181.4895
; Promedio: 5240.12

; Versión paralela
; Run #1 2768.2874
; Run #2 2798.8556
; Run #3 2788.2728
; Run #4 2788.9066
; Run #5 2787.6653
; Promedio: 2786.40

; Sp = T1 / Tp
; p = 16
; S16 = 5240.12 / 2786.40 = 1.88
(defn palindrome?
      "Checks if a string s is a palindrome (reads the same forward and backward)"
      [s]
      (let [x (str s)]
           (= x (apply str (reverse x)))))

(defn palindrome-bin-hex?
      "Checks if a number x is a palindrome in both binary and hexadecimal representation"
      [x]
      (let [bin (Integer/toString x 2) hex (Integer/toString x 16)]
           (and (palindrome? bin) (palindrome? hex))))

(defn palindrome-ranges
      "Divides the range [0, 2^n) into p approximately equal chunks for parallel processing"
      [n p]
      (partition 2
                 1
                 (concat (range 0 (int (Math/pow 2 n)) (quot (int (Math/pow 2 n)) p))
                         [(int (Math/pow 2 n))])))

(defn palindrome-partials
      "Counts the number of bin-hex palindromes in the range [start, end)"
      [[start end]]
      (count (filter palindrome-bin-hex? (range start end))))


(defn count-palindromes-seq
      "Counts the number of bin-hex palindromes less than 2^n sequentially"
      [n]
      (let [r (int (Math/pow 2 n))]
           (count (filter palindrome-bin-hex? (range r)))))


(defn count-palindromes-par
      "Counts the number of bin-hex palindromes less than 2^n in parallel"
      [n]
      (let [p (.availableProcessors (Runtime/getRuntime))]
           (reduce + (pmap palindrome-partials (palindrome-ranges n p)))))

(time(count-palindromes-seq n))
(time(count-palindromes-par n))

; -----------------------------------------------------
; Problema 4
; n = 200,000
; p = 16

; Versión secuencial
; Run #1 T1 = 1677.6295
; Run #2 T1 = 1773.0313
; Run #3 T1 = 1836.9112
; Run #4 T1 = 1586.3765
; Run #5 T1 = 1739.1465
; Promedio: 1722.619

; Versión paralela
; Run #1 T8 = 395.6871
; Run #2 T8 = 375.0673
; Run #3 T8 = 323.9575
; Run #4 T8 = 388.8618
; Run #5 T8 = 305.9068
; Promedio: 357.896

; Sp = T1 / Tp
; p = 16
; S16 = 1722.619 /357.896 = 4.81
(defn create-random-data
      "Generates a list of n random integers between 0 and 999"
      [n]
      (repeatedly n #(rand-int 1000)))

(create-random-data 100)

(defn insertion-sort
      "Sorts a sequence s using the insertion sort algorithm"
      [s]
      (loop [new-s s
             r ()]
            (if (empty? new-s)
              r
              (let [x              (first new-s)
                    [before after] (split-with #(< % x) r)]
                   (recur (rest new-s)
                          (concat before [x] after))))))

(defn merge-algorithm
      "Merges two sorted lists a and b into a single sorted list"
      [a b]
      (loop [new-a a
             new-b b
             r     []]
            (cond
              (empty? new-a)
              (concat r new-b)

              (empty? new-b)
              (concat r new-a)

              (< (first new-a) (first new-b))
              (recur (rest new-a)
                     new-b
                     (conj r (first new-a)))

              :else
              (recur new-a
                     (rest new-b)
                     (conj r (first new-b))))))

(defn hybrid-sort-seq
      "Sorts sequence s using a hybrid of merge sort and insertion sort sequentially
       Uses insertion sort for sequences shorter than 100 elements"
      [s]
      (if (< (count s) 100)
        (insertion-sort s)
        (let [[a b] (split-at (quot (count s) 2) s)]
             (merge-algorithm (hybrid-sort-seq a)
                              (hybrid-sort-seq b)))))

(defn hybrid-sort-par
      "Sorts sequence s using a hybrid of merge sort and insertion sort in parallel
       Uses insertion sort for sequences shorter than 100 elements"
      [s]
      (if (< (count s) 100)
        (insertion-sort s)
        (let [splitted (split-at (quot (count s) 2) s)]
             (apply merge-algorithm (pmap hybrid-sort-par splitted)))))


;(def n 200000)
(def random-data (create-random-data n))
(apply <= random-data)
(println (apply <= (time (hybrid-sort-seq random-data))))
(println (apply <= (time (hybrid-sort-par random-data))))

(.availableProcessors (Runtime/getRuntime))