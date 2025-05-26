(ns parallelism)

; -----------------------------------------------------
; Problema 1
(defn bits
      [x]
      (.bitCount (biginteger x)))

(defn fact-seq
      [n]
      (loop [i 2
             r 1]
            (if (> i n)
              (bits r)
              (recur (inc i)
                     (*' r i)))))

; (time (fact-seq n))

(defn fact-ranges
      [n p]
      (partition 2
                 1
                 (concat (range 1 n (quot n p))
                         [(inc n)])))

(defn fact-partial
      [[start end]]
      (loop [i start
             r 1]
            (if (= i end)
              r
              (recur (inc i)
                     (*' r i)))))

(defn fact-par
      [n]
      (let [p (.availableProcessors (Runtime/getRuntime))]
           (bits (reduce *'
                         (pmap fact-partial
                               (fact-ranges n p))))))

;(time (fact-seq n))
;(time (fact-par n))

; -----------------------------------------------------
; Problema 2
(defn compute-pi-seq [n]
      (let [width (/ 1 n)]
           (loop [i 0 sum 0]
                 (if (< i n)
                   (let [mid (* width (+ 0.5 i))
                         height (/ 4 (+ 1 (* mid mid)))]
                        (recur (inc i) (+ sum height)))
                   (*' sum width)))))

(time(compute-pi-seq 1000))

(defn pi-ranges [n p]
      (partition 2
                 1
                 (concat (range 0 n (quot n p))
                         [n])))

(defn pi-partial [[start end] width]
      (loop [i start accum 0]
            (if (= i end)
              accum
              (let [mid (* width (+ 0.5 i))
                    height (/ 4 (+ 1 (* mid mid)))]
                   (recur (inc i) (+ accum height))))))

(defn compute-pi-par [n]
      (let [p (.availableProcessors (Runtime/getRuntime))
            width (/ 1 n)
            ranges (pi-ranges n p)
            partials (pmap #(pi-partial % width) ranges)]
           (*' width (reduce + partials))))


(time(compute-pi-seq 10001))
(time(compute-pi-par 10001))

; -----------------------------------------------------
;Problema 3
(defn palindrome? [s]
      (let [x (str s)]
           (= x (apply str (reverse x)))))

(defn palindrome-bin-hex? [x]
      (let [bin (Integer/toString x 2) hex (Integer/toString x 16)]
           (and (palindrome? bin) (palindrome? hex))))

(defn palindrome-ranges
      [n p]
      (partition 2
                 1
                 (concat (range 0 (int (Math/pow 2 n)) (quot (int (Math/pow 2 n)) p))
                         [(int (Math/pow 2 n))])))

(defn palindrome-partials
      [[start end]]
      (count (filter palindrome-bin-hex? (range start end))))


(defn count-palindromes-seq [n]
      (let [r (int (Math/pow 2 n))]
           (count (filter palindrome-bin-hex? (range r)))))


(defn count-palindromes-par
      [n]
      (let [p (.availableProcessors (Runtime/getRuntime))]
           (reduce + (pmap palindrome-partials (palindrome-ranges n p)))))

(time(count-palindromes-seq 21))
(time(count-palindromes-par 21))|

; -----------------------------------------------------
; Problema 4

(defn create-random-data
      [n]
      (repeatedly n #(rand-int 1000)))

(create-random-data 100)

(defn insertion-sort
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

(defn hybrid-sort-seq [s]
      (if (< (count s) 100)
        (insertion-sort s)
        (let [[a b] (split-at (quot (count s) 2) s)]
             (merge-algorithm (hybrid-sort-seq a)
                              (hybrid-sort-seq b)))))

(defn hybrid-sort-par [s]
      (if (< (count s) 100)
        (insertion-sort s)
        (let [splitted (split-at (quot (count s) 2) s)]
             (apply merge-algorithm (pmap hybrid-sort-par splitted)))))


(def n 200000)
(def random-data (create-random-data n))
(apply <= random-data)
(println (apply <= (time (hybrid-sort-seq random-data))))
(println (apply <= (time (hybrid-sort-par random-data))))

(.availableProcessors (Runtime/getRuntime))
