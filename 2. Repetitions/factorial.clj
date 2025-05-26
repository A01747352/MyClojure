(ns factorial)

; Factorial versión recursiva
(defn fact-v1
      [n]
      (if (zero? n)
        1
        (*' n
            (fact-v1 (dec n)))))

; Factorial version loop/recur (iterativa)
(defn fact-v2
      [n]
      (loop [accum 1
             i n]
            (if (zero? i)
              accum
              (recur (*' accum i) (dec i)))))

; Factorial versión API de secuencias
(defn fact-v3
      [n]
      (reduce *' (range 1 (inc n))))


(fact-v3 0)
(fact-v3 1)
(fact-v3 3)
(fact-v3 5)
(fact-v3 20)
(fact-v3 100000)
