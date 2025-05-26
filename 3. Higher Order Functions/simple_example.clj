(ns simple-example)

(defn $map
      [fun seq]
      (if (empty? seq)
        ()
        (cons (fun (first seq))
              ($map fun (rest seq)))))

($map inc [4 8 15])
($map #(* % 2) [1 8 15 16 23 42])

(defn suma
      [base]
      (fn [x] (+ x base)))

(def z (suma 10))
(z 4)
(z 100)
(def w (suma 15))
(w 4)
(z 50)
(w 50)

((suma 20) 5)

(defn composite
      [f g]
      (fn [x] (f (g x))))

(defn f1 [x] (* 2 x))
(defn f2 [x] (+ x 5))
(def f3 (composite f1 f2))
(def f4 (composite f2 f1))
(def f5 (composite f3 f4))

(f1 1)
(f2 1)
(f3 1)
(f4 1)
(f5 1)

(defn z
      [a b c d e]
      (+ (* a b)
         (/ c d)
         (inc e)))

(defn z-curry
      [a]
      (fn [b]
          (fn [c]
              (fn [d]
                  (fn [e]
                      (+ (* a b)
                         (/ c d)
                         (inc e)))))))

(z 1 2 3 4 5)
(((((z-curry 1) 2) 3) 4) 5)