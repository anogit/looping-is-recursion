(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [val exp]
          (if (zero? exp)
            val
            (recur (* base val) (dec exp))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (if (empty? (rest a-seq))
    (last a-seq)
    (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (let [v1 (first seq1)
        v2 (first seq2)]
    (cond
      (and (empty? seq1) (empty? seq2)) true
      (or (empty? seq1) (empty? seq2)) false
      (== v1 v2) (recur (rest seq1) (rest seq2))
      :else false)))

(defn find-first-index [f a-seq]
  (loop [index 0
         seq   a-seq]
    (cond
      (empty? seq) nil
      (f (first seq)) index
      :else (recur (inc index) (rest seq)))))

(defn avg [a-seq]
  (loop [index 0
         avg   0
         seq   a-seq]
    (cond
      (empty? seq) (/ avg index) ; (/ avg (count a-seq))
      :else (recur (inc index) (+ avg (first seq)) (rest seq)))))

(defn parity [a-seq]
  (let [toggle (fn [a-set element]
         (if (contains? a-set element)
           (disj a-set element)
           (conj a-set element)))]
    (loop [states #{}
           seq    a-seq]
      (if (empty? seq)
        states
        (recur (toggle states (first seq)) (rest seq))))))

(defn fast-fibo [n]
  ":(")

(defn cut-at-repetition [a-seq]
  [":("])

