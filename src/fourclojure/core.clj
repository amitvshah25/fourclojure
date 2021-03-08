(ns fourclojure.core
  (:gen-class))

(defn comparision [c a b]
  (cond
    (c a b) :lt
    (c b a) :gt
    :else :eq))

(defn digits [a b]
  (letfn [(d [n]
            (if (< n 10) [n]
              (conj (d (quot n 10)) (mod n 10))))]
    (d (* a b))))

(defn sym-diff [s1 s2]
  (let [i (set (filter s1 s2))]
    (set (concat (remove i s1) (remove i s2)))))

(defn binary-num [n]
  (Integer/parseInt n 2))

(defn infix-calc [p1 o p2 & expr]
  (reduce (fn [r [op p]]
            (op r p)) (o p1 p2) (partition 2 expr)))

(defn pascals-triangle 
  ([n] (pascals-triangle [[1]]))
  ([[_ last-row :as p]]
   (conj p (reduce #() last-row))))

(defn fibonacci [n]
  (loop [res [1 1]]
    (if (> n (count res))
      (recur (conj res (+ (last res) (last (butlast res)))))
      res)))

(defn my-intersection [s1 s2]
  (filter s1 s2))

(defn longest-incr-subseq [s]
  (let [split-sequences (loop [[a & remaining] s
                               res []]
                          (if a
                            (if (last (last res))
                              (if (= (inc (last (last res))) a)
                                (recur remaining (conj (into [] (butlast res)) (conj (last res) a)))
                                (recur remaining (conj res [a])))
                              (recur remaining (conj res [a])))
                            res))]
    (->> split-sequences
         (filter #(>= (count %) 2))
         (sort #(compare (count %2) (count %1)))
         first
         (into []))))

(defn my-merge [f first-map & remaining-maps]
  (reduce (fn [m1 m2]
            (let [m2-keys (keys m2)]
              (reduce (fn [conjed-map m2-key]
                        (if (contains? conjed-map m2-key)
                          (assoc conjed-map m2-key (f (get conjed-map m2-key) (get m2 m2-key)))
                          (assoc conjed-map m2-key (get m2 m2-key))))
                      m1 m2-keys)))
          first-map
          remaining-maps))

(defn perfect-squares [csv]
  (->> (clojure.string/split csv #",")
       (map #(Integer/parseInt %))
       (map #(Math/sqrt %))
       (map str)
       (map #(clojure.string/split % #"."))
       (filter #(= 0 (last (clojure.string/split % #"."))))))
