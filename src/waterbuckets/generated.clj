(ns waterbuckets.generated
  (:gen-class)
  (require [clojure.core.match :refer [match]])
  (require [criterium.core :refer :all]))

(defprotocol IBucket
  (-current [_])
  (-fits [_]))

(defrecord Bucket [max current]
  IBucket
  (-current [this] (:current this))
  (-fits [this] (- (:max this) (:current this))))

(defn buckets->vec [state]
  (into [] (map :current state)))

(defn transfer [from to]
  (min (-current from) (-fits to)))

(defn solved? [[x y z]]
  (match [(:current x) (:current y) (:current z)]
         [6 6 0] true
         [6 0 6] true
         [0 6 6] true
         [_ _ _] false))

(defn valid-state? [state]
  (not-any? #(or (>= (:current %) 0)
                 (<= (:current %) (:max %)))))

(defn permutate-state [state]
  (for [x state y state z state
        :when (and (not= x y)
                   (not= x z)
                   (not= y z))]
    [x y z]))

(defn sort-state [state]
  (reverse (sort-by :max  state)))

(defn create-steps [state]
  (let [steps (permutate-state state)]
    (remove #(= state %)
            (map (fn [[fst snd trd]]
                   (let [t (transfer fst snd)]
                     (into []
                           (sort-state
                            [(assoc fst :current (- (:current fst) t))
                             (assoc snd :current (+ (:current snd) t))
                             trd]))))
                 steps))))

(defn next-steps [current history]
  (loop [steps (into (list) (create-steps current))
         history history
         next []]
    (if (empty? steps)
      [next history]
      (let [in-history? (contains? history (peek steps))
            head (peek steps)]
        (if in-history?
          (recur (pop steps)
                 history
                 next)
          (recur (pop steps)
                 (assoc history
                   head
                   (conj (get history current []) head))
                 (conj next head)))))))

(defn solve []
  (loop [state (conj clojure.lang.PersistentQueue/EMPTY [(->Bucket 12 12)
                                                         (->Bucket 8 0)
                                                         (->Bucket 5 0)])
         history {[(->Bucket 12 12)
                   (->Bucket 8 0)
                   (->Bucket 5 0)]
                  [[(->Bucket 12 12)
                    (->Bucket 8 0)
                    (->Bucket 5 0)]]}]
    (let [current (peek state)
          next-states (pop state)]
      (if (not (solved? current))
        (do
          (let [[steps next-history] (next-steps current history)]
               (recur (into next-states steps)
                      next-history)))
        (do
          (get history current))))))

(time (map buckets->vec (solve)))
