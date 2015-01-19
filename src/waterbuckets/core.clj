(ns waterbuckets.core
  (:gen-class)
  (require [clojure.core.match :refer [match]])
  (require [criterium.core :refer :all]))

(defn solved? [state]
  (match state
         [6 6 0] true
         [6 0 6] true
         [0 6 6] true
         [_ _ _] false))

(defn valid-state? [[x y z]]
  (and (>= x 0)
       (>= y 0)
       (>= z 0)
       (<= x 12)
       (<= y 8)
       (<= z 5)))

(defn create-steps [[x y z]]
  (-> [[0 (+ x y) z]
       [0 y (+ x z)]
       [(- x (- 8 y)) 8 z]
       [(- x (- 5 z)) y 5]
       [(+ x y) 0 z]
       [x 0 (+ y z)]
       [12 (- y (- 12 x)) z]
       [x (- y (- 5 z)) 5]
       [(+ x z) y 0]
       [x (+ y z) 0]
       [12 y (- z (- 12 x))]
       [x 8 (- z (- 8 y))]]
      (#(filter valid-state? %))
      (#(distinct %))))

;;(create-steps [12 0 0])

(defn next-steps [current history]
  (loop [steps (into (list) (create-steps current))
         history history
         next []]
    (if (empty? steps)
      [next history]
      (recur (pop steps)
             (assoc history
               (peek steps)
               (let [old (get history current [])
                     new (conj old (peek steps))
                     prev (get history (peek steps) [])
                     prev-new (conj prev current)]
                 (if (or (empty? prev)
                         (> (count prev-new)(count new)))
                   new
                   prev)))
             (if (contains? history (peek steps))
               next
               (conj next (peek steps)))))))

;;(next-steps [12 0 0] {[12 0 0] [[12 0 0]]})

(defn solve []
  (loop [state (conj clojure.lang.PersistentQueue/EMPTY [12 0 0])
         history {[12 0 0] [[12 0 0]]}]
    (let [current (peek state)
          next-states (pop state)]
      (if (not (solved? current))
        (do
          (let [[steps next-history] (next-steps current history)]
               (recur (into next-states steps)
                      next-history)))
        (do
          (get history current))))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))


(solve)
;;(with-progress-reporting (bench (solve) :verbose))
