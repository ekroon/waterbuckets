(ns waterbuckets.core
  (:gen-class)
  (require [criterium.core :refer :all]
           [waterbuckets.unrolled :as u]
           [waterbuckets.generated :as g]))

(defn -main
  "Execute with benchmark"
  [& args]
  (println "Generated version:")
  (bench (g/solve))
  (println "\n\nUnrolled version")
  (bench (u/solve)))


;;(time (map buckets->vec (solve)))
