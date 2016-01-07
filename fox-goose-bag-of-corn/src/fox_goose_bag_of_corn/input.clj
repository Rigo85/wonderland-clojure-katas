(ns fox-goose-bag-of-corn.input
  (require [clojure.string :as s]))

(def input "input.txt")

(def re #"\s|,|\[|\]")

(def convert {"x" true "X" true})

(defn process-line
  ([line] (process-line line re))
  ([line re] (->> re
               (s/split line)
               (filterv not-empty))))

(def process-row (partial mapv convert))

(def standarize (partial mapv process-row))

(def non-standard-affinity (partial mapv process-line))

(defn affinity
  [lines] (->> lines
            non-standard-affinity
            standarize))

(defn items
  [line] (mapv (comp keyword s/lower-case) (process-line line)))

(defn create-problem
  [file] (let [[it & aff] (->> input slurp s/split-lines)]
           {:items (items it) :affinity (affinity aff)}))

