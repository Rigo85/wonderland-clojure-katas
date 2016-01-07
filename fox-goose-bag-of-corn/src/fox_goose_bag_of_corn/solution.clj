(ns fox-goose-bag-of-corn.solution)

(def boatman 0)

(defn partial-zip
  ;([start y]+) where [start < y < size]
  [start size] (map vector (repeat start) (range (inc start) size)))

(defn indexes-zip
  ;([x y]+) where [0 < x < size] and [y = x + n | 0 < n < size].
  [size] (reduce #(into % (partial-zip %2 size)) [] (range 1 size)))

(defn filter-same-shore
  ;keep only the items in the same shore.
  [items zips] (filter (fn [[i j]] (= (items i) (items j))) zips))

(defn process-zips*
  ;return [conflit-count-in-A conflit-count-in-B]
  [affinity items [shoreACount shoreBCount] zip]
  (let [keys (mapv dec zip)
        inc* (if (get-in affinity keys) 1 0)]
    (if (items (first keys))
      [(+ inc* shoreACount) shoreBCount]
      [shoreACount (+ inc* shoreBCount)])))

(defn fitness
  ;evaluate the solution (heuristic function).
  [solution] (let [items (solution :items)
                   zips (->> items count indexes-zip)
                   aff (get-in solution [:problem :affinity])
                   howMuchInShoreA (->> items rest (filter true?) count)
                   [shoreACount shoreBCount] (reduce (partial process-zips* aff items) [0 0] (filter-same-shore items zips))]
               (+ howMuchInShoreA shoreACount shoreBCount)))

(defn filter-without-current-position
  [position zips] (filter #(not (some #{position} %)) zips))

(defn filter-can-move [position items zips] (->> zips
                                              (filter-same-shore items)
                                              (filter-without-current-position position)))

(defn canMove
  ;We need to verify that when the boatman travel with some fellows, don't leave the shore in conflict.
  [solution position] (let [items (solution :items)
                            to-boolean #(get-in solution (into [:problem :affinity] (mapv dec %)))]
                        (->> items
                          count
                          indexes-zip
                          (filter-can-move position items)
                          (map to-boolean)
                          (not-any? true?))))

(defn not-way-back
  [path item] (let [last-step (last path)]
                (or
                  (empty? path)
                  (->> last-step first first (= item) not))))

(defn filter-valid-moves
  [solution i] (let [items (solution :items)
                     path (solution :path)]
                 (and
                   (= (items boatman) (items i))
                   (canMove solution i)
                   (not-way-back path i))))

(defn validMoves
  [solution] (->> (solution :items)
               count
               range
               (filter (partial filter-valid-moves solution))
               (mapv #(vector boatman %))))

(defn update-items-position
  [moves items] (reduce #(update % %2 not) items (set moves)))

(defn update-path
  [moves* items path] (let [moves (next moves*)]
                        (conj path [moves (->> moves first items)])))

(defn applyMoves
  [solution moves] (let [items (solution :items)]
                     (->> solution
                       (#(update % :items (partial update-items-position moves)))
                       (#(update % :path (partial update-path moves items)))
                       (#(assoc % :fitness (fitness %))))))

