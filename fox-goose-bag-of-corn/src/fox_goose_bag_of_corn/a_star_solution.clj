(ns fox-goose-bag-of-corn.a-star-solution
  (use fox-goose-bag-of-corn.solution :reload))

(def goal #(every? false? (% :items)))

(defn process-move
  [solution move [open closed]] (let [next* (applyMoves solution move)]
                                  (if (contains? closed next*)
                                    (let [prior (closed next*)]
                                      (if (< (next* :fitness) (prior :fitness))
                                        (let [new-closed (disj closed prior)]
                                          (.add open next*)
                                          [open new-closed])
                                        [open closed]))
                                    (do
                                      (.add open next*)
                                      [open closed]))))

(defn process-moves
  [solution open closed] (reduce #(process-move solution %2 %) [open closed] (validMoves solution)))

(defn process-open
  [[open closed]] (if (or (.isEmpty open) (goal (.peek open)))
                  (if (.isEmpty open)
                    :no-solution (.peek open))
                  (let [solution (.poll open)
                        closed* (conj closed solution)]
                    (recur (process-moves solution open closed*)))))

(defn search
  ;Algorithm A*
  ;Algorithms in a Nutshell by George T. Heiniman, Gary Pollice and Stanley Selkow. 2009. page 194.
  [initial] (let [open (java.util.PriorityQueue. 100 #(compare (% :fitness) (%2 :fitness)))
                  closed (sorted-set-by #(compare (% :items) (%2 :items)))
                  _ (.add open initial)]
              (process-open [open closed])))

