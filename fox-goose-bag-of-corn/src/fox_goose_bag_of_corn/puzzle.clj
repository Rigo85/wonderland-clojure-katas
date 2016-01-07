(ns fox-goose-bag-of-corn.puzzle
  (use fox-goose-bag-of-corn.input :reload)
  (use fox-goose-bag-of-corn.solution :reload)
  (use fox-goose-bag-of-corn.a-star-solution :reload)
  (use [clojure.set :as set]))

(declare translate-path-to-plan initial)

(def start-pos [[[:fox :goose :corn :you] [:boat] []]])

(defn river-crossing-plan [] (translate-path-to-plan (search initial)))

;------------------------------
;------------------------------
;------------------------------

(def p (create-problem input))

(def repeat-true #(vec (repeat % true)))

(def initial (->> {:items (->> p :items count inc repeat-true)
                   :problem p
                   :path []}
               (#(assoc % :fitness (fitness %)))))

(def print-from-or-to #(if % "SHORE_A" "SHORE_B"))

(defn print-step
  [convert step] (doseq [item (first step)]
                   (prn (format "%s: from %s to %s"
                          (convert (dec item))
                          (print-from-or-to (second step))
                          (print-from-or-to (not (second step)))))))

(defn convert-to-kw
  [items] (zipmap (range -1 (count items)) (cons :empty items)))

(defn print-pretty-path
  [solution] (let [path (solution :path)
                   items (get-in solution [:problem :items])
                   convert (convert-to-kw items)]
               (doseq [step path]
                 (print-step convert step))))

(def shore-a #{:fox :goose :corn})
(def boat #{:boat :you})
(def shore-b #{})
(def convert-to-plan {0 #{} 1 #{:fox} 2 #{:goose} 3 #{:corn}})
(def all-items #{:fox :goose :corn :you :boat})

(defn process-path
  [shore-a shore-b path plan] (if (empty? path)
                                plan
                                (let [step (first path)
                                      item (->> step first first convert-to-plan)
                                      direcction (second step)
                                      shore-a* (set/difference shore-a item)
                                      boat* (set/union boat item)
                                      shore-b* (set/difference all-items shore-a* boat*)]
                                  (recur (set/union shore-b* item) shore-a* (rest path)
                                    (if direcction
                                      (conj plan
                                        [(vec shore-a*) (vec boat*) (vec shore-b*)]
                                        [(vec shore-a*) [:boat] (vec (set/union shore-b* (disj boat* :boat)))])
                                      (conj plan
                                        [(vec shore-b*) (vec boat*) (vec shore-a*)]
                                        [(vec (set/union shore-b* (disj boat* :boat))) [:boat] (vec shore-a*)]))))))

(defn translate-path-to-plan
  [solution] (process-path shore-a shore-b (solution :path) start-pos))

;to see one solution to the problem ;-)
;(print-pretty-path (search initial))

;to see one solution to 'lein test'
;(doseq [step (translate-path-to-plan (search initial))] (prn step))

