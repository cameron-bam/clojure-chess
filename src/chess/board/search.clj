(ns chess.board.search
  (:require [chess.board :refer [get-all-valid-moves take-turn initial-board score-for] :as board]))

(defn make-search [{:keys [get-children score-node sort-children]}]
  (letfn [(get-unvisited [node context]
            (->> (get-children node)
                 (remove (comp (:visited @context) hash-unordered-coll))))
          (search
           ([node depth]
            (search node depth true))
           ([node depth maximize?]
            (search node depth maximize? (atom {:visited #{} :max-unvisited-children 0})))
           ([{::keys [alpha beta] :as node} depth maximize? context]
            (swap! context update :visited conj (hash-unordered-coll node))
            (if (<= depth 0)
              (assoc node ::score (score-node node))
              (let [children (->> (get-unvisited node context) (sort-children maximize?))]
                (if-not (seq children)
                  (assoc node ::score (score-node node))
                  (let [best-node (reduce (fn [{::keys [score] :as best} child]
                                            (let [result (search (assoc child (if maximize? ::alpha ::beta) score) (dec depth) (not maximize?) context)
                                                  {::keys [score] :as best} (if ((if maximize? > <) (::score result) score) result best)]
                                              (cond-> best
                                                (or (and maximize? (int? beta) (> score beta))
                                                    (and (not maximize?) (int? alpha) (< score alpha)))
                                                reduced)))
                                          {::score (if maximize? Integer/MIN_VALUE Integer/MAX_VALUE)}
                                          children)]
                    (swap! context update :max-unvisited-children max (count children))
                    (assoc node ::score (::score best-node) :best-node best-node)))))))]
                     (comp :best-node search)))


(defn search-board [board]
  (let [score-node (partial score-for (:turn board))
        search (make-search {:get-children (fn [board] (->> board get-all-valid-moves (map #(take-turn board %))))
                             :score-node score-node
                             :sort-children (fn [maximize? children] (cond-> (sort-by score-node children) maximize? reverse))})
        state (atom {:visited #{} :max-unvisited-children 0})
        move (time (-> (search board 4 true state) meta :last-move))]
    (prn "Nodes visited" (count (:visited @state)))
    (prn "Max unvisited children" (:max-unvisited-children @state))
    move))

(defn test-search [tree]
  (let [search (make-search {:get-children :children
                             :score-node :score
                             :sort-children (fn [maximize? children] (cond-> (sort-by :score children) maximize? reverse))})
        state (atom {:visited #{} :max-unvisited-children 0})
        move (time (-> (search tree 3 true state)))]
    (prn "Nodes visited" (count (:visited @state)))
    (dissoc move :children :best-node)))

(defn make-test-node []
  {:score (- (rand-int 10) 5)})

(defn make-test-tree
  ([depth breadth]
   (make-test-tree (make-test-node) depth breadth))
  ([node depth breadth]
   (cond-> node
     (> depth 0)
     (assoc :children (for [i (range 0 breadth)]
                        (make-test-tree (make-test-node) (dec depth) breadth))))))

(comment
  (search-board initial-board)
  (test-search (make-test-tree 5 31))
  (+ 1 1)
  )