(ns chess.board.search
  (:require [chess.board :refer [get-all-valid-moves take-turn initial-board] :as board]))

(def negate (partial * -1))

(defn make-negamax-search [{:keys [get-children score-node]}]
  (letfn [(negamax-search
            ([node depth color-int]
             (negamax-search node depth color-int (atom {:visited #{}})))
            ([node depth color-int state]
             (swap! state update :visited conj node)
             (if (= depth 0)
               (assoc node :negamax-score (score-node node))
               (let [children (->> (get-children node)
                                   (filter (complement (:visited @state))))]
                 (if-not (seq children)
                   (assoc node :negamax-score (score-node node))
                   (let [{:keys [negamax-score] :as best-node}
                         (reduce (fn [{:keys [negamax-score] :as max} child]
                                   (let [result (-> (negamax-search child (dec depth) (negate color-int) state)
                                                    (update :negamax-score negate))]
                                     (if (> (:negamax-score result) negamax-score) result max)))
                                 {:negamax-score Integer/MIN_VALUE} children)]
                     (assoc node :negamax-score negamax-score :best-node best-node)))))))]
    (comp :best-node negamax-search)))


(comment
  (let [negamax-search (make-negamax-search {:get-children (fn [board] (->> board get-all-valid-moves (map #(take-turn board %))))
                                             :score-node (fn [{:keys [score turn]}] (get score turn))})
        state (atom {:visited #{}})
        move (time (-> (negamax-search initial-board 4 1 state) meta :last-move))]
    (prn "Nodes visited" (count (:visited @state)))
    move))