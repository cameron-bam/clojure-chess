(ns chess.board.search
  (:require [chess.board :refer [get-all-valid-moves take-turn initial-board score-for] :as board]))

(defn make-negamax-search [{:keys [get-children score-node]}]
  (letfn [(negamax-search
            ([node depth color-int]
             (negamax-search node depth color-int (atom {:visited #{}})))
            ([node depth color-int state]
             (swap! state update :visited conj node)
             (if (= depth 0)
               (assoc node :negamax-score (* color-int (score-node node)))
               (let [children (->> (get-children node)
                                   (remove (:visited @state)))]
                 (if-not (seq children)
                   (assoc node :negamax-score (score-node node))
                   (let [{:keys [negamax-score] :as best-node}
                         (reduce (fn [{:keys [negamax-score] :as max} child]
                                   (let [result (-> (negamax-search child (dec depth) (* -1 color-int) state)
                                                    (update :negamax-score (partial * -1)))]
                                     (if (> (:negamax-score result) negamax-score) result max)))
                                 {:negamax-score Integer/MIN_VALUE} children)]
                     (assoc node :negamax-score negamax-score :best-node best-node)))))))]
    (comp :best-node negamax-search)))


(defn negamax-search-board [board]
  (let [negamax-search (make-negamax-search {:get-children (fn [board] (->> board get-all-valid-moves (map #(take-turn board %))))
                                             :score-node (partial score-for (:turn board))})
        state (atom {:visited #{}})
        move (time (-> (negamax-search board 4 1 state) meta :last-move))]
    (prn "Nodes visited" (count (:visited @state)))
    move))

(comment
  (negamax-search-board initial-board))