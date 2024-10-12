(ns chess.board.search
  (:require [chess.board :refer [get-all-valid-moves take-turn initial-board score-for] :as board])
  (:import (java.util.concurrent Semaphore)))


(defn context->get-permits [max-permits context] 
  (loop [acquired-permits 0]
    (if (>= acquired-permits max-permits)
      max-permits
      (let [permits-to-aquire 1]
        (if-not (-> @context :semaphore (.tryAcquire permits-to-aquire))
          acquired-permits
          (recur (+ acquired-permits permits-to-aquire)))))))

(defn context->restore-permit [context]
  (-> @context :semaphore .release))

(defrecord ScoredMove [move score following-move])

(defn make-search [{:keys [get-children score-node sort-children parallelism]}]
  (letfn [(get-unvisited [node context]
            (->> (get-children node)
                 (remove (:visited @context))))
          (search
           ([node depth]
            (search node depth true))
           ([node depth maximize?]
            (search node depth maximize? (atom {})))
           ([node depth maximize? context]
            (search node depth maximize? context nil nil))
           ([node depth maximize? context alpha beta]
            (when (= @context {})
              (swap! context merge {:visited #{} :semaphore (Semaphore. (or parallelism (* 2 (.availableProcessors (Runtime/getRuntime)))))}))
            (swap! context update :visited conj node)
            (if (<= depth 0)
              (->ScoredMove node (score-node node) nil)
              (let [children (->> (get-unvisited node context) (sort-children maximize?))]
                (if-not (seq children)
                  (->ScoredMove node (score-node node) nil)
                  (let [best-node (loop [cur-best (->ScoredMove nil (if maximize? Integer/MIN_VALUE Integer/MAX_VALUE) nil)
                                         children children]
                                    (if (empty? children)
                                      cur-best
                                      (let [permits (context->get-permits (count children) context)
                                            cur-best-score (.-score cur-best)
                                            searches (max permits 1)
                                            search-fn #(search %
                                                               (dec depth)
                                                               (not maximize?)
                                                               context 
                                                               (when maximize? cur-best-score)
                                                               (when-not maximize? cur-best-score))
                                            best-node (cond->> children
                                                        :always (take searches)
                                                        (> permits 0) (#(->> %
                                                                             (mapv (fn [node] (future (let [res (search-fn node)]
                                                                                                        (context->restore-permit context)
                                                                                                        res))))
                                                                             (map deref)))
                                                        (<= permits 0) (map search-fn)
                                                        :always (reduce #(try (if ((if maximize? > <) (.-score %1) (.-score %2)) %1 %2) (catch Exception e (prn %1 %2) (throw e))) cur-best))
                                            score (.-score best-node)]
                                        (recur best-node (when-not (or (and maximize? (int? beta) (> score beta))
                                                                       (and (not maximize?) (int? alpha) (< score alpha)))
                                                           (drop searches children))))))]
                    (->ScoredMove node (.-score best-node) best-node)))))))]
    (comp #(.-following-move %) search)))


(defn search-board [board]
  (let [score-node (partial score-for (:turn board))
        search (make-search {:get-children (fn [board] (->> board get-all-valid-moves (map #(take-turn board %))))
                             :score-node score-node
                             :sort-children (fn [maximize? children] (cond-> (sort-by score-node children) maximize? reverse))})
        state (atom {})
        move (time (-> (search board 4 true state) :move meta :last-move))]
    (prn "Nodes visited" (count (:visited @state)))
    move))

(defrecord TestNode [score children])

(defn test-search [tree]
  (let [search (make-search {:get-children #(.-children %)
                             :score-node #(.-score %)
                             :sort-children (fn [maximize? children] (cond-> (sort-by #(.-score %) children) maximize? reverse))})
        state (atom {})
        move (time (-> (search tree 6 true state)))]
    (prn "Nodes visited" (count (:visited @state)))
    (-> move
        (update :move dissoc :children)
        (dissoc :following-move))))

(defn make-test-node [children]
  (->TestNode (- (rand-int 10) 5) children))

(defn make-test-tree
  ([depth breadth]
   (make-test-node (if (<= depth 0) [] (for [_ (range 0 breadth)]
                                         (make-test-tree (dec depth) breadth))))))

(comment
  (search-board initial-board)

  (def test-tree (make-test-tree 6 31))
  (test-search test-tree)
  (+ 1 1))


(comment
  (with-meta (TestNode 1 []) {:test 1}))