(ns chess.board.search
  (:require [chess.board :refer [get-all-valid-moves initial-board score-for take-turn] :as board]
            [clj-async-profiler.core :as prof])
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

(defrecord ScoredMove [^Integer move ^Integer score following-move])

(defrecord Context [visited visited-count semaphore])

(defn make-search [{:keys [get-children score-node sort-children parallelism]}]
  (letfn [(get-unvisited [node context]
            (->> (get-children node)
                 (remove (:visited @context))))
          (search
           ([node depth]
            (search node depth true))
           ([node depth maximize?]
            (search node depth maximize? (atom nil)))
           ([node depth maximize? context]
            (search node depth maximize? context nil nil))
           ([node depth maximize? context alpha beta]
            (when (= @context nil)
              (reset! context (->Context #{} 0 (Semaphore. (or parallelism (* 2 (.availableProcessors (Runtime/getRuntime))))))))
            (when (-> (Thread/currentThread) .isInterrupted)
              (throw (InterruptedException.)))
            (swap! context #(-> %
                                (update :visited conj node)
                                (update :visited-count inc)))
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
                                            cur-best-score (:score cur-best)
                                            searches (max permits 1)
                                            search-fn #(search %
                                                               (dec depth)
                                                               (not maximize?)
                                                               context
                                                               (when maximize? cur-best-score)
                                                               (when-not maximize? cur-best-score))
                                            prune? #(or (and maximize? (int? beta) (> (:score %) beta))
                                                        (and (not maximize?) (int? alpha) (< (:score %) alpha)))
                                            searches! (atom nil)
                                            _ (reset! searches! (cond->> children
                                                                  :always (take searches)
                                                                  (> permits 0) (mapv (fn [node]
                                                                                        (future
                                                                                          (let [res (search-fn node)]
                                                                                            (when (prune? res)
                                                                                              (run! (fn [f] (.cancel f true)) @searches!))
                                                                                            res))))
                                                                  (<= permits 0) (map search-fn)))
                                            best-node (cond->> @searches!
                                                        (> permits 0) (#(->> % (map (fn [result] (try @result (catch Exception _ nil) (finally (context->restore-permit context)))))
                                                                             (filter some?)))
                                                        :always (reduce #(try (if ((if maximize? > <) (:score %1) (:score %2)) %1 %2) (catch Exception e (prn %1 %2) (throw e))) cur-best))]
                                        (recur best-node (when-not (prune? best-node)
                                                           (drop searches children))))))]
                    (->ScoredMove node (:score best-node) best-node)))))))]
    (comp :following-move search)))

(defn search-board [board]
  (let [score-node (partial score-for (:turn board))
        search (make-search {:get-children (fn [board] (->> board get-all-valid-moves (map #(take-turn board %))))
                             :score-node score-node
                             :sort-children (fn [maximize? children] (sort-by (cond->> score-node maximize? (comp -)) children))})
        state (atom nil)
        move (time (-> (search board 4 true state) :move meta :last-move))]
    (prn "Nodes visited" (:visited-count @state))
    move))

(defrecord TestNode [score depth breadth])

(def subtractor (int (/ Integer/MAX_VALUE 2)))

(defn make-test-node [depth breadth]
  (->TestNode (java.lang.Math/subtractExact ^Integer (.nextInt (java.util.concurrent.ThreadLocalRandom/current) Integer/MAX_VALUE) ^Integer subtractor) depth breadth))

(defn make-test-tree
  ([depth breadth]
   (make-test-node depth breadth)))


(defn test-search [tree]
  (let [search (make-search {:get-children #(let [depth (dec (:depth %))] 
                                              (if (< depth 0)
                                                []
                                                (loop [children []]
                                                  (if (>= (count children) (:breadth %)) children (recur (conj children (make-test-node depth (:breadth %))))))))
                             :score-node :score
                             :sort-children (fn [maximize? children] (cond-> (sort-by :score children) maximize? reverse))})
        state (atom nil)
        move (time (search tree 6 true state))]
    (prn "Nodes visited" (:visited-count @state))
    (-> move
        (update :move dissoc :children)
        (dissoc :following-move))))

(comment
  (prof/profile (test-search (make-test-tree 5 31))))

(defn run-test-search! [& _]
  (test-search (make-test-tree 4 31)))

(defn search-board! [& _]
  (search-board initial-board))