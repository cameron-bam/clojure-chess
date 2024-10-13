(ns user
  (:require [clj-async-profiler.core :as prof]
            [clojure.math :refer [pow]]
            [chess.board.search :refer [make-test-node]]))

(prof/serve-ui "0.0.0.0" 8080)

(comment
  (time
   (prof/profile
    (let [ops (pow 10 9)
          concurrency (-> (Runtime/getRuntime) .availableProcessors)
          partitions (mapv (fn [_] (/ ops concurrency)) (take concurrency (range)))]
      (->> partitions
           (mapv (fn [data]
                   (future
                     (loop [data data]
                       (when (> data 0)
                         (make-test-node 3 3)
                         (recur (dec data)))))))
           (mapv deref))))))