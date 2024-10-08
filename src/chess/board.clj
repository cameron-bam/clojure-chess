(ns chess.board)

(def RANKS "87654321")
(def FILES "ABCDEFGH")

(def array-coords->board-coords
  (memoize
   (fn array-coords->board-coords* [[x y]]
     {:rank (str (get RANKS x))
      :file (str (get FILES y))})))

(defn ->coords-map [coords]
  (if (map? coords)
    coords
    {:rank (str (.charAt coords 0))
     :file (str (.charAt coords 1))}))

(def board-coords->array-coords
  (memoize
   (fn board-coords->array-coords* [{:keys [rank file]}]
     [(try (int (.indexOf (seq RANKS) (.charAt rank 0))) (catch Exception _ -1))
      (try (int (.indexOf (seq FILES) (.charAt file 0))) (catch Exception _ -1))])))

(defn includes [x [start end]]
  (and (>= x start) (< x end)))

(defn array-coords-in-bounds? [coords]
  (when (every? #(includes % [0 8]) coords)
    coords))

(defn in-bounds? [coords]
  (-> coords board-coords->array-coords array-coords-in-bounds?))

(def piece-aliases {:r :rook :kn :knight :b :bishop :q :queen :k :king :p :pawn})
(def last-ranks {:white "8" :black "1"})

(defn alias-vec->pieces [color array]
  (->> array
       (map
        (fn [[rank row]]
          (map-indexed
           (fn [j cell]
             {:coords (array-coords->board-coords [(- 8 rank) j])
              :color color
              :piece (get piece-aliases cell)}) row)))
       (apply concat)))

(def initial-pieces (set
                     (concat
                      (alias-vec->pieces :white [[8 [:r :kn :b :q :k :b :kn :r]]
                                                 [7 [:p :p :p :p :p :p :p :p]]])
                      (alias-vec->pieces :black [[2 [:p :p :p :p :p :p :p :p]]
                                                 [1 [:r :kn :b :q :k :b :kn :r]]]))))
(defn normalize-board [board]
  (->> board
       (into {} (map (fn [[k v]]
                       (if (keyword? k)
                         (case k
                           :castling-rights [k (into #{} (map ->coords-map) v)]
                           :en-passante [k (->coords-map v)]
                           [k v])
                         [(->coords-map k) (update v :coords ->coords-map)]))))))

(def initial-board (-> (reduce (fn [board {:keys [coords] :as piece}] (assoc board coords piece)) {} initial-pieces)
                       (assoc :turn :white :castling-rights #{"8A" "8E" "8H" "1A" "1E" "1H"} :score {:white 0 :black 0})
                       (normalize-board)))

(defn opposing-color [color]
  (if (= color :white) :black :white))

(defn traverse-step [start [xd yd]]
  (let [[xs ys] (board-coords->array-coords start)]
    (array-coords->board-coords [(+ xs xd) (+ ys yd)])))

(defn traverse [board start direction]
  (let [start-piece (get board start)]
    (->> (iterate (fn [{:keys [finish capture]}]
                    (when-not capture ;; stop after capturing
                      (let [finish (traverse-step finish direction)]
                        (when (in-bounds? finish) ;; only include inbounds moves
                          (let [{:keys [color] :as piece} (get board finish)]
                            (when-not (= (:color start-piece) color) ;; only allow capture of opponents pieces
                              (cond-> {:finish finish} piece (assoc :capture piece))))))))
                  {:finish start})
         (take-while some?)
         (rest)
         (map #(-> % (assoc :start start) (merge (dissoc start-piece :coords)))))))

(defn build-moves [board start steps directions]
  (cond->> directions
    :always (map (partial traverse board start))
    (> steps 0) (map (partial take steps))
    :always (apply concat)))

(defn basic-move-dispatch [board start]
  (:piece (get board start)))

(defmulti get-basic-moves #'basic-move-dispatch)

(def rook-directions #{[-1 0] [1 0] [0 -1] [0 1]})
(defmethod get-basic-moves :rook [board start]
  (build-moves board start -1 rook-directions))

(comment
  (get-basic-moves (normalize-board {"1A" {:coords "1A" :piece :rook :color :white}
                                     "2A" {:coords "2A" :piece :pawn :color :white}
                                     "8A" {:coords "8A" :piece :rook :color :black}}) (->coords-map "1A")))

(def knight-directions #{[2 1] [2 -1] [-2 1] [-2 -1] [1 2] [1 -2] [-1 2] [-1 -2]})
(defmethod get-basic-moves :knight [board start]
  (build-moves board start 1 knight-directions))

(comment
  (get-basic-moves (normalize-board
                    {"8B" {:coords "8B" :piece :knight :color :white}
                     "7B" {:coords "7B" :piece :pawn :color :white}})
                   (->coords-map "8B")))

(def bishop-directions #{[1 1] [1 -1] [-1 1] [-1 -1]})
(defmethod get-basic-moves :bishop [board start]
  (build-moves board start -1 bishop-directions))

(def queen-directions (into bishop-directions rook-directions))
(defmethod get-basic-moves :queen [board start]
  (build-moves board start -1 queen-directions))

(defmethod get-basic-moves :king [board start]
  (build-moves board start 1 queen-directions))

(defmethod get-basic-moves :pawn [board start]
  (let [{:keys [color] :as pawn} (get board start)
        direction (if (= color :white) 1 -1)
        last-rank (get last-ranks (opposing-color color))
        build-all-moves (partial build-moves board start)
        initial-move? (initial-pieces pawn)
        standard-moves (build-all-moves (if initial-move? 2 1) [[direction 0]])
        standard-moves (cond-> standard-moves
                         (and initial-move?
                              (= 2 (count standard-moves)))
                         ((comp #(update % 1 assoc :en-passante true) vec)))
        capture-moves (->> (build-all-moves 1 [[direction 1] [direction -1]])
                           (map #(if-not (:capture %)
                                   (if-let [coord (:en-passante board)]
                                     (if (= coord (traverse-step (:finish %) [(* -1 direction) 0]))
                                       (-> % (assoc :capture (get board coord) :en-passante true))
                                       %)
                                     %)
                                   %)))
        all-moves (mapcat #(let [rank (-> % :finish :rank)]
                             (if (= rank last-rank)
                               (map (fn [p] (assoc % :promotion p)) [:rook :knight :bishop :queen])
                               [%]))
                          (concat standard-moves (filter :capture capture-moves)))]
    all-moves))

(defmethod get-basic-moves :default [_ _] [])

(def get-all-basic-moves
  (memoize
   (fn get-all-basic-moves* [board color]
     (->> board
          keys
          (filter map?)
          (map (partial get board))
          (filter (comp #{color} :color))
          (mapcat (comp (partial get-basic-moves board) :coords))))))

(defn get-castle-moves [{:keys [castling-rights] :as board} color]
  ;; Rules
  ;; - [x] the king and the rook involved must not have previously moved
  ;; - [x] there must be no pieces between the king and the rook
  ;; - [x] The king cannot be under attack 
  ;; - [x] The king may not pass through any square under attack
  ;; - [x] The previous two rules don't apply to the rook
  (let [castles (->> castling-rights
                     (map (partial get board))
                     (filter (comp #{color} :color))
                     (group-by :piece))
        under-attack (->> (get-all-basic-moves board (opposing-color color))
                          (map :finish)
                          set)
        files-between (fn [start finish]
                        (let [piece->file (comp :file :coords)
                              start-file (piece->file start)
                              finish-file (piece->file finish)
                              direction (->> [finish-file start-file]
                                             (map (comp second board-coords->array-coords (partial assoc {:rank "1"} :file)))
                                             (apply -))
                              direction (/ direction (abs direction))]
                          (->> (iterate #(traverse-step % [0 direction]) {:rank "1" :file start-file})
                               (map :file)
                               (take-while (complement #{finish-file})))))
        {:keys [king] :as castles} (update castles :king first)
        castles (update castles :rook
                        (partial keep (fn [rook]
                                        (let [castle-spaces (->> (files-between king rook)
                                                                 (map (fn [file] (-> rook :coords (assoc :file file)))))]
                                          (when-not (some #(or (board %) (under-attack %)) (take 3 castle-spaces))
                                            (assoc rook :castle-spaces (rest castle-spaces)))))))]
    (when (:king castles)
      (map (fn [{[rook-coord king-coord] :castle-spaces :as rook}]
             {:castle [{:start (-> castles :king :coords)
                        :finish king-coord
                        :color color
                        :piece :king}
                       {:start (:coords rook)
                        :finish rook-coord
                        :color color
                        :piece :rook}]}) (:rook castles)))))

(comment
  (get-castle-moves initial-board :white))

(declare apply-move)

(defn is-check-for?
  ([board]
   (is-check-for? board (:turn board)))
  ([board color]
   (some #(= :king (-> % :capture :piece)) (get-all-basic-moves board color))))

(defn get-all-valid-moves [{:keys [turn] :as board}]
  (concat (remove #(is-check-for? (apply-move board %))
                  (get-all-basic-moves board turn))
          (get-castle-moves board turn)))

(def piece-scores
  {:pawn 1
   :kight 3
   :bishop 3
   :rook 5
   :queen 9
   :king Integer/MAX_VALUE})

(defn score-move [move]
  (get piece-scores (or (-> move :capture :piece)
                        (-> move :promotion)) 0))

(defn score-for [color {:keys [score]}]
  (->> [color (opposing-color color)]
       (map score)
       (apply -)))

(defn apply-move [{:keys [turn] :as board} {:keys [piece start finish capture color en-passante castle] :as move}]
  (if castle
    (reduce apply-move board castle)
    (let [new-board (-> board
                        (dissoc start (:coords capture))
                        (assoc (:finish move) {:piece piece :color color :coords finish}
                               :en-passante (when en-passante finish)
                               :turn (opposing-color turn))
                        (update-in [:score turn] + (score-move move))
                        (with-meta {:last-move move}))]
      (cond-> new-board
        (#{:rook :king} piece)
        (update :castling-rights disj start)))))

(defn take-turn [{:keys [turn] :as board} move]
  (let [new-board (apply-move board move)]
    (if-not (seq (get-all-valid-moves new-board))
      (merge board (if (is-check-for? new-board turn)
                     {:game-result :checkmate
                      :winner turn}
                     {:game-result :draw}))
      new-board)))

(comment
  (get-all-valid-moves (normalize-board
                        {"2A" {:piece :pawn :color :black :coords "2A"}
                         "3B" {:piece :pawn :color :white :coords "3B"}
                         "2C" {:piece :pawn :color :white :coords "2C"}
                         "4D" {:piece :pawn :color :black :coords "4D"}
                         "4E" {:piece :pawn :color :white :coords "4E"}
                         "8F" {:piece :rook :color :black :coords "8F"}
                         "1F" {:piece :king :color :white :coords "1F"}
                         :turn :white
                         :en-passante "4D"}))
  (apply-move initial-board (second (get-all-basic-moves initial-board :white)))
  (get-all-basic-moves initial-board :white)
  (get-all-valid-moves initial-board))