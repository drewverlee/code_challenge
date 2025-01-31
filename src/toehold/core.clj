(ns toehold.core
  (:require [clojure.core.logic :as l]
            [clojure.core.logic.arithmetic :as la]
            [clojure.pprint :refer [pprint]]
            [toehold.utils :refer [inspect]]
            [clojure.set :as set])
  (:gen-class))

(def players #{:x :o})

;; We represent a board-state as a 3x3 matrix of :x, :o, or :_ (blank).
;; nil for blank would be slightly easier to deal with, but would decrease
;; readability on print
(def empty-board [[:_ :_ :_] [:_ :_ :_] [:_ :_ :_]])

;; Moves are a sequence of [x y p], where player is either :x or :o, and
;; x, y indicates the column and row of the move.

(defn occupied? [board x y ]
  (not= ((board x) y) :_))

;; CHALLENGE 1: Implement this function. See toehold.core-test/available-moves-test
(defn available-moves [board]
  "Return all empty positions as [x y]"
  (for [x     (range (count board))
        y     (range (count board))
        :when (= :_ (get-in board [x y]))]
    [x y]))

(defn move [board [x y val]]
  (if (occupied? board x y)
    (throw (Exception. (str "Can't move to occupied space: " board x y val)))
    (assoc board x (assoc (board x) y val))))

(defn after [mvs board] (reduce move board mvs))

(defn board-from [mvs] (after mvs empty-board))

(defn cur-player  [moves] ([:x :o] (mod (count moves) 2)))
(defn last-player [moves] ([:o :x] (mod (count moves) 2)))

(defn- rand-player [] (rand-nth players))

(defn rand-valid-move [moves & [player]]
  (let [board     (board-from moves) ; TODO memoize?
        avl-moves (available-moves board)]
    (assert (seq avl-moves) ; Make sure board's not full
            (str "No valid moves left on " board))
    (conj moves
          (conj (rand-nth avl-moves)
                (or player (cur-player moves))))))


(defn full? [moves]
  (>= (count moves) 9))

(defn cols [b]
  (for [col-i (range 3)]
    (mapv #(nth % col-i) b)))

;; Trivial, but handy to have the matching call
(defn rows [b] b)

(defn call
  "Given a seq containing a fn and some args, apply the fn to the args"
  [& args]
  (apply (first args) (rest args)))

(defn diags [b]
  "(mapv call b (range 3)) returns [((b 0) 0), ((b 1) 1), ((b 2) 2)]. Then
we do the same thing but for [2 1 0]."
  (vector (mapv call b (range 3))
          (mapv call b (reverse (range 3)))))

(defn triplets [b]
  "Return all triplets of b that could qualify as a win"
  (concat (rows b) (cols b) (diags b)))

(defn- check-triplet [triplet]
  (let [one (players (first triplet))] ;call players to restrict to :x :o
    (when (every? #(= % one) triplet)
      one)))

;; Todo consider renaming, it's not clear this will return the winning player as the suffix implies true/false. 
(defn win?
  "Given a list of moves, return the winning player (or nil if none)"
  [moves]
  (first (keep check-triplet (triplets (board-from moves)))))

(defn full-or-win? [moves]
  (or (full? moves) (win? moves)))

(defn grid-spot->flat-spot
  [[x y player]]
  [(+ y (* x 3)) player])

(defn flat-spot->grid-spot
  [n]
  [(int (/ n 3)) (mod n 3) ])

(defn optimal-move [moves & [player]]
  (let [board                       (board-from moves) ; TODO memoize?
        current-spot->player        (into {}      (map grid-spot->flat-spot moves))
        avl-moves                   (available-moves board)
        current-player              (or player (cur-player moves))
        ;; TODO just use moves to get ppm and popm
        other-player                ({:x :o :o :x} current-player) 
        possible-player-moves       (map #(conj % current-player) avl-moves)
        possible-other-player-moves (map #(conj % other-player) avl-moves)
        best-grid-spot              (->>
                                      ;; for every available spot create a possible future board and count how many triplets the potentially claimed
                                      ;; spot would give the player. Take the highest  one
                                      (for [available-spot (set/difference (set (range 9)) (-> current-spot->player keys set) )
                                            :let           [possible-future-spot->player (assoc current-spot->player available-spot current-player)]
                                            ;;TODO only use one way to get triplets in project
                                            triplet        #{#{0 1 2} #{3 4 5} #{6 7 8}
                                                             #{0 3 6} #{1 4 7} #{2 5 8}
                                                             #{0 4 8} #{2 4 6}}
                                            triplet-spot   triplet
                                            :when          (and (triplet available-spot) (possible-future-spot->player triplet-spot))]
                                        {:spot available-spot :player (possible-future-spot->player triplet-spot)})
                                      (reduce
                                        (fn [spot->score {:keys [spot player]}]
                                          (update spot->score spot
                                                  (if (= player current-player)
                                                    (fnil inc 0)
                                                    (fnil dec 0))))
                                        {})
                                      (sort-by val)
                                      reverse
                                      ffirst
                                      flat-spot->grid-spot)]
    (assert (seq avl-moves) ; Make sure board's not full
            (str "No valid moves left on " board))
    (conj moves
          (some #(when (seq %) (first (shuffle %)))
                ;; win the game
                [(filter #(win? (conj moves % )) possible-player-moves)

                 ;; don't lose the game
                 ;; check what move would let the other player win and take it
                 (->> possible-other-player-moves
                      (filter #(win? (conj moves %)))
                      (map (fn [[x y]] [x y other-player])))

                 [(conj best-grid-spot current-player)]]))))

(defn game-configuration->full-game-moves
  [{:keys [stragety moves] :or {moves    []
                                stragety rand-valid-move}}]
  (first (drop-while (comp not full-or-win?)
                     (iterate stragety moves ))))



(defn moves->print-board!
  [moves]
  (->> moves
       board-from
       (run! println)))

(defn -main
  "Minimal usage example"
  [& args]
  (let [game (stragety->game-output rand-valid-move)]
    (println "moves: " game)
    (println "board at end of random game: " (board-from game))
    (println "winner: " (win? game))
    (println "final move: " (last game))))
