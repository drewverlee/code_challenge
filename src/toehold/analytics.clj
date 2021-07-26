(ns toehold.analytics
  (:require [toehold.core :as ttt]
            [clojure.test :as t]
            [clojure.set :as set]
            [clojure.math.combinatorics :as combo]))



;; CHALLENGE 2: Write a function to build a tree of all possible games. Explain
;; why or why not it uses content-zipper (above).

;; Part one:  Brute force / treeless for generating all valid game states
;;
;; I wanted to sketch something out first that would find all the possible games using a more brute force approach. This creates all permutations for a series of player moves (spots claimed) of sizes 5 and up to and including 9.
;; It then just has to validate that set of moves. I did a rough count in my head and it took around 40 seconds in my nrepl, but I suspect that could be halved by multithreaded solution.
;; In part I did it this way first because I'm not sure how to do a significant improve on the my next solution which will build the tree and try to memoize paths we have already seen.

;; I didn't use the content-zipper or any existing code here because I went with a treeless solution.
;; NOTE i'm mildly worried the total valid end of game moves is off, something concerning the last set of permutations
(comment
  (let [moves->valid-end-of-game?
        (fn [moves]
          "moves is an a collection of alternating player owned spots, return truthy value if the moves form a valid end of game."
          (let [player-spots->winning-sets
                (fn
                  [player-owned-spots]
                  (filter
                    #(set/subset? % (set player-owned-spots))
                    ;; board for reference
                    ;; 0 1 2
                    ;; 3 4 5
                    ;; 6 7 8

                    #{#{0 1 2} #{3 4 5} #{6 7 8}
                      #{0 3 6} #{1 4 7} #{2 5 8}
                      #{0 4 8} #{2 4 6}}))
                winning-sets (concat
                               (player-spots->winning-sets (take-nth 2 moves))
                               (player-spots->winning-sets (take-nth 2 (rest moves))))
                winning-set  (first winning-sets)
                draw?        (and
                               (zero? (count winning-sets))
                               (= 9 (count moves)))
                one-winner?  (and
                               ;; one winner
                               (= 1 (count winning-sets))

                               ;; last move won it
                               (winning-set (last moves)))]
            (or draw? one-winner?))) ]

    (->> (range 5 10)
         (mapcat #(combo/permuted-combinations (range 9) %))
         (filter moves->valid-end-of-game?)
         count))
  ;; => 242496
  ;; I think should should be 255168 based off http://www.se16.info/hgb/tictactoe.htm
  ;; the 9th permutation returns less then it should.
  )


;; Questions overview
;; CHALLENGE 4: write code to answer some of the following questions:
;; 1. What percentage of 100000 random games have no win?
;; 2. Given a partial game in a particular state, which player if any has
;;   a guaranteed win if they play optimally?
;; 3. Under what conditions is player 2 (O) guaranteed a win?
;; 4. Can X get a win if they blow 1st move?

;; 1. What percentage of 100000 random games have no win?
;; Roughly 12% based on this logic:

(defn strategy->draw-percentage
  [{:keys [strategy game-count] :or {game-count 100000}}]
  (let [;; NOTE nil represents no win. It's like awkward here and kills the chance to use threading.
        no-win-game-count (get (frequencies (repeatedly game-count
                                                        (fn [] (ttt/win? (ttt/stragety->game-output strategy)))))
                               nil
                               0)]
    (* 100 (float (/ no-win-game-count game-count)))))

(comment

  (strategy->draw-percentage {:strategy ttt/rand-valid-move})
  ;; => 12.



  ;; check that are optimal game always ends in a draw
  ;; 100% of games are no win/draw


  (strategy->draw-percentage {:strategy ttt/optimal-move})
  ;; => 100




  )


;; 2. Given a partial game in a particular state, which player if any has
;;   a guaranteed win if they play optimally?
