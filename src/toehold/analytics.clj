(ns toehold.analytics
  (:require [toehold.core :as ttt]
            [clojure.test :as t]))

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
