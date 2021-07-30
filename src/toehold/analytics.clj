(ns toehold.analytics
  (:require [toehold.core :as ttt]
            [clojure.set :as set]
            [clojure.math.combinatorics :as combo]))

;; CHALLENGE 2: Write a function to build a tree of all possible games. Explain
;; why or why not it uses content-zipper (above).

;; Part one:  Brute force / treeless for generating all valid game states
;;
;; I wanted to sketch something out fast first that would find all the possible games using a more brute force approach. This creates all permutations for a series of player moves (spots claimed) of sizes 5 and up to and including 9.
;; It then just has to validate that set of moves. I did a rough count in my head and it took around 40 seconds in my nrepl, but I suspect that could be halved by multithreaded solution.
;; In part I did it this way first because I'm not sure how to do a significant improve on what I want to be my next solution, which will build the tree and try to memoize paths we have already seen.

;; I didn't use the content-zipper or any existing code here because I went with a treeless solution.
;; NOTE i'm worried the total valid end of game moves is off, something concerning the last set of permutations
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

;; Part two: Tree

;; This solution actually builds a tree, but doesn't keep it in memory as that exceeds the heap bounds I have set on my repl. It returns 255168 which is the correct solution of possible games. It's self contained and doesn't use pre-existing functions because for the most part they don't specifically help the hard part of this question. Though It would be possible to refactor this idea to take advantage of the functions in core.

;; I ended up not using context zip after thinking about it again because it's will be less space to just aggregate the games played. I believe the way content zipper could be used to _build_ a tree (not search it) would be to use the zipper/append-child function, but this would be more complex then simply conjing the children in a tree vector. The zipper library as a whole seems designed to help manual search trees. But I haven't had any experience with it before, so it's possible i'm missing something in the docs.

(defn count-possible-games
  ([]
   (count-possible-games []))
  ([moves]
   (if
       (or (= 9 (count moves))
           (seq
             ;; board for reference
             ;; 0 1 2
             ;; 3 4 5
             ;; 6 7 8
             (for [win    [#{0 1 2} #{3 4 5} #{6 7 8}
                           #{0 3 6} #{1 4 7} #{2 5 8}
                           #{0 4 8} #{2 4 6}]
                   pmoves [(take-nth 2 moves) (take-nth 2 (rest moves))]
                   :when  (set/subset? win (set pmoves))]
               pmoves)))
     1
     (reduce +
             (map
               #(count-possible-games (conj moves %))
               (set/difference  (set (range 9)) (set moves)))))))


(comment
  (count-possible-games)

  ;;=> 255168
  )

;; CHALLENGE 3: Is it possible to rewrite build-tree so that it's significantly
;; more efficient in time and/or space? If so, what strategies do you see for
;; that? Implement one.

;; I'm calling the function `count-possible-games` at least at the moment.
;; Given the goal of counting all possible games. One thing we could do is memoize, which i believe would cut the tree search in half. To do that, we have to abstract one level higher though and just keep track of the player owned spots and not the order they played them. e.g

;; [0 1 2 3] => {:x #{0 2} :o #{1 3}} now x playing 0 then 2 will be the same result as 2 and then 0... which from the games perspective it is. We can just add the last move to the smallest set of moves each times. for this question that doesn't matter, doing that leads to a bit more functionality then expected. Maybe it would better to keep track of whose turn it is? I'll just note that I also find this code to be somewhat hard to read, but it's compact, which means that it should make abstracting out bits easier later if necessary. For now, i'm not sure this approach is ideal, so there is no use sugar coating it. It gives the right answer.

(defn count-possible-games-v2
  ([]
   (count-possible-games-v2 [#{} #{}]))
  ([moves]
   (let [taken-spots (apply set/union moves)]
     (if
         (or (= 9 (count taken-spots))
             (seq
               ;; board for reference
               ;; 0 1 2
               ;; 3 4 5
               ;; 6 7 8
               (for [win    [#{0 1 2} #{3 4 5} #{6 7 8}
                             #{0 3 6} #{1 4 7} #{2 5 8}
                             #{0 4 8} #{2 4 6}]
                     pmoves moves
                     :when  (set/subset? win (set pmoves))]
                 pmoves)))
       1
       (reduce +
               (map
                 #(count-possible-games-v2
                    (update (vec (sort-by count moves)) 0 (fn [current-player-spots] (conj current-player-spots %))))
                 (set/difference  (set (range 9)) taken-spots)))))))

(count
  (count-possible-games-v2);; => 255168

  )

;; the Memoize version has to be different in one way, as we discussed earlier, in that x and o need to be indistinguishable from each other, so that we can half the work. We can also introduce a bit of parallelism to see if that speeds it up. We can't be sure, we have to time it.

(defn count-possible-games-v3
  [moves]
  (let [taken-spots (apply set/union moves)]
    (if
        (or (= 9 (count taken-spots))
            (seq
              ;; board for reference
              ;; 0 1 2
              ;; 3 4 5
              ;; 6 7 8
              (for [win    [#{0 1 2} #{3 4 5} #{6 7 8}
                            #{0 3 6} #{1 4 7} #{2 5 8}
                            #{0 4 8} #{2 4 6}]
                    pmoves moves
                    :when  (set/subset? win (set pmoves))]
                pmoves)))
      1
      (reduce +
              (map
                #(count-possible-games-v3
                   (update (set (vec (sort-by count moves))) 0 (fn [current-player-spots] (conj current-player-spots %))))
                (set/difference  (set (range 9)) taken-spots))))))


;; Sense a set needs to contain unique items and both players start with no spots take (an empty set) we need some extra setup here. 
(comment
  ;; about 11 seconds if i count in my head...
  (reduce +
          (map
            #(count-possible-games-v3 #{#{%} #{}})
            (range 9)));; => 255168

  ;; about 3 seconds!
  (reduce +
          (pmap
            #(count-possible-games-v3 #{#{%} #{}})
            (range 9)));; => 255168

  ;; ok, oh yea and we wanted to memoize this...

  )

(def count-possible-games-v3-memoized (memoize count-possible-games-v3))

(comment
  ;; !!!! This memoized version finishes instantly. 
  (reduce +
          (map
            #(count-possible-games-v3-memoized #{#{%} #{}})
            (range 9)));; => 255168

  ;; also instant for our multithreaded version
  (reduce +
          (pmap
            #(count-possible-games-v3-memoized #{#{%} #{}})
            (range 9)));; => 255168


  )


;; Questions overview
;; CHALLENGE 4: write code to answer some of the following questions:
;; 1. What percentage of 100000 random games have no win?
;; 2. Given a partial game in a particular state, which player if any has
;;   a guaranteed win if they play optimally?
;; 3. Under what conditions is player 2 (O) guaranteed a win?
;; 4. Can X get a win if they blow 1st move?

;; 4.1. What percentage of 100000 random games have no win?
;; Roughly 12% based on this logic:

(defn strategy->draw-percentage
  [{:keys [strategy game-count] :or {game-count 100000}}]
  (let [;; NOTE nil represents no win. It's like awkward here and kills the chance to use threading.
        no-win-game-count (get (frequencies (repeatedly game-count
                                                        (fn [] (ttt/win? (ttt/game-configuration->full-game-moves
                                                                          {:stragety strategy})))))
                               nil
                               0)]
    (* 100 (float (/ no-win-game-count game-count)))))

(comment

  (strategy->draw-percentage {:strategy ttt/rand-valid-move})
  ;; => 12.8



  ;; check that are optimal game always ends in a draw
  ;; 100% of games are no win/draw


  (strategy->draw-percentage {:strategy ttt/optimal-move})
  ;; => 100

  )


;; 4.2. Given a partial game in a particular state, which player if any has
;;   a guaranteed win if they play optimally?

;; The question can be answered in a couple different ways. The most straight forward of which is a function that takes a game state and returns the winner assuming both players play optimal.
;; In a way we already have this, our strategy->game-output function just needs to be adapted


(comment
  (->> {:moves [[0 0 :x] [0 1 :o] [1 1 :x] ] :stragety ttt/optimal-move}
       ttt/game-configuration->full-game-moves
       ttt/win?);; => :x
  )


;; 4.3. Under what conditions is player 2 (O) guaranteed a win?

;; Assuming both play players play optimally from the start. Never
;; This would be the same could as starting from an init state as we demonstrated earlier.

(comment
  (strategy->draw-percentage {:strategy ttt/optimal-move})
  ;; => 100






  )



;; 4. Can X get a win if they blow 1st move?

;; Without coding it I know the answer is No if the other player plays optimally.  Yes for anything other then that.
;; TODO prove with code
;; 4.1 The sub question is, what is and is there, an the optimal first move?




;; What about zippers?
;; 1. What about zippers?
;; 2. Mini tree

;; 1. What about Zippers?

;; So having never looked into zippers until this coding challenge, the impression I get scanning the docs is that they primarily useful navigate _existing_ and modifying existing tree structures. In this case, sense were building a tree, It wasn't at all clear how that would be useful. The question says to explain why or why not you would use one to build a tree.
;; I would have discounted their usefulness all together for this except a zipper/loc will keep track of the path to the current tree were at. Which means, given the use of the content-zipper, we can see the moves that preceded the current location. Which i demonstrate here in a function which I hope to evolve into one that

(defn build-zipper-tree
  [loc l]
  (cond
    (and (-> loc z/node :children empty?) (seq l)) (do
                                                     (println (map :content (flatten (z/path loc))))
                                                     (recur (z/next (z/append-child loc (v->nodes (first l)))) (rest l)))
    (z/branch? loc)                                (recur (z/next loc) l)
    (z/end? loc)                                   loc
    :else                                          loc))


;; 2. Mini tree

;; When i was building the tree functions that i later ended up just returning integers from (to count possible games). I wanted to test my idea on something I could see, so here is a mini 2x2 tick tac toe function and here is the output form it:

(defn mini-build-moves
  [moves]
  (if (seq
        (for [win   [#{0 1} #{2 3} #{0 2} #{2 1} #{1 3} #{3 0}]
              pmove [(take-nth 2 moves) (take-nth 2 (rest moves))]
              :when (set/subset? win (set pmove))]
          pmove))
    moves
    (cons moves (map #(mini-build-moves (conj moves %))
                     (set/difference (set (range 4)) (set moves) )))))


(comment
  (mini-build-moves [])

  )

;; => ([]
;;     ([0]
;;      ([0 1] ([0 1 2] ([0 1 2 3])) [0 1 3])
;;      ([0 2] [0 2 1] [0 2 3])
;;      ([0 3] [0 3 1] ([0 3 2] ([0 3 2 1]))))
;;     ([1]
;;      ([1 0] [1 0 2] ([1 0 3] ([1 0 3 2])))
;;      ([1 2] [1 2 0] ([1 2 3] ([1 2 3 0])))
;;      ([1 3] [1 3 0] [1 3 2]))
;;     ([2]
;;      ([2 0] [2 0 1] [2 0 3])
;;      ([2 1] ([2 1 0] ([2 1 0 3])) [2 1 3])
;;      ([2 3] ([2 3 0] ([2 3 0 1])) [2 3 1]))
;;     ([3]
;;      ([3 0] ([3 0 1] ([3 0 1 2])) [3 0 2])
;;      ([3 1] [3 1 0] [3 1 2])
;;      ([3 2] [3 2 0] ([3 2 1] ([3 2 1 0])))))
