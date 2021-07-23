(ns toehold.tree
  (:require [clojure.zip :as z]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as string]
            [toehold.core :as c :refer :all]
            [clojure.math.combinatorics :as combo]
            [clojure.set :as set]))

(defrecord node [content children])

;; Contentful-node zipper largely borrowed from
;; http://grokbase.com/p/gg/clojure/12ag6cjnch/how-to-represent-trees-for-use-with-zippers
(defn content-zipper
  "vector-zip and seq-zip assume that branch nodes don't have content. This version is like vector-zip, but assumes all nodes can have content."
  [root]
  (z/zipper (comp coll? :children)
            :children
            (fn [nd children]
              (assoc nd :children children))
            root))

(defn content [loc] (:content (first loc)))

(defn node-str
  "Return the attractively formatted contents of a node"
  [loc]
  (when-let [node (z/node loc)]
    (str "> " (:content node))))

(defn print-tree [loc]
  (when-not (z/end? loc)
    (do (when (z/branch? loc)
          (pprint (str (string/join "" (repeat (count (z/path loc)) " "))
                       (node-str loc))))
        (recur (z/next loc)))))

(defn node-path "Return the simple path of nodes up to and including this location, including the location"
  [loc]
  (conj (z/path loc) (z/node loc)))


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

    (->> [9]
         (mapcat #(combo/permuted-combinations (range 9) %))
         (filter moves->valid-end-of-game?)
         count))
  ;; => 242496

  )

;; Part 2 TREE TODO


;; CHALLENGE 3: Is it possible to rewrite build-tree so that it's significantly
;; more efficient in time and/or space? If so, what strategies do you see for
;; that? Implement one.

;; The significant change I can think of would be to memoize the paths we have seen.

;; TODO Memoize Tree

;; CHALLENGE 4: write code to answer some of the following questions:
;; 1. What percentage of 100000 random games have no win?
;; 2. Given a partial game in a particular state, which player if any has
;;   a guaranteed win if they play optimally?
;; 3. Under what conditions is player 2 (O) guaranteed a win?
;; 4. Can X get a win if they blow 1st move?


;; 1. What percentage of 100000 random games have no win?

;; Roughly 12% based on this logic:
(comment
  (let [game-count        100000
        ;; NOTE nil represents o win. It's like awkward here and kills the chance to use threading.
        no-win-game-count (get (frequencies (repeatedly game-count (fn [] (win? (rand-game))))) nil 0)]
    (* 100 (float (/ no-win-game-count game-count))))

  )

;; 2. Given a partial game in a particular state, which player if any has
;;   a guaranteed win if they play optimally?

;; TODO
;; NOTE I assume the other player also plays optimally given the current board which might not be optimal for either player. 

;; The trick here is to define "play optimally"
;; If we use the same input's and outputs as rand-valid-move we should be to plug this right in



;; 3. Under what conditions is player 2 (O) guaranteed a win?

;; 3.1 Assuming both play players play optimally from the start. Never
;; TODO prove prove with code


;; 4. Can X get a win if they blow 1st move?

;; Without coding it I know the answer is No if the other player plays optimally.  Yes for anything other then that.
;; TODO prove with code
;; 1. The sub question is, what is and is there, an the optimal first move?
