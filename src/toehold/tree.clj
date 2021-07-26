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

(defn v->nodes "Build a simple tree of nodes from a nested vector"
  [vectr]
  (node. (first vectr) (mapv v->nodes (rest vectr))))

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
