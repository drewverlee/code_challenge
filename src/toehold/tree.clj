(ns toehold.tree
  (:require [clojure.zip :as z]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as string]
            [toehold.core :as c :refer :all]
            [clojure.math.combinatorics :as combo]
            [clojure.set :as set]
            [clojure.test :as t]))

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
