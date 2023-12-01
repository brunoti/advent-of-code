(ns y2023.day1
  (:require
   [babashka.fs :as fs]
   [clojure.string :as str]))

(def input-file (str (fs/parent *file*) "/day1.txt"))

(def num-map
  {"one" 1
   "two" 2
   "three" 3
   "four" 4
   "five" 5
   "six" 6
   "seven" 7
   "eight" 8
   "nine" 9})

(defn get-numbers [text]
  (->> (seq text)
       (filter #(Character/isDigit %))
       (map #(Character/digit % 10))))

(defn to-number [item]
  (if (number? item)
    item
    (get num-map item)))

(defn make-number [data]
  (Integer/parseInt (str (first data) (last data))))

(defn dashify [text]
  (apply str (repeat (count text) "-")))

(defn get-all-indexes [haystack needle]
  (loop [haystack haystack
         indexes []]
    (if (not (str/includes? haystack needle))
      indexes
      (recur (str/replace-first haystack (re-pattern needle) (dashify needle)) (into indexes [(str/index-of haystack needle)])))))

(defn number-position-map [text]
  (as-> (into (vals num-map) (keys num-map)) m
    (map #(-> (zipmap (get-all-indexes text (str %)) (repeat %))) m)
    (apply merge m)))

(defn get-pair [number-map]
  [(get number-map (apply min (keys number-map)))
   (get number-map (apply max (keys number-map)))])

(defn pair-to-number [pair]
  (Integer/parseInt (apply str (map to-number pair))))

(defn part-2 []
  (->> (slurp input-file)
       (str/split-lines)
       (map (comp pair-to-number get-pair number-position-map))
       (apply +)))

(defn part-1 []
  (->> (slurp input-file)
       (str/split-lines)
       (map (comp make-number get-numbers))
       (apply +)))

(defn -main []
  (println "Part 1:" (part-1))
  (println "Part 2:" (part-2)))
