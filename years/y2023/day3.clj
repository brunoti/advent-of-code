(ns y2023.day3
  (:require
   [babashka.fs :as fs]
   [clojure.pprint :refer [pprint]]
   [clojure.string :as str]))

(def input-file (str (fs/parent *file*) "/day3.txt"))

(defn part-2 []
  (->> (slurp input-file)
       (str/split-lines)))

(defn slice [value start end]
  (try
    (let [start (if (< start 0) 0 start)
          end (if (> end (count value)) (count value) end)]
      (if (string? value)
        (subs value start end)
        (subvec value start end)))
    (catch Exception e nil)))

(defn find-numbers [line]
  (let [matcher (re-matcher #"\d+" line)]
    (loop [result []]
      (let [match (re-find matcher)]
        (if match
          (recur (conj result match))
          result)))))

(defn is-special? [char]
  (and
   (not (re-matches #"[0-9.]" (str char)))
   (not (nil? char))))

(defn is-numeric? [char]
  (re-matches #"[0-9]" (str char)))

(defn strict-index-of [needle haystack]
  (loop [current-index 0 index-of nil]
    (let [current-char (get haystack current-index)
          whole (slice haystack current-index (+ current-index (count needle)))
          before (get haystack (dec current-index))
          after (get haystack (+ current-index (count needle)))]
      (if (and
           (= (first needle) current-char)
           (= (last needle) (last whole))
           (or (nil? before) (not (is-numeric? before)))
           (or (nil? after) (not (is-numeric? after))))
        current-index
        (if (>= current-index (count haystack))
          index-of
          (recur (inc current-index) index-of))))))

(defn index-of-numbers [numbers line]
  (->> (for [number numbers
             :let [index (strict-index-of number line)]]
         [number index])
       flatten
       (apply hash-map)))

(defn parse-line [index line]
  (let [numbers (find-numbers line)
        where-numbers (index-of-numbers numbers line)]
    {:index index
     :line line
     :numbers numbers
     :where-numbers where-numbers}))

(defn is-valid-based-on-current-line [line numbers]
  (map first
       (filter
        (fn [[number index]]
          (let [before (dec index) after (+ 1 index (count number))]
            (println)
            (println "----------------------------------")
            (println "Looking stuff related to: " number)
            (doseq [char (slice line before after)]
              (println char (is-special? char)))
            (println "----------------------------------")
            (println)
            (some is-special? (vec (slice line before after)))))
        numbers)))

(def zica1 {:numbers ["908" "824" "197" "227" "956" "331" "390"],
            :line
            ".908...*...........*.........*...........*824...197.....$......227...........956............331.......*......390.................*....*.....",
            :where-numbers
            {"227" 63,
             "908" 1,
             "197" 48,
             "331" 92,
             "390" 109,
             "824" 42,
             "956" 77}})

(defn find-valid-numbers [lines]
  (loop [current 0
         new-list []]
    (if (= (count lines) current)
      new-list
      (let [current-line (get lines current)
            next-line (get lines (inc current))
            previous-line (get lines (dec current))]
        (recur (inc current)
               (conj new-list (merge current-line (let [current (is-valid-based-on-current-line (:line current-line) (:where-numbers current-line))
                                                        next (is-valid-based-on-current-line (:line next-line) (:where-numbers current-line))
                                                        previous (is-valid-based-on-current-line (:line previous-line) (:where-numbers current-line))]
                                                    {:current current
                                                     :next next
                                                     :previous previous
                                                     :all (concat current next previous)}))))))))

(defn part-1 []
  (->> (slurp input-file)
       (str/split-lines)
       (map-indexed parse-line)
       (vec)
       (find-valid-numbers)
       (map :all)
       (flatten)
       ; (distinct)
       (map #(Integer/parseInt %))
       (apply +)
       ; (pprint)
       ))

(defn -main []
  (println "Part 1:" (part-1))
  (println "Part 2:" (part-2)))
