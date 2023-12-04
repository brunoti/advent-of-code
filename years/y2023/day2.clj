(ns y2023.day2
  (:require
   [babashka.fs :as fs]
   [clojure.string :as str]))

(def input-file (str (fs/parent *file*) "/day2.txt"))

(def colors ["red" "green" "blue"])
(def maxes {"red" 12 "green" 13 "blue" 14})

(defn is-valid-hand [hand]
  (every? (fn [[color value]] (or (nil? value) (<= value (maxes color)))) hand))

(defn get-min-dice-count [hand color]
  (apply max (remove nil? (map #(get % color) hand))))

(defn get-min-hand-count [hands]
  (let [result {:red (get-min-dice-count hands "red")
                :green (get-min-dice-count hands "green")
                :blue (get-min-dice-count hands "blue")}]
    (assoc result :power (apply * (vals result)))))

(defn remove-game-id [line]
  (str/trim (str/replace line #"^Game \d+:" "")))

(defn extract-color [color from]
  (let [value (second (re-find (re-pattern (str "(\\d+) " color)) from))]
    (when value
      (Integer/parseInt value))))

(defn parse-hand [hand]
  (reduce #(assoc %1 %2 (extract-color %2 hand)) {} colors))

(defn get-hands [line]
  (map str/trim (str/split (remove-game-id line) #";")))

(defn get-game-id [line]
  (Integer/parseInt (second (re-find #"^Game (\d+)" line))))

(defn parse-game [line]
  (let [hand-line (remove-game-id line)
        hands (get-hands hand-line)
        parsed-hands (map parse-hand hands)]
    {:id (get-game-id line)
     :hands hands
     :parsed-hands parsed-hands
     :possible (every? is-valid-hand parsed-hands)
     :grounds (get-min-hand-count parsed-hands)}))

(defn part-1 []
  (->> (slurp input-file)
       (str/split-lines)
       (map parse-game)
       (filter :possible)
       (map :id)
       (apply +)))

(defn part-2 []
  (->> (slurp input-file)
       (str/split-lines)
       (map parse-game)
       (map :grounds)
       (map :power)
       (apply +)))

(defn -main []
  (println "Part 1:" (part-1))
  (println "Part 2:" (part-2)))
