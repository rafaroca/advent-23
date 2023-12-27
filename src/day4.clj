(ns day4
  (:require [clojure.string :as str]
            [clojure.set]
            [clojure.math]
            [core :refer [read-input]]))

(defn separate-card-no [line]
  (let [[_ card-no all-numbers] (re-find #"^Card +(\d+): (.*)$" line)]
    {:card-no (Integer/parseInt card-no) :raw-numbers all-numbers}))

(defn parse-numbers [{:keys [raw-numbers] :as line-state}]
  (let [[winnings-str havings-str] (str/split raw-numbers #" \| ")
        winnings-vec (str/split (str/trim winnings-str) #"\s+")
        winnings (set (map #(Integer/parseInt %) winnings-vec))
        havings-vec (str/split (str/trim havings-str) #"\s+")
        havings (set (map #(Integer/parseInt %) havings-vec))]
    (into line-state {:winnings winnings :havings havings})))

(defn extract-matches [{:keys [winnings havings] :as line-state}]
  (let [matches (clojure.set/intersection winnings havings)]
    (into line-state {:matches matches})))

(defn calculate-points [{:keys [matches] :as line-state}]
  (let [hit-count (count matches)
        points (if (zero? hit-count)
                 0
                 (int (clojure.math/pow 2 (dec hit-count))))]
    (into line-state {:points points})))

(defn solve-part1 [lines]
  (->> lines
    (map separate-card-no)
    (map parse-numbers)
    (map extract-matches)
    (map calculate-points)
    (map :points)
    (reduce +)))

(comment
  (def input (read-input "resources/input4.txt"))
  (solve-part1 (read-input "resources/input4.txt")))