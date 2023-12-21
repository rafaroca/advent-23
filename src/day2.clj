(ns day2
  (:require [clojure.string :as str]
            [core :refer [read-input]]))

(defn separate-game-no [game-line]
  (let [[_ game-no game-str] (re-find #"^Game (\d+): (.*)$" game-line)]
    {:game-no (Integer/parseInt game-no) :sets-str game-str}))

(defn set-vec-to-map [[_ cnt color]]
  {(keyword color) (Integer/parseInt cnt)})

(defn parse-set [set-str]
  (->> (re-seq #"(\d+) (blue|green|red)" set-str)
    (map set-vec-to-map)
    (reduce merge)))

(defn parse-sets-str [set-str]
  (let [sets-str (str/split set-str #";")]
    (map parse-set sets-str)))

(defn parse-line [line]
  (let [game (separate-game-no line)
        sets (parse-sets-str (:sets-str game))]
    (-> game
        (assoc :sets sets)
        (dissoc :sets-str))))

(defn possible-game? [{sets :sets}]
  (every? #(and (<= (get % :red 0) 12)
                (<= (get % :green 0) 13)
                (<= (get % :blue 0) 14))
          sets))

(comment
  (def game1 {:game-no 1, :sets [{:blue 3 :red 4} {:red 1 :green 2 :blue 6} {:green 2}]})
  (def game3 {:game-no 3, :sets [{:blue  6 :green 8 :red   20} {:blue  5 :green 13 :red   4} {:green 5 :red   1}]})
  (map #(and (<= (get % :blue 0) 3)
             (<= (get % :green 0) 1))
       (:sets game1))
  (map #(and (<= (get % :blue 0) 5)) (:sets game3))
  (possible-game? game3)
  (possible-game? game1))

(defn solve-part1 [lines]
  (let [games (map parse-line lines)]
    (->> games
      (filter possible-game?)
      (map :game-no)
      (reduce +))))