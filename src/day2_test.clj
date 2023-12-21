(ns day2-test
  (:require [clojure.string :as str]
            [clojure.test :refer :all]
            [day2 :refer :all]))

(def test-input-day1-part1 ["Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
                            "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
                            "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
                            "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
                            "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"])

(def first-input (first test-input-day1-part1))

(deftest should-separate-game-no
  (is (some #(= [:game-no 1] %) (separate-game-no first-input)))
  (is (some #(= [:sets-str "3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"] %)
            (separate-game-no first-input))))

(deftest should-parse-set
  (is (= {:blue 3 :red 4} (parse-set "3 blue, 4 red"))))

(deftest should-parse-sets-str
  (is (= [{:blue 3 :red 4} {:red 1 :green 2 :blue 6} {:green 2}]
         (parse-sets-str "3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"))))

(deftest should-parse-line
  (is (= {:game-no 1, :sets [{:blue 3 :red 4} {:red 1 :green 2 :blue 6} {:green 2}]}
         (parse-line first-input)))
  (is (= {:game-no 3, :sets [{:blue  6 :green 8 :red   20} {:blue  5 :green 13 :red   4} {:green 5 :red   1}]}
         (parse-line (get test-input-day1-part1 2)))))

(deftest should-evaluate-possible-game
  (let [games (map parse-line test-input-day1-part1)]
    (is (possible-game? (nth games 0)) "First game is possible")
    (is (possible-game? (nth games 1)))
    (is (false? (possible-game? (nth games 2))))
    (is (false? (possible-game? (nth games 3))))
    (is (possible-game? (nth games 4)))))

(deftest should-solve-part1
  (is (= 8 (solve-part1 test-input-day1-part1))))