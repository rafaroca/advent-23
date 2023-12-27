(ns day4-test
  (:require [clojure.test :refer :all])
  (:require [day4 :refer :all]))

(def test-input ["Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
                 "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"
                 "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"
                 "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"
                 "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"
                 "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"])

(deftest should-extract-card-no
  (is (some #(= % [:card-no 2]) (separate-card-no (second test-input)))))

(deftest should-parse-numbers
  (is (some #(= % [:winnings #{86 48 41 17 83}])
            (parse-numbers {:raw-numbers "41 48 83 86 17 | 83 86  6 31 17  9 48 53"})))
  (is (some #(= % [:havings #{86 48 31 6 17 9 83 53}])
            (parse-numbers {:raw-numbers "41 48 83 86 17 | 83 86  6 31 17  9 48 53"}))))

(deftest should-extract-matches
  (is (some #(= % [:matches #{48 83 17 86}])
            (extract-matches {:winnings #{86 48 41 17 83} :havings #{86 48 31 6 17 9 83 53}}))))

(deftest should-calculate-points
  (is (some #(= % [:points 8])
            (calculate-points {:matches #{48 83 17 86}}))))

(deftest should-solve-part1
  (is (= 13 (solve-part1 test-input))))

(comment
  (separate-card-no (first test-input)))