(ns day1-test
  (:require [clojure.test :refer :all]
            [day1 :refer :all]))

(def test-input-day1-part1 ["1abc2" "pqr3stu8vwx" "a1b2c3d4e5f" "treb7uchet"])
(def test-input-day1-part2 ["two1nine" "eightwothree" "abcone2threexyz" "xtwone3four" "4nineeightseven2" "zoneight234" "7pqrstsixteen"])

(def first-input (first test-input-day1-part1))

(deftest triml-non-num-works-with-all-inputs
  (is (= "1abc2" (apply str (triml-non-num first-input))))
  (is (= "3stu8vwx" (apply str (triml-non-num (second test-input-day1-part1))))))

(deftest extract-first-and-last-number-works
  (is (= "12" (extract-first-and-last-number "1abc2"))))

(deftest solve-part1-works
  (is (= 142 (solve-part1 test-input-day1-part1))))

(deftest should-solve-part2
  (is (= (solve-part2 test-input-day1-part2) 281))
  (is (= (solve-part2 ["abcone2threexyz"]) 13))
  (is (= (solve-part2 ["4nineeightseven2"]) 42))
  (is (= (solve-part2 ["zoneight234"]) 14))
  (is (= (solve-part2 ["7pqrstsixteen"]) 76))
  (is (= (solve-part2 ["sevenine"]) 79)))

(deftest should-find-first-digit
  (is (= (find-first-digit "12") \1))
  (is (= (find-first-digit "ab1") \1))
  (is (= (find-first-digit "twone") \2)))

(deftest should-find-last-digit
  (is (= (find-last-digit "12") \2))
  (is (= (find-last-digit "1ab") \1))
  (is (= (find-last-digit "twone") \1)))