(ns day1
  (:require [clojure.string :as str])
  (:gen-class))

(defn read-input [file-name]
  (-> file-name
      slurp
      str/split-lines))

(defn triml-non-num [s]
  (drop-while #(not (Character/isDigit ^Character %)) s))

(defn extract-first-and-last-number [cx]
  (let [trimmed (triml-non-num cx)
        first-num (first trimmed)
        reversed (reverse cx)
        reversed-trimmed (triml-non-num reversed)
        last-num (first reversed-trimmed)]
    (str first-num last-num)))

(defn solve-part1 [lines]
  (->> lines
       (map extract-first-and-last-number)
       (map #(Integer/parseInt %))
       (reduce +)))

(def digit-by-textual
  {"one" \1 "two" \2 "three" \3 "four" \4 "five" \5 "six" \6 "seven" \7 "eight" \8 "nine" \9})

(def textual-replacements-regex
  #"one|two|three|four|five|six|seven|eight|nine")

(defn find-first-digit [s]
  (loop [acc ""
         [^Character c & cx] s]
    (if (Character/isDigit c)
      c
      (let [next-acc (str acc c)
            textual-in-acc (re-find textual-replacements-regex next-acc)]
        (if textual-in-acc
          (digit-by-textual textual-in-acc)
          (recur next-acc cx))))))

(defn find-last-digit [s]
  (let [rs (reverse s)]
    (loop [acc ""
           [^Character c & cx] rs]
      (if (Character/isDigit c)
        c
        (let [next-acc (str c acc)
              textual-in-acc (re-find textual-replacements-regex next-acc)]
          (if textual-in-acc
            (digit-by-textual textual-in-acc)
            (recur next-acc cx)))))))

(defn line-to-calibration-values [line]
  (let [first-digit (find-first-digit line)
        last-digit (find-last-digit line)]
    (Integer/parseInt (str first-digit last-digit))))

(defn solve-part2 [lines]
  (->> lines
       (map line-to-calibration-values)
       (reduce +)))

(defn -main [& args]
  (let [lines (read-input "resources/input1.txt")
        calibration-values (map line-to-calibration-values lines)]
    (prn (reduce + calibration-values))))