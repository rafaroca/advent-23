(ns day3
  (:require [clojure.string :as str]
            [core :refer [read-input]]
            [hashp.core]))

(defn symbol-dot-digit [^Character c]
  (cond
    (= c \.) :dot
    (Character/isDigit c) :digit
    :else :symbol))

(defn str-inner-seqs [s]
  (map #(apply str %) s))

(defn get-char [coll x y]
  (-> coll
      (nth y)
      (nth x)))

(defn number-and-coords [entry x y]
  (let [type (symbol-dot-digit (first entry))]
    (if (= :digit type)
      {:value  (Integer/parseInt entry)
       :x      x
       :y      y
       :length (count entry)}
      nil)))

(defn parsed-line-to-number-and-coords [y line]
  (loop [numbers []
         x 0
         [entry & entries] line]
    (if (nil? entry)
      numbers
      (let [type (symbol-dot-digit (first entry))
            length (count entry)
            new-numbers (if (= :digit type)
                          (conj numbers (number-and-coords entry x y))
                          numbers)]
        (recur new-numbers
               (+ x length)
               entries)))))

(defn numbers-and-coords-from-raw-lines [state]
  (->> (:lines state)
    (map #(partition-by symbol-dot-digit %))
    (map str-inner-seqs)
    (map-indexed parsed-line-to-number-and-coords)
    (filter not-empty)
    (reduce (comp flatten conj))
    (assoc state :numbers)))

(defn find-symbols-in-raw-lines [lines line-length x1 y1 x2 y2]
  "Finds the symbols in the rectangle of the provided coordinates"
  (let [x-min (max 0 x1)
        y-min (max 0 y1)
        x-max (min (dec line-length) x2)
        y-max (min (dec line-length) y2)
        chars-in-rectangle (for [y (range y-min (inc y-max))
                                 x (range x-min (inc x-max))]
                             (str (get-char lines x y) "-" x "-" y))]
    (->> chars-in-rectangle
      (remove (partial re-find #"^\.|^\d")))))

(defn enhance-number-with-symbol [lines line-length number]
  (let [{:keys [x y length]} number
        symbols (find-symbols-in-raw-lines lines line-length (dec x) (dec y) (+ x length) (inc y))]
    (if (seq symbols)
      (assoc number :symbols symbols)
      number)))

(defn enhance-numbers-and-coords-with-symbols [{:keys [lines numbers line-length]}]
  (map (partial enhance-number-with-symbol lines line-length) numbers))

; extract numbers and their starting coordinates
; search for symbols within the bounding box of the number, attach as info to number
; filter for symbols, sum up

(defn solve-part1 [lines]
  (let [line-length (count (first lines))
        state {:line-length line-length
               :lines lines}]
    (->> state
      numbers-and-coords-from-raw-lines
      enhance-numbers-and-coords-with-symbols
      (filter #(contains? % :symbols))
      (map :value)
      (reduce +))))

(defn key-start-with-asterisk? [[[k] _]]
  (str/starts-with? k "*"))

(defn has-two-adjacent-numbers? [[_ v]]
  (= 2 (count v)))

(defn multiply-values [values]
  (->> values
    (map :value)
    (reduce *)))

(defn solve-part2 [lines]
  (let [line-length (count (first lines))
        state {:line-length line-length
               :lines lines}]
    (->> state
      numbers-and-coords-from-raw-lines
      enhance-numbers-and-coords-with-symbols
      (filter #(contains? % :symbols))
      (group-by :symbols)
      (filter key-start-with-asterisk?)
      (filter has-two-adjacent-numbers?)
      (map val)
      (map multiply-values)
      (reduce +))))

(comment
  (solve-part1 test-input)
  (solve-part1 (read-input "resources/input3.txt"))
  (solve-part2 test-input)
  (solve-part2 (read-input "resources/input3.txt")))