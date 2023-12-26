(ns day3
  (:require [core :refer [read-input]]))

(def LINE-LENGTH 139)

(def test-input ["467..114.."
                 "...*......"
                 "..35..633."
                 "......#..."
                 "617*......"
                 ".....+.58."
                 "..592....."
                 "......755."
                 "...$.*...."
                 ".664.598.."])

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

(defn numbers-and-coords-from-raw-lines [lines]
  (->> lines
    (map #(partition-by symbol-dot-digit %))
    (map str-inner-seqs)
    (map-indexed parsed-line-to-number-and-coords)
    (filter not-empty)
    (reduce (comp flatten conj))))

(defn find-symbols-in-raw-lines [lines x1 y1 x2 y2]
  "Finds the symbols in the rectangle of the provided coordinates"
  (let [x-min (max 0 x1)
        y-min (max 0 y1)
        x-max (min LINE-LENGTH x2)
        y-max (min LINE-LENGTH y2)
        chars-in-rectangle (for [y (range y-min (inc y-max))
                                 x (range x-min (inc x-max))]
                             (get-char lines x y))]
    (->> chars-in-rectangle
         (filter #(and (not= \. %) (not (Character/isDigit ^Character %)))))))

(defn enhance-number-with-symbol [lines number]
  (let [{:keys [x y length]} number
        symbols (find-symbols-in-raw-lines lines (dec x) (dec y) (+ x length) (inc y))]
    (if (seq symbols)
      (assoc number :symbols symbols)
      number)))

(defn enhance-numbers-and-coords-with-symbols [numbers lines]
  (map (partial enhance-number-with-symbol lines) numbers))

; extract numbers and their starting coordinates
; search for symbols within the bounding box of the number, attach as info to number
; filter for symbols, sum up

(defn solve-part1 [lines]
  (->> lines
       (enhance-numbers-and-coords-with-symbols
         (numbers-and-coords-from-raw-lines lines))
       (filter #(contains? % :symbols))
       (map :value)
       (reduce +)))

(comment
  (solve-part1 test-input)
  (solve-part1 (read-input "resources/input3.txt"))
  (enhance-number-with-symbol test-input {:value 664 :x 1 :y 9 :length 3})
  (enhance-numbers-and-coords-with-symbols
    (numbers-and-coords-from-raw-lines test-input)
    test-input)

  (find-symbols-in-raw-lines test-input 0 1 9 1)
  (partition-by symbol-dot-digit (first test-input))
  (->> test-input
    (map #(partition-by symbol-dot-digit %))
    (map str-inner-seqs))
  (get-char test-input 3 1)
  (numbers-and-coords-from-raw-lines test-input)
  (parsed-line-to-number-and-coords 0 ["467" ".." "114" ".."])
  (map-indexed parsed-line-to-number-and-coords test-input))