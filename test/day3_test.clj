(ns day3-test
  (:require [clojure.test :refer :all])
  (:require [day3 :refer :all]))

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

(deftest should-solve-all-parts-with-test-input
  (is (= 4361 (solve-part1 test-input)))
  (is (= 467835 (solve-part2 test-input))))

(deftest should-extract-numbers-and-their-coords
  (is (some #(= % [:numbers [{:value 467, :x 0, :y 0, :length 3}
                             {:value 114, :x 5, :y 0, :length 3}]])
            (numbers-and-coords-from-raw-lines {:lines (take 1 test-input)}))))

(comment
  (numbers-and-coords-from-raw-lines {:lines (take 1 test-input)})
  (enhance-numbers-and-coords-with-symbols {:lines       test-input
                                            :line-length 10
                                            :numbers     [{:value 467, :x 0, :y 0, :length 3}
                                                          {:value 114, :x 5, :y 0, :length 3}]})
  (key-start-with-asterisk? ['("*-5-8") [{:value 598, :x 5, :y 9, :length 3, :symbols ("*-5-8")}
                                         {:value 755, :x 6, :y 7, :length 3, :symbols ("*-5-8")}]])

  (find-symbols-in-raw-lines test-input 9 0 1 9 1)
  (partition-by symbol-dot-digit (first test-input))
  (->> test-input
    (map #(partition-by symbol-dot-digit %))
    (map str-inner-seqs))
  (get-char test-input 3 1)
  (parsed-line-to-number-and-coords 0 ["467" ".." "114" ".."]))