(ns core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clj-http.client :as client])
  (:gen-class))

(defn read-input [file-name]
  (-> file-name
      slurp
      str/split-lines))

(def session-cookie-file "session-cookie.txt")

(defn retrieve-cookie []
  (if (.exists (io/file session-cookie-file))
    (do
      (println "Reading cookie from 'session-cookie.txt'...")
      (slurp session-cookie-file))
    (do
      (print "Enter your session cookie: ")
      (flush)
      (let [session-cookie (read-line)]
        (spit session-cookie-file session-cookie)
        session-cookie))))

(defn -main [day & _]
  (if-not day
    (do (println "Download the puzzle input of a day")
        (println "Usage: core <number of day>")
        (System/exit 1)))
  (let [session-cookie (retrieve-cookie)
        url-of-day (str "https://adventofcode.com/2023/day/" day "/input")
        filename-of-day (str "resources/input" day ".txt")]
    (println "Downloading " url-of-day "...")
    (->> (client/get url-of-day
                     {:cookies {"session" {:value session-cookie}}})
         :body
         (spit filename-of-day))))
