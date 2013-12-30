(ns clj-time-series.io
  (:require 
    [clojure.java.io :as io]
    [clj-time.format :as fmt]
    [clojure.string :refer [join split]]))

(def ^:dynamic *filepath* "data/")

(def default-format "dd-MMM-yyyy")

(defn- readline 
  "Parses time series data into [clj-time [values]]"
  [line infmt] 
  (let [[rawdate & rawprices] (split line #",")]
    [(fmt/parse infmt rawdate)
     (map #(Float/parseFloat %) rawprices)]))

(defn- readheaders
  "Reads the first line as headers and changes to keywords"
  [headerline]
  (let [headersraw (split headerline #",")]
    (rest headersraw)))

(defn readfile
  "Produces a map like follows:
  {:dates [d1 d2 ...]
  :names [n1 n2 ...]
  :values [[val_n1d1 val_n1d2...] [val_n2d1 val_n2d2...]]}
  If no format-string is specified, default-format is used"
  ([filename format-string]
    (with-open [rdr (io/reader (str *filepath* filename))]
      (let [headers (vec (readheaders (first (line-seq rdr))))
            infmt (fmt/formatter format-string)
            rows (doall (map #(readline % infmt) (line-seq rdr)))]
        (into (sorted-map)
          (for [[date values] rows]
            [date (zipmap headers values)])))))
  ([filename]
    (readfile filename default-format)))

(defn writefile
  "Writes time-series ts to filename
  dates are formatted with format-string"
  ([ts filename format-string]
  (let [outfmt (fmt/formatter format-string)]
    (with-open [wtr (io/writer (str *filepath* filename))]
      (.write wtr (str "Dates," 
                       (join "," (-> ts first val keys)) 
                       "\n"))
      (doseq 
        [[date namevals] ts]
        (.write wtr (str (fmt/unparse outfmt date) 
                         "," 
                         (join "," (vals namevals)) "\n"))))))
  ([ts filename]
    (writefile ts filename default-format)))
    

