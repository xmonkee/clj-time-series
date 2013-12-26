(ns clj-time-series.readfile
  (:require 
  	[clojure.java.io :as io]
  	[clj-time.format :as fmt]))

(def ^:dynamic *filepath* "data/")

(def ddmmmyyyy (fmt/formatter "dd-MMM-yy"))

(defn readline 
	"Parses time series data into [clj-time [values]]"
	[line] 
	(let [[rawdate & rawprices] (clojure.string/split line #",")]
		[(fmt/parse ddmmmyyyy rawdate)
		 (map #(Float/parseFloat %) rawprices)]))

(defn readheaders
	"Reads the first line as headers and changes to keywords"
	[headerline]
	(let [headersraw (clojure.string/split headerline #",")]
		(map #(keyword (.toLowerCase %)) (rest headersraw))))

(defn readfile
	"Produces a map like follows:
	{:dates [d1 d2 ...]
	:names [n1 n2 ...]
	:values [[val_n1d1 val_n1d2...] [val_n2d1 val_n2d2...]]}"
	([filename]
		(with-open [rdr (io/reader (str *filepath* filename))]
			(let 
				[headers (vec (readheaders (first (line-seq rdr))))
				rows (doall (map readline (line-seq rdr)))
				dates (vec (map first rows))
				values (vec (apply map vector (map second rows)))]
				{:dates dates
				 :names headers
				 :values values}))))

