(ns clj-time-series.readfile
  (:require 
  	[clojure.java.io :as io]
  	[clj-time.format :as fmt]))

(def ^:dynamic *filepath* "data/")

(def ddmmmyyyy (fmt/formatter "dd-MMM-yy"))

(defn- readline 
	"parses time series data into clj-time date and long"
	[line] 
	(let [[rawdate & rawprices] (clojure.string/split line #",")]
		(into [(fmt/parse ddmmmyyyy rawdate)]
			(map #(Float/parseFloat %) rawprices))))

(defn- readheaders
	"reads the first line as headers and changes to keywords"
	[headerline]
	(let [headersraw (clojure.string/split headerline #",")]
		(conj 
			(map #(keyword (.toLowerCase %)) (rest headersraw))
			:dates)))

(defn readfile
	"Produces a vector like [[:date :Index1 :index 2] [date11 level11 level21] [date12 level12 level22]]"
	([filename]
		(with-open [rdr (io/reader (str *filepath* filename))]
			(let 
				[headers (readheaders (first (line-seq rdr)))
				data (doall (map readline (line-seq rdr)))
				cols (apply map vector data)]
				(zipmap headers cols)))))

