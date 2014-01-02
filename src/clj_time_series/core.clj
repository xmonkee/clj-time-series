(ns clj-time-series.core
  (:require 
    [clj-time.core :as ct]
    [clj-time.format :as fmt]
    [incanter.core :refer [view]]
    [incanter.charts :refer [time-series-plot add-lines]]))

(def outfmt (fmt/formatter "dd-MMM-yyyy"))
(def infmt (fmt/formatter "yyyyMMdd"))

(defn to-date [datestring]
  "Converts a string like infmt into a clj-time date-time object"
  (fmt/parse infmt datestring))

(def dates keys)
(defn names [ts] (-> ts first val keys))

(defn filter-dates
 "Filters a time-series based on the pred applied to the dates"
  [ts pred]
  (into (sorted-map)
    (select-keys ts
      (filter pred (dates ts))))) 

(defn select-names
  "Returns ts of only the names supplied"
  [ts names]
    (into (sorted-map)
      (for [[date namevals] ts]
        [date (select-keys namevals names)])))

(defn from
  "Returns a subset of the ts from the given date"
  [ts datestring]
  (filter-dates ts (fn [date] 
    ((complement ct/before?) date (to-date datestring)))))    

(defn to
  "Returns a subset of the ts from the given date"
  [ts datestring]
  (filter-dates ts (fn [date] 
    ((complement ct/after?) date (to-date datestring)))))    

 (defn join
   "Joins 2 or more sets of time-serieses.
   Only takes dates present in both"
   ([ts1 ts2]
    (into (sorted-map)
      (for [[date namevals] ts1 :when (ts2 date)]
        [date (into namevals (ts2 date))])))
   ([ts1 ts2 & tss]
     (apply join (join ts1 ts2) tss)))
     
 (defn get-cols
    "Returns the asset columns from a ts"
    [ts names dates]
    (for [name names]
      (for [date dates]
        (get-in ts [date name]))))
  
  (defn make-ts
    "Returns a ts from given dates, names and cols"
    [dates names cols]
    (into (sorted-map)
      (for [[date & values] (apply map list dates cols)]
        [date (zipmap names values)])))

 (defn colmap
   "Takes a function to apply on value columns
   Optionally takes a function for the date column
   optionally takes a suffix to apply to asset names"
  ([ts colfun datefun] (colmap ts colfun datefun ""))
  ([ts colfun] (colmap ts colfun identity ""))
  ([ts colfun datefun suffix]
    (let [names (names ts)
          dates (dates ts)
          cols (get-cols ts names dates)]
      (make-ts (datefun dates)
               (map #(str % suffix) names)
               (map colfun cols))))) 
	
(defn graph
  "Shows a graph with all time serieses in ts"
  [ts]
  (let 
    [dates (dates ts)
    datenums (map #(.getMillis %) dates)
    names (names ts)
    [col1 & other-cols] (get-cols ts names dates)
    chart (time-series-plot datenums col1
      :x-label "" :y-label "" :legend true :series-label (str (first names)))
    add-to-chart (fn [chart [col sname]] 
      (add-lines chart datenums col
        :series-label (str sname)))
    chartall (reduce add-to-chart chart (map vector other-cols (next names)))]
    (view chartall)))

(defn show [ts]
  "Shows the time serieses as a table"
  (let 
    [datestrs (map #(fmt/unparse outfmt %) (dates ts))
    names (names ts)
    headers (cons "Dates" names)
    table (apply map vector datestrs (get-cols ts names (dates ts)))]
    (view (cons headers table))))
