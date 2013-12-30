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

 ; (defn colmap
 ;   "Takes a function to apply on value columns
 ;   Optionally takes a function for the date column
 ;   optionally takes a suffix to apply to asset names"
 ;   ([ts colfun datefun suffix]
 ;    (into (sorted-map)
 ;      (for [name (names ts)
 ;            [date {value name}] ts]
 ;        [(datefun date)]
;     (-> ts
;       (update-in [:dates] datefun)
;       (update-in [:values] (partial map colfun))
;       (update-in [:names] (fn [names] (map #(str % suffix) names)))))
;   ([ts colfun datefun] (colmap ts colfun datefun ""))
;   ([ts colfun] (colmap ts colfun identity "")))
  


(defn graph
  "Shows a graph with all time serieses in ts"
  [ts]
  (let 
    [dates (map #(.getMillis %) (:dates ts))
    [name1 & other-names] (:names ts)
    [col1 & other-cols] (:values ts)
    chart (time-series-plot dates col1
      :x-label "" :y-label "" :legend true :series-label (str name1))
    add-to-chart (fn [chart [col sname]] 
      (add-lines chart dates col
        :series-label (str sname)))
    chartall (reduce add-to-chart chart (map vector other-cols other-names))]
    (view chartall)))

(defn show [ts]
  "Shows the time serieses as a table"
  (let 
    [dates (map #(fmt/unparse outfmt %) (:dates ts))
    table (apply map vector dates (ts :values))]
    (view table)))
