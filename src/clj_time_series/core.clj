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

  
(defn make-ts
    "Returns a ts from given dates, names and cols"
    [dates names cols]
    (into (sorted-map)
      (for [[date & values] (apply map list dates cols)]
        [date (zipmap names values)])))

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

 (defn join-ts
   "Joins 2 or more sets of time-serieses.
   Only takes dates present in both"
   ([ts1 ts2]
    (into (sorted-map)
      (for [[date namevals] ts1 :when (ts2 date)]
        [date (into namevals (ts2 date))])))
   ([ts1 ts2 & tss]
     (apply join-ts (join-ts ts1 ts2) tss)))
  
  (defn join-ts-with
    "Makes a new ts with the first columns of 2 tss
    joined together with supplied function f"
    [ts1 ts2 f namefun]
     (into (sorted-map)
      (for [[date namevals] ts1 :when (ts2 date)]
        [date {(namefun (-> namevals first key) (-> date ts2 first key)) (f (-> namevals first val) (-> date ts2 first val))}]
        ))) 
  
 (defn rowfun
  "Takes a function f and vector of names and performs 
  (f asset1 asset2 asset3 ...) (in the order of names supplied
  for each row and returns a single column ts with the above result"
  [ts f names]
  (into (sorted-map)
    (for [[date namevals] ts]
      [date {(apply str names) (apply f (map namevals names))}]))) 
   
 (defn get-cols
    "Returns the asset columns from a ts"
    [ts names dates]
    (for [name names]
      (for [date dates]
        (get-in ts [date name]))))


 (defn colmap
   "Takes a function to apply on value columns
   Optionally takes a function for the date column
   optionally takes a function to apply to asset names"
  ([ts colfun datefun] (colmap ts colfun datefun identity))
  ([ts colfun] (colmap ts colfun identity identity))
  ([ts colfun datefun namefun]
    (let [names (names ts)
          dates (dates ts)
          cols (get-cols ts names dates)]
      (make-ts (datefun dates)
               (map namefun names)
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
