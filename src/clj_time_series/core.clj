(ns clj-time-series.core
  (:require 
    [clj-time.core :as ct]
    [clj-time.format :as fmt]
    [incanter.core :refer [view]]
    [incanter.charts :refer [time-series-plot add-lines]]))

(def outfmt (fmt/formatter "dd-MMM-yyyy"))
(def infmt (fmt/formatter "yyyyMMdd"))

(defn make-ts
  "Construct a TS"
  [dates names cols]
  {:dates dates
  :names names
  :values cols})
  
(defn to-date [datestring]
  "Converts a string like infmt into a clj-time date-time object"
  (fmt/parse infmt datestring))

(defn filter-dates
 "Filters a time-series based on the pred applied to the dates"
  [ts pred]
  (let [[fdates fvalues] 
      (reduce
        (fn [[fdates fvalues] [date & values]]
          (if (pred date)
            [(conj fdates date) (map conj fvalues values)]
            [fdates fvalues]))
        [[] (repeat [])]
        (apply map vector (ts :dates) (ts :values)))]
      (assoc ts :dates fdates :values fvalues)))
    
 (defn from
  "Returns a subset of the ts from the given date"
  [ts datestring]
  (let [from-date (to-date datestring)
    at-or-after (fn [x] (or (= x from-date) (ct/after? x from-date)))]
    (filter-dates ts at-or-after)))
    

(defn to
  "Returns a subset of the ts to the given date"
  [ts datestring]
  (let [from-date (to-date datestring)
    at-or-before (fn [x] (or (= x from-date) (ct/before? x from-date)))]
    (filter-dates ts at-or-before)))

(defn between 
  "Returns subset between two dates (inclusive)"
  [ts startdate enddate]
  (-> ts
    (from startdate)
    (to enddate)))

(defn select-names
  "Returns ts of only the names supplied"
  [ts names]
  (let [[fnames fvalues] 
    (reduce  
      (fn [[fnames fcols] [name col]] 
        (if 
          ((set names) name)
          [(conj fnames name) (conj fcols col)]
          [fnames fcols]))
      [[] []]
      (map vector (ts :names) (ts :values)))]
    (assoc ts :names fnames :values fvalues)))
    
(defn join
  "Joins 2 or more sets of time-serieses.
  Only takes dates present in both"
  ([ts1 ts2]
  (let [
    dates (clojure.set/intersection (set (ts1 :dates)) (set (ts2 :dates)))
    _ts1 (filter-dates ts1 dates)
    _ts2 (filter-dates ts2 dates)]
    (make-ts 
      (_ts1 :dates) 
      (concat (_ts1 :names) (_ts2 :names)) 
      (concat (_ts1 :values) (_ts2 :values)))))
  ([ts1 ts2 & tss]
    (apply join (join ts1 ts2) tss)))

(defn colmap
  "Takes a function to apply on value columns
  Optionally takes a function for the date column
  optionally takes a suffix to apply to asset names"
  ([ts colfun datefun suffix]
    (-> ts
      (update-in [:dates] datefun)
      (update-in [:values] (partial map colfun))
      (update-in [:names] (fn [names] (map #(str % suffix) names)))))
  ([ts colfun datefun] (colmap ts colfun datefun ""))
  ([ts colfun] (colmap ts colfun identity "")))
  


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
