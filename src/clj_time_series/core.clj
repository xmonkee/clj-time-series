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
  [dates names & cols]
  {:dates dates
  :names names
  :values cols})
  
(defn- to-date [datestring]
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
          (apply map vector
            (filter 
              (fn [[name col]] ((set names) name)) 
              (map vector (ts :names) (ts :values))))]
        (assoc ts :names fnames :values fvalues)))
    
(defn join
  "Joins 2 or more sets of time-serieses.
  Only takes dates present in both"
  [& tss]
  (let [
    dates (apply clojure.set/intersection (map (comp set :dates) tss))
    cols (map #(filter-dates % dates) tss)]
    (reduce (fn 
          [{dates1 :dates names1 :names vals1 :values} 
           {names2 :names vals2 :values}]
          {:dates dates1 :names (into names1 names2) :values (into vals1 vals2)})
        cols)))

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
    chart (charts/time-series-plot dates col1
      :x-label "" :y-label "" :legend true :series-label (str name1))
    add-to-chart (fn [chart [col sname]] 
      (charts/add-lines chart dates col
        :series-label (str sname)))
    chartall (reduce add-to-chart chart (map vector other-cols other-names))]
    (incanter/view chartall)))

(defn show [ts]
  "Shows the time serieses as a table"
  (let 
    [dates (map #(fmt/unparse outfmt %) (:dates ts))
    table (apply map vector dates (ts :values))]
    (incanter/view table)))
