(ns clj-time-series.core
  (:require 
    [clj-time.core :as ct]
    [clj-time.format :as fmt]
<<<<<<< HEAD
    [incanter.core :as incanter]
    [incanter.charts :as charts]))
=======
    [incanter.core :refer [view]]
    [incanter.charts :refer [time-series-plot add-lines]]))
>>>>>>> vector_in_map

(def outfmt (fmt/formatter "dd-MMM-yyyy"))
(def infmt (fmt/formatter "yyyyMMdd"))

<<<<<<< HEAD
(defn series-names
  "Returns names of all the series"
  [ts]
  (keys (dissoc ts :dates)))

(defn to-date [datestring]
  "Converts a string like infmt into a clj-time date-time object"
  (fmt/parse infmt datestring))

(def to-rows
  "Returns a map with dates as keys and an {:assetname value} map as values"
  (memoize
    (fn
      [ts]
      (let [
        dates (:dates ts)
        s-names (series-names ts)
        rows (apply map vector (map ts s-names))]
        (zipmap dates (map #(zipmap s-names %) rows))))))

(def to-cols
  "Returns usual TS format from rows"
  (memoize
    (fn
      [ts-rows]
      (let [
        dates-col (sort (keys ts-rows))
        dates {:dates dates-col}
        asset-names (keys (val (first ts-rows)))
        asset-cols (map #(map % (map ts-rows dates-col)) asset-names)
        assets (zipmap asset-names asset-cols)]
        (into dates assets)))))

(defn from
  "Returns a subset of the ts from the given date"
  [ts datestring]
  (let [
    date (to-date datestring)
    at-or-after (fn [x] (or (= (key x) date) (ct/after? (key x) date)))
    rows (to-rows ts)]
    (to-cols
      (into {}
        (filter at-or-after rows)))))
=======
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
    
>>>>>>> vector_in_map

(defn to
  "Returns a subset of the ts to the given date"
  [ts datestring]
<<<<<<< HEAD
  (let [
    date (to-date datestring)
    at-or-before (fn [x] (or (= (key x) date) (ct/before? (key x) date)))
    rows (to-rows ts)]
    (to-cols
      (into {}
        (filter at-or-before rows)))))
=======
  (let [from-date (to-date datestring)
    at-or-before (fn [x] (or (= x from-date) (ct/before? x from-date)))]
    (filter-dates ts at-or-before)))
>>>>>>> vector_in_map

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
<<<<<<< HEAD
  ([ts1 ts2]
  (let [
    dates (clojure.set/intersection 
      (set (:dates ts1))
      (set (:dates ts2)))
    rows1 (select-keys (to-rows ts1) dates)
    rows2 (select-keys (to-rows ts2) dates)]
    (to-cols (merge-with merge rows1 rows2))))
  ([ts1 ts2 & tss]
    (apply join (join ts1 ts2) tss)))
=======
  [& tss]
  (let [
    dates (apply clojure.set/intersection (map (comp set :dates) tss))
    cols (map #(filter-dates % dates) tss)]
    (reduce (fn 
          [{dates1 :dates names1 :names vals1 :values} 
           {names2 :names vals2 :values}]
          {:dates dates1 :names (into names1 names2) :values (into vals1 vals2)})
        cols)))
>>>>>>> vector_in_map

(defn colmap
  "Takes a function to apply on value columns
  Optionally takes a function for the date column
  optionally takes a suffix to apply to asset names"
  ([ts colfun datefun suffix]
<<<<<<< HEAD
    (let [assets (dissoc ts :dates)
        dates (ts :dates)]
      (into {:dates (datefun dates)}
        (for [[sname values] assets] 
          [(-> sname (name) (str suffix) (keyword)) 
           (colfun values)]
           ))))
  ([ts colfun datefun] (colmap ts colfun datefun ""))
  ([ts colfun] (colmap ts colfun identity "")))

(defn colreduce
  "Reduce with acc and f across columns
  in the order specified by s-names.
  If s-names aren't specified it's assumed alphabetical"
  ([ts colfun s-names colname]
    (let [rows (to-rows ts)
        dates (ts :dates)
        reduced 
          (for [date dates :let [row (select-keys (rows date) s-names)]]
            (zipmap
              s-names
              (reduce colfun row)))]
          {:dates dates :reduced reduced})))
          
=======
    (-> ts
      (update-in [:dates] datefun)
      (update-in [:values] (partial map colfun))
      (update-in [:names] (fn [names] (map #(str % suffix) names)))))
  ([ts colfun datefun] (colmap ts colfun datefun ""))
  ([ts colfun] (colmap ts colfun identity "")))
  

>>>>>>> vector_in_map

(defn graph
  "Shows a graph with all time serieses in ts"
  [ts]
  (let 
    [dates (map #(.getMillis %) (:dates ts))
<<<<<<< HEAD
    [name1 & other-names] (series-names ts)    
    chart (charts/time-series-plot dates (name1 ts)
      :x-label "" :y-label "" :legend true :series-label (str name1))
    add-to-chart (fn [chart sname] 
      (charts/add-lines chart dates (ts sname)
        :series-label (str sname)))
    chartall (reduce add-to-chart chart other-names)]
=======
    [name1 & other-names] (:names ts)
    [col1 & other-cols] (:values ts)
    chart (charts/time-series-plot dates col1
      :x-label "" :y-label "" :legend true :series-label (str name1))
    add-to-chart (fn [chart [col sname]] 
      (charts/add-lines chart dates col
        :series-label (str sname)))
    chartall (reduce add-to-chart chart (map vector other-cols other-names))]
>>>>>>> vector_in_map
    (incanter/view chartall)))

(defn show [ts]
  "Shows the time serieses as a table"
  (let 
    [dates (map #(fmt/unparse outfmt %) (:dates ts))
<<<<<<< HEAD
    table (apply map vector dates (map ts (series-names ts)))]
=======
    table (apply map vector dates (ts :values))]
>>>>>>> vector_in_map
    (incanter/view table)))
