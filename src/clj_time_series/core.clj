(ns clj-time-series.core
  (:require 
    [clj-time.core :as ct]
    [clj-time.format :as fmt]
    [incanter.core :as incanter]
    [incanter.charts :as charts]))

(def outfmt (fmt/formatter "yyyyMMdd"))
(def infmt (fmt/formatter "yyyyMMdd"))

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

(defn to
  "Returns a subset of the ts to the given date"
  [ts datestring]
  (let [
    date (to-date datestring)
    at-or-before (fn [x] (or (= (key x) date) (ct/before? (key x) date)))
    rows (to-rows ts)]
    (to-cols
      (into {}
        (filter at-or-before rows)))))

(defn between 
  "Returns subset between two dates (inclusive)"
  [ts startdate enddate]
  (-> ts
    (from startdate)
    (to enddate)))

(defn join
  "Joins 2 or more sets of time-serieses.
  Only takes dates present in both"
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

(defn colmap
  "Takes a function to apply on value columns
  Optionally takes a function for the date column
  optionally takes a suffix to apply to asset names"
  ([ts colfun datefun suffix]
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
          

(defn graph
  "Shows a graph with all time serieses in ts"
  [ts]
  (let 
    [dates (map #(.getMillis %) (:dates ts))
    [name1 & other-names] (series-names ts)    
    chart (charts/time-series-plot dates (name1 ts)
      :x-label "" :y-label "" :legend true :series-label (str name1))
    add-to-chart (fn [chart sname] 
      (charts/add-lines chart dates (ts sname)
        :series-label (str sname)))
    chartall (reduce add-to-chart chart other-names)]
    (incanter/view chartall)))

(defn show [ts]
  "Shows the time serieses as a table"
  (let 
    [dates (map #(fmt/unparse outfmt %) (:dates ts))
    table (apply map vector dates (map ts (series-names ts)))]
    (incanter/view table)))
