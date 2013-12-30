(ns finsys.mov-avg
  (:require [clj-time-series.core :as cts]
        [clj-time-series.io :as io]))

(defn- part-fun [fun n s]
  (fn [col]
    (map fun (partition n s col))))

(defn moving-average [ts n]
  (let [
    avg (fn [col] (/ (apply + col) n))
    colfun (part-fun avg n 1)
    datefun (part-fun last n 1)]
    (cts/colmap ts colfun datefun (str "/" n))))
  
(def avgs
  (let [
    ts (io/readfile "TimeSeries2.csv")]
    (apply cts/join (map #(moving-average ts %) [5 10 20 50]))))

(def diffs
  (let [
    rows  (apply map vector (avgs :values))
    pairdiffs (for [row rows] (map - row (rest row))) 
    cols (apply map vector pairdiffs)
    names (map #(str %1 "-" %2) (avgs :names) (rest (avgs :names)))]
    (cts/make-ts (avgs :dates) names cols)))    
