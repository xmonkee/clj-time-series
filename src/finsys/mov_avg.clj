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
    datefun (part-fun last n 1)
    namefun #(str % "/" n)]
    (cts/colmap ts colfun datefun namefun)))
  
(defn diff
  [ts1 ts2]
  (cts/join-ts-with ts1 ts2 - #(str %1 "-" %2)))
  
(defn countpred [ts pred n suffix]
  (let [
    cnt (fn [col] (count (filter pred col)))
    colfun (part-fun cnt n 1)
    datefun (part-fun last n 1)
    namefun #(str % suffix)]
    (cts/colmap ts colfun datefun namefun)))
    
(defn pos [ts n]
  (countpred ts pos? n "+"))
  
(defn neg [ts n]
  (countpred ts neg? n "-"))

(defn signal [ts n1 n2 lookback]
  (let [ts* (diff (moving-average ts n1) (moving-average ts n2))]
    (cts/join-ts-with (pos ts* lookback) (neg ts* lookback) 
      (fn [cpos cneg]
        (cond
          (>= cpos 5)   1
          (>= cneg 5)  -1
          :else         0))
    (constantly "Signal"))))
    
 

 