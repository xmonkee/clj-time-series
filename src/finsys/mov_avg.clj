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
	
(defn tester [xxx]
	(let [
		ts (io/readfile "TimeSeries2.csv")
		mas (apply cts/join ts (map #(moving-average ts %) [20]))]
		(cts/graph ts)))