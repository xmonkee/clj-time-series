(ns finsys.moving-averages
	(:require [clj-time-series.core :as cts]
			  [clj-time-series.readfile :as rf]))

(defn- part-fun [fun n s]
  (fn [col]
  	(map fun (partition n s col))))

(defn moving-averages [ts n]
	(let [
		avg (fn [col] (/ (apply + col) n))
		colfun (part-fun avg n 1)
		datefun (part-fun last n 1)]
		(cts/colmap ts colfun datefun (str "/" n))))
	
(defn tester [xxx]
	(let [
		ts (rf/readfile "TimeSeries2.csv")
		mas (apply cts/join (map #(moving-averages ts %) [5 10 20 100]))]
		(cts/graph moving-avgs)))