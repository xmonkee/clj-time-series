(ns finsys.moving-averages
	(:require [clj-time-series.core :as cts]))

(defn- part-fun [fun n s]
  (fn [col]
  	(map fun (partition n s col))))

(defn moving-averages [ts n]
	(let [
		avg (fn [col] (/ (apply + col) n))
		colfun (part-fun avg n 1)
		datefun (part-fun last n 1)]
		(cts/colmap ts colfun datefun (str "/" n))))
