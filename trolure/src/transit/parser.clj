(ns transit.parser
  (:require [clojure.java.io :as jio]
            [clojure.string :as cstr]))

(defn read-trips
  [path]
  (with-open [trip-stream (jio/reader path)]
    (reduce (fn [acc item] (assoc acc 
               :a (conj (:a acc) {:route-id (first item) :service-id (second item)
                                 :trip-id (nth item 2)})
               :m (assoc (:m acc) (first item) (conj (get-in acc [:m (first item)] []) (:idx acc)))
               :idx (inc (:idx acc))))
            {:a [] :m {} :idx 0}
            (map #(cstr/split % #",") (line-seq trip-stream)))))


(defn read-stoptimes
  [path]
  (with-open [stime-stream (jio/reader path)]
    (reduce #(assoc %1 
               :a (into (:a %1) [{:trip-id (first %2) :arrival (second %2)
                                 :departure (nth %2 2) :stop-id (nth %2 3)}])
               :m (assoc (:m %1) (first %2) (conj (get-in %1 [:m (first %2)] []) (:idx %1)))
               :idx (inc (:idx %1)))
            {:a [] :m {} :idx 0}
            (map #(cstr/split % #",") (line-seq stime-stream)))))
