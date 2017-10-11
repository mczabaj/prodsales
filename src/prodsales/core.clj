(ns prodsales.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:gen-class))

(def products-file (slurp "resources/products.tab"))
(def sales-file (slurp "resources/sales.tab"))

(def products-rows (str/split products-file #"\n"))
(def sales-rows (str/split sales-file #"\n"))

; (defn header-row [row]
;   (let [head-split (str/split row #"\t")
;         head-dash (map #(str/replace % " " "-") head-split)]
;     (map keyword head-dash)))

(def products-data (map #(str/split % #"\t") products-rows))
(def sales-data (map #(str/split % #"\t") sales-rows))

(def product-maps (map #(zipmap [:name :category] %) products-data))
(def sales-maps (map #(zipmap [:name :sale-price] %) sales-data))

(defn str->num [m kw]
  (let [str (get m kw)]
    (assoc m kw (read-string str))))

(def sales-maps-num (map #(str->num % :sale-price) sales-maps))

(def prod-by-name (group-by :name product-maps))
(def sales-by-name (group-by :name sales-maps-num))

(def merged (merge-with merge sales-by-name prod-by-name))

;; Q1 What category has the highest average sales price?
;;    (Please include the average sale price)
(defn average [coll]
  (let [sum (apply + coll)
        count (count coll)]
    (if (pos? count)
      (/ sum count)
      0)))

(def prod-by-cat (group-by :category product-maps))

(def cats (map #(get prod-by-cat %) (vec (keys prod-by-cat))))

(defn cat-prices [cat]
  (flatten (map #(get merged %) (map #(get % :name) cat))))

(defn average-cat-price [cat]
  {:category (:category (first cat))
   :average (average (remove nil? (map #(get % :sale-price) (cat-prices cat))))})

(def category-and-average (map average-cat-price cats))

(defn get-max [list]
  (apply max (map :average list)))

(defn get-max-cat [list max]
  (for [m list
        :when (= max (:average m))]
    m))

(def max-cat-average (first (get-max-cat category-and-average (get-max category-and-average))))

;; Q2 What is the minimum and maximum sale in the category 'Breakfast'
(def bfast (get prod-by-cat "Breakfast"))

(def bfast-prices (flatten (map #(get merged %) (map #(get % :name) bfast))))

(def max-bfast-price (apply max (remove nil? (map :sale-price bfast-prices))))
(def min-bfast-price (apply min (remove nil? (map :sale-price bfast-prices))))

(defn -main
  "read the files, print the answers"
  [& args]
  (println "Question 1: ")
  (println "Category with highest average sales price: " (:category max-cat-average))
  (println "    average: " (:average max-cat-average))
  (println "Question 2: ")
  (println "Max Breakfast price: " max-bfast-price)
  (println "Min Breakfast price: " min-bfast-price))
