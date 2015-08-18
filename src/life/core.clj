(ns life.core
  (:require [clojure.string :refer [join]])
  (:gen-class))

(def glider
  #{[0 0] [0 2] [1 1] [1 2] [2 1]})

(defn make-grid [height width]
  {:cells #{}
   :height height
   :width width})

(defn num-neighbors [{:keys [height width cells]} [i j]]
  (let [in-bounds? (fn [[i j]]
                     (and (> i 0) (< i height) (> j 0) (< j height)))]
    (count (for [di [-1 0 1]
                 dj [-1 0 1]
                 :let [neighbor [(+ i di) (+ j dj)]]
                 :when (in-bounds? neighbor)]
             neighbor))))

(defn coords [{:keys [height width]}]
  (for [i (range height)
        j (range width)]
    [i j]))

(defn next-grid [{:keys [height width cells] :as grid}]
  (let [should-live? (fn [cell]
                       (let [n (num-neighbors grid cell)]
                         (if (cells cell)
                           (and (> n 1) (< n 3))
                           (= n 3))))]
    (->> (coords grid)
         (filter should-live?)
         (set)
         (assoc grid :cells))))

(defn all-grids [grid]
  (let [living? (fn [{:keys [cells]}] (not (empty? cells)))]
    (take-while living? (iterate next-grid grid))))

(defn grid->str [{:keys [height width cells]}]
  (let [cell->str (fn [cell] (if (cells cell) "o" "x"))
        row->str (fn [i] (apply str (map cell->str (map (partial conj [i]) (range width)))))]
  (join "\n" (map row->str (range height)))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [initial-grid {:height 12 :width 12 :cells glider}
        future-grids (all-grids initial-grid)]
    (doseq [grid future-grids]
      (println grid)
      (println (grid->str grid))
      (Thread/sleep 100))))
