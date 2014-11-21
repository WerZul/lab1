(ns lab1.core
  (:gen-class))
(require '[clojure.java.io :as io])
(require '[clojure.string :as str])

(declare r-heming)
(declare r-evclid)
(declare points-from-file)
(declare points-potentials); first time 
(declare potential)
(declare cluster-center)
(declare max-ppoint)
(declare potential-difference)
(declare potential-differences)
(declare clear-potentials)



(def r-a 3.0)
(def r-b (* 1.5 r-a))
(def e-max 0.5)
(def e-min 0.15)
(def alpha-koeff (/ 4 (Math/pow r-a 2)))
(def betta-koeff (/ 4 (Math/pow r-b 2)))

(defn -main
	[dist-type file-name]
	(println (str "app run with length type: " dist-type))
	(println (str "               file-name: " file-name))
	(def dist-func (case dist-type
                        "evclid" r-evclid
                        "heming" r-heming))

	(def points (points-from-file file-name))
	(defn ppoint-from-point [point] {:coords point :potential 0.0})
	(def ppoints (map ppoint-from-point points))
	; (println ppoints)
	(def potentials (points-potentials ppoints dist-func))
	; (doseq [ppoint potentials] (println ppoint))
	(def first-cluster-potential (max-ppoint potentials))
	(println first-cluster-potential)
	(def pdiff (potential-differences ppoints first-cluster-potential dist-func))
	(doseq [ppoint pdiff] (println ppoint))
)

(defn r-heming [x y]
  	{:pre [(= (count x) (count y))]}
	(defn abs [n] (max n (- n)))
	(reduce + (map abs (map - x y)))
)

(defn r-evclid [x y]
  	{:pre [(= (count x) (count y))]}
	(defn sqr [n] (* n n))
	(Math/sqrt (reduce + (map sqr (map - x y))))
)		

(defn points-from-file [file-name]
  {:pre [(not (nil? file-name))]}
	(if (.exists (io/file file-name))
	  	(with-open [rdr (io/reader file-name)]
	 	 	; (def data (line-seq rdr))
	 	 	(defn read-string [string] (vec (map #(Double/parseDouble %) (drop-last (str/split string #",")))))
	 	 	(doall (map read-string (line-seq rdr)))
	 	)
	  	(println (str "ERROR: " file-name " does not exists"))
	)
)

; FIRST POTENTIALS
(defn points-potentials [points dist-func]
  (map #(potential % points dist-func) points)
)

(defn potential [point points dist-func]
  (let [pot (reduce
          		(fn [sum point2]
          			(+
            			sum
            			(Math/pow Math/E (* (- alpha-koeff)
                			(dist-func (point :coords) (point2 :coords))))
            		)
           		)
            	0.0
            	points)]
    (assoc point :potential pot))
)

(defn potential-difference [point point-base dist-func]  
	(let [potential-diff 
          			(-
            			(point :potential)
            			(* 
            				(point-base :potential) 
            				(Math/pow Math/E (* (- betta-koeff)
            					(dist-func (point :coords) (point-base :coords))))
            			)
            		)]
    (assoc point :potential potential-diff))
)

(defn potential-differences [points point-base dist-func]
	(map #(potential-difference % point-base dist-func) points)
)

(defn max-ppoint [ppoints]
  	(apply max-key
    	(fn [ppoint] (ppoint :potential))
    	ppoints)
)

(defn clear-potentials [points target-point]
  	(map
    	(fn [point]
      		(if (=
          			(point :coords)
          			(target-point :coords))
        		(assoc point :potential 0.0)
        		point))
    	points)
)
