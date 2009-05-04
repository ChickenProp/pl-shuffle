(ns pl-shuffle
  (:require [clojure.zip :as zip])
  (:require [clojure.contrib.lazy-xml :as xml])
  (:use clojure.contrib.zip-filter.xml)
  (:use com.infolace.format)
  (:import (java.util ArrayList Collections))) ;required for shuffle

(defn shuffle
  "Shuffles coll using a Java ArrayList."
  [coll]
  (let [l (ArrayList. coll)]
    (Collections/shuffle l)
    (seq l)))

(defn weighted-shuffle
  "Shuffles a collection, putting highly-weighted elements near the front with
higher probability.

The weight of an element of coll is given by (weightfn element). weightfn
defaults to first.

Elements with weights of 0 will go to the back, in random order. Negative
weights cause undefined behaviour."
  ([coll]
     (weighted-shuffle first coll))
  ([weightfn coll]
     ;; We shuffle coll so that if multiple elements have weight 0, they appear
     ;; in the result in an order which is not dependant on their original
     ;; positions.
     ;; This is O(n^2). An O(n log n) implementation would be to generate a
     ;; random number based on the weight of each element, and sort according to
     ;; these. To get the same results, the distribution would be continuous 
     ;; with the property that P(D(a) < D(b)) = a/(a+b).
     ;; In this case, the shuffling would be useful in the unlikely event of two
     ;; elements recieving the same random number.
     (let [scoll      (shuffle coll)
	   vals	    (ArrayList. scoll)
	   wgts	    (ArrayList. (map weightfn scoll))]
       (loop [#^Integer total	(apply + wgts)
	      #^Integer target	(rand-int total)
	      #^Integer start	0
	      #^Integer i		0]
	 (cond (= (inc start) (count coll))
	       true
	       (> (.get wgts i) target)
	       (let [tmpv (.get vals i)
		     tmpw (.get wgts i)
		     newtot (- total tmpw)]
		 (.set vals i (.get vals start))
		 (.set vals start tmpv)
		 (.set wgts i (.get wgts start))
		 (.set wgts start tmpw)
		 (recur newtot (rand-int newtot) (inc start) (inc start)))
	       (= (inc i) (count coll))
	       true
	       true
	       (recur total (- target (.get wgts i)) start (inc i))))
       (seq vals))))

(defn index-of
  "Finds the index in coll of the first element for which (pred item elt) is
true. pred defaults to ="
  ([item coll]
     (index-of item coll =))
  ([item coll pred]
     (first (filter #(if % true false)
		    (map #(if (pred item (nth coll %)) %)
			 (range (count coll)))))))

(defn getattr [hsh key]
  "Gets an attribute from a zipped hashmap."
  (let [a (attr hsh key)]
    (cond (#{:id :rating :playcount :lastplay} key)
	    (if (or (= a "") (= a nil)) ; sometimes they don't show up/are empty
	      0
	      (Long. a))
	  true
	    a)))

(defn ts-to-str
  "Converts a mac timestamp to a string. 0 becomes \"--\"."
  [ts]
  (if (= ts 0) "--"
      (. (java.text.SimpleDateFormat. "yyyy-MM-dd HH:mm:ss")
	 format (java.util.Date. (* (- ts 2082848400)
				    1000)))))

;; The order is important for print-song. That's silly.
(def relevant-keys '(:id :title :album :lastplay :rating :playcount))
(defn get-songs [tree]
  "Given an xml tree structure, returns only the relevant keys of the songs in
it."
  ;; No real reason for just the relevant ones. But currently no reason to
  ;; change.
  (xml-> (zip/xml-zip tree)
	 :files :file (fn [loc]
			(zipmap relevant-keys
				(map #(getattr loc %)
				     relevant-keys)))))


(defn print-song [song]
  "Prints some of the information about a song, formatted to 80 characters.
Fields are padded/truncated as necessary."
  ;; depends on the order of relevant-keys.
  (apply cl-format true "~4A ~20A   ~20A   ~19A   ~3A ~A~%"
	 (map (fn [key]
		(let [s (song key)]
		  (cond (#{:title :album} key)
			  (subs s 0 (min (.length s) 20))
			(= key :lastplay)
			  (ts-to-str s)
			true s)))
	      relevant-keys)))

(defn add-last-played-weights [songs]
  "Given a collection of songs, add a field :lpw to each. The most recently
played song has :lpw 1, the next most recent has :lpw 2, etc."
  ;; Maybe this should be based on timestamp instead, probably logarithmically?
  ;; then I wouldn't need to pass the entire songlist around.
  ;; Shuffle because unplayed files have the same timestamp, and would otherwise
  ;; be sorted by id.
  (let [sorted (sort-by #(- (% :lastplay))  (shuffle songs))]
    (map #(assoc (nth sorted %) :lpw (inc %))
	 (range (count songs)))))

(defn add-weights
  "Given a collection of songs, add a field :weight to each, taking into account
rating, playcount, and when it was last played relative to the other songs in
the collection."
  [songs]
  (map (fn [song]
	 (assoc song :weight
	   (let [#^Integer rt (song :rating)
		 #^Integer pc (song :playcount)
		 #^Integer lp (song :lpw)]
	     (if (= rt 20) 0
		 (* (if (= rt 0) 200 rt)
		    (+ (int (* 1000 (/ lp (count songs))))
		       (int (/ 1000 (inc pc)))))))))
       (add-last-played-weights songs)))

(defn songs-to-playlist
  "Given a collection of songs, return a playlist of them with the given name
and (if provided) id."
  ([coll name]
     (songs-to-playlist coll name nil))
  ([coll name id]
     {:tag :playlist
      :attrs (if id {:name name :plid id}
		 {:name name})
      :content (map (fn [s]
		      {:tag :add
		       :attrs {:id (str (s :id))}
		       :content ()})
		    coll)}))

(defn add-runthrough-playlist [tree songs]
  "Given an xml tree structure and a collection of songs, add a playlist to the
tree named \"Runthrough\" consisting of the songs, in supplied order. If such a
playlist already exists, it will be replaced."
  (zip/root (let [loc (first (xml-> (zip/xml-zip tree)
				    :playlist (attr= :name "Runthrough")))
		  id (if loc (attr loc :plid))
		  pl (songs-to-playlist songs "Runthrough" id)]
	      (if loc
		(zip/replace loc pl)
		(zip/append-child (zip/xml-zip tree) pl)))))

(let [tree (xml/parse-trim (or (nth *command-line-args* 0)
			       "/mnt/ipod/iPod_Control/.gnupod/GNUtunesDB.xml"))
      songs (get-songs tree)]
  (xml/emit (add-runthrough-playlist tree
				     (weighted-shuffle #(% :weight)
						       (add-weights songs)))
	    :pad true))
