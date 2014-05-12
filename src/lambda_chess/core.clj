(ns lambda-chess.core
  (:require [clj-chess-engine.core :as chess]))

(defn check? [board am-i-white?]
  (chess/check? board (not am-i-white?) []))

(def col-map
  {\a 0 \b 1 \c 2 \d 3 \e 4 \f 5 \g 6 \h 7})

(def row-map
  {\1 0 \2 1 \3 2 \4 3 \5 4 \6 5 \7 6 \8 7})

(defn convert-to-index [s]
  (+ (* (col-map (first s)) 8) (row-map (second s))))


(defn my-apply-move [board [from to]]
  (let [from-i (convert-to-index from)
        to-i (convert-to-index to)
        piece (get board from-i)]
    (assoc (assoc board from-i \-) to-i piece)))

(defn try-check [board valid-moves am-i-white?]
  (get valid-moves (ffirst (drop-while #(not (check? (second %) (not am-i-white?)))
                 (map-indexed (fn [idx itm] [idx (my-apply-move board itm)])
                              valid-moves)))))

;; my-fn is the var pointing onto the function we want to test
(def my-fn
  ;;-----------------
  ;; copy and past this anonymous function on lambda-zone.com
  (fn [{:keys [board valid-moves am-i-white? in-check? history state]}]
    (or (let [res (try-check board valid-moves am-i-white?)] (when res (println "CHECK MATE!!! CHECK MATE!!! CHECK MATE!!! CHECK MATE!!!")) res)
                      (let [v (into [] valid-moves)]
                        (let [move (rand-int (count valid-moves))]
                          {:move (get v move), :state nil})))))


(defn test-fn-vs-fn []
  "play a white function against a black function"
  (chess/trace-game-play {:white my-fn :black my-fn}))
(defn test-fn-vs-human []
  "play a white function against a human playing black"
  (chess/trace-game-play {:white my-fn}))
(defn test-human-vs-fn []
  "play a human playing white against a black function"
  (chess/trace-game-play {:black my-fn}))

;; un comment the type of test you want to run
;; i.e. do you want to play 2 functions agains one another or a function against an interactive prompt?
(defn -main []
  (test-fn-vs-fn)
  ;;(test-fn-vs-human)
  ;;(test-human-vs-fn)

  )

