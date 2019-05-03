(ns comp-3mm
  (:require [clojure.spec.alpha :as s]))

(s/def ::position (into #{} (range 9)))

(defn add-move [moves next-move]
  (conj moves next-move))

(defn current-player [moves]
  (let [white-or-black (if (odd? (count moves))
                         even?  ;; white
                         odd?)] ;; black
    (->> moves
         (map-indexed (fn [idx itm]
                        (if (white-or-black idx)
                          itm
                          nil)))
         (filter identity))))

(defn latest-positions [moves]
  (reduce (fn [state [from to]]
            (-> state
                (conj to)
                (disj from)))
          #{}
          moves))

(def horizontal-line?
  #{#{0 1 2}
    #{3 4 5}
    #{6 7 8}})

(defn horizontal? [moves]
  (horizontal-line? (latest-positions moves)))

(def vertical-line?
  #{#{0 3 6}
    #{1 4 7}
    #{2 5 8}})

(defn vertical? [moves]
  (vertical-line? (latest-positions moves)))

(def diagonal-line?
  #{#{0 4 8}
    #{2 4 6}})

(defn diagonal? [moves]
  (diagonal-line? (latest-positions moves)))

(defn any-three-in-a-line? [moves]
  (or (horizontal? moves)
      (vertical? moves)
      (diagonal? moves)))

(defn win? [moves]
  (->> moves
       current-player
       any-three-in-a-line?))

(defn parse-input [input]
  (try (Integer/parseInt input)
       (catch Exception _ ::parse-input.bad-input)))

(defn adjacent? [from to]
  (let [right inc
        left dec
        down (partial + 3)
        up (partial + -3) ;; a-b != b-a
        =to (partial = to)
        left-edge #{0 3 6}
        right-edge #{2 5 8}]
    (or (-> from up =to)
        (-> from down =to)
        (-> from right =to
            (and (not (right-edge from))))
        (-> from left =to
            (and (not (left-edge from))))
        (-> from up right =to
            (and (not (right-edge from))))
        (-> from down right =to
            (and (not (right-edge from))))
        (-> from up left =to
            (and (not (left-edge from))))
        (-> from down left =to
            (and (not (left-edge from)))))))

(defn occupied? [state posn]
  (contains? (latest-positions state) posn))

(defn allowed? [state from to]
  (and (occupied? state from)
       (not (occupied? state to))
       (adjacent? from to)))

(defn get-player [selector state]
  (->> state
       (map-indexed vector)
       (filter (comp selector first))
       (map second)))

(def white-player (partial get-player even?)) ;; 0-indexed
(def black-player (partial get-player odd?))

(defn set-pieces [state k current-board]
  (let [select-player (if (= k :white)
                        white-player
                        black-player)]
    (->> state
         select-player
         latest-positions
         (reduce (fn [board posn]
                   (assoc board posn k))
                 current-board))))

(defn get-board [state]
  (let [empty-board (into [] (repeat 9 nil))]
    (->> empty-board
         (set-pieces state :white)
         (set-pieces state :black))))

(defn print-board [state]
  (->> state
       get-board
       (map {nil " "
             :black "x"
             :white "o"})
       (map (fn [x]
              (str " " x " ")))
       (partition 3)
       (map (partial clojure.string/join "|"))
       (interpose (clojure.string/join "" (repeat 11 "-")))
       (run! println)))

(defn get-move [state]
  (if (< (count state) 6)
    (do (println "enter a position 0-9 :")
        (let [input (read-line)
              i (parse-input input)]
          (if (s/valid? ::position i)
            [nil i]
            (get-move state))))

    (do (let [piece-to-move (atom nil)
              new-position (atom nil)]
          (println "choose a piece to move:")
          (reset! piece-to-move (parse-input (read-line)))
          (println "choose where to move the piece:")
          (reset! new-position (parse-input (read-line)))
          (if (and (s/valid? ::position @piece-to-move)
                   (s/valid? ::position @new-position)
                   (allowed? @piece-to-move @new-position))
            [@piece-to-move @new-position]
            (get-move state))))))

(defn play []
  (println "welcome to three mens moris")
  (loop [state [[]]]
    (println "current board:")
    (print-board state)
    (let [next-move (get-move state)
          next-state (add-move state next-move)]
      (if (win? next-state)
        (do (print-board state)
            (println "game over"))
        (recur next-state)))))
