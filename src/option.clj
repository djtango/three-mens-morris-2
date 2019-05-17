(ns option
  (:refer-clojure :exclude [map apply some?])
  (:require [clojure.spec.alpha :as s]))

(def option-tag? #{:option/some :option/none})

(defn ->some [x]
  [:option/some x])

(defn ->none [x]
  [:option/none x])

(defn some? [x]
  {:pre [(and (vector? x)
              (= 2 (count x))
              (option-tag? (first x)))]}
  (= :option/some (first x)))

(defn map [f opt-a]
  (if (some? opt-a)
    (->some f (second opt-a))
    (->none (second opt-a))))

(defn return [x]
  (if-not (nil? x)
    (->some x)
    (->none x)))

(defn apply [opt-f opt-a]
  (if (and (some? opt-f)
           (some? opt-a))
    (->some ((second opt-f)
             (second opt-a)))
    (->none [(second opt-f) (second opt-a)])))

(defn bind [f opt-a]
  nil
  ;;(if (and (some? f) (some? opt-a))
  ;;(f (second opt-a)))
  )

(s/fdef bind
        :args (s/cat :f ifn?
                     :opt-a (option-of any?))
        :ret any?
        :fn )

(defn get-input []
  (->some (str (rand-int 10))))

(defn parse-input [s]
  (try (let [i (Integer/parseInt s)]
         (->some i))
       (catch Throwable _
         (->none s)))) ;; nested exceptions...!? and spec failures

(defn validate-position [i]
  (if (#{0 1 2 3 4 5 6 7 8} i)
    (->some i)
    (->none i)))

(comment
  (ns option)
  (let [input (read-line)
        i (parse-input input)]
    (if (s/valid? ::position i)
      [nil i]
      (get-move state)))
  )
(defn ->move [i]
  [nil i])

(defmacro option-of [optn-spec]
  (assert (s/valid? (s/or :spec s/spec? :pred ifn?) optn-spec) "optn-spec must be a spec or function")
  `(s/or :some (s/tuple option-tag? ~optn-spec)
         :none (s/tuple #{:option/none} any?)))

(s/def ::opt-s (option-of string?))

(s/fdef get-input
        :args (s/cat)
        :ret (option-of string?))

(s/fdef parse-input
        :args (s/cat :s string?)
        :ret (option-of integer?))

(s/fdef validate-position
        :args (s/cat :i integer?)
        :ret (option-of integer?))

(s/fdef do-it!
        :args (s/cat)
        :ret (s/nilable (s/tuple nil? integer?)))

(comment
  (ns option)
  (defn do-it! []
    (bind ->move
          (bind validate-position
                (bind parse-input
                      (get-input)))))
  (require '[clojure.spec.test.alpha :as stest])
  (stest/check `do-it!)

  (->> (get-input)
       (>>= parse-input)
       (>>= validate-position)
       (>>= ->move))
  )

(defn move-piece [from to]
  [from to])

(defn allowed? [a b]
  (<= (rand) 0.5))

(defn check-allowed [[a b]]
  (if (allowed? a b)
    (->some [a b])
    (->none [a b])))

(comment
 (let [from-posn (->> (get-input)
                      (>>= parse-input)
                      (>>= validate-position))
       to-posn (->> (get-input)
                    (>>= parse-input)
                    (>>= validate-position))
       ]
   (apply (map (fn [from]
                 (partial allowed? from))
               from-posn)
          to-posn))
 )

(def >>= bind)
(def <$> map)
(def <*> apply)
(def p* (fn [f] (fn [a] (partial f a))))

;; (let [from-posn (->> (get-input)
;;                      (>>= parse-input)
;;                      (>>= validate-position))
;;       to-posn (->> (get-input)
;;                    (>>= parse-input)
;;                    (>>= validate-position))]
;;   (apply (map (fn [from]
;;                 (partial check-allowed from))
;;               from-posn)
;;          to-posn))
;;

;; (let [from-posn (->> (get-input)
;;                      (>>= parse-input)
;;                      (>>= validate-position))
;;       to-posn (->> (get-input)
;;                    (>>= parse-input)
;;                    (>>= validate-position))]
;;   (->> from-posn
;;        (<$> (p* check-allowed))
;;        (<*> to-posn)))

;; (do (let [piece-to-move (atom nil)
;;           new-position (atom nil)]
;;       (println "choose a piece to move:")
;;       (reset! piece-to-move (parse-input (read-line)))
;;       (println "choose where to move the piece:")
;;       (reset! new-position (parse-input (read-line)))
;;       (if (and (s/valid? ::position @piece-to-move)
;;                (s/valid? ::position @new-position)
;;                (allowed? state @piece-to-move @new-position))
;;         [@piece-to-move @new-position]
;;         (get-move state))))
