(ns comp-3mm-test
  (:require [clojure.test :refer :all]
            [comp-3mm :as sut]))

(deftest empty-state
  (testing "empty state should not be a win"
    (let [empty-state [[]]]
     (is (not (sut/win? empty-state))))))

(deftest first-move
  (testing "first move should not be a win"
    (let [first-move [[nil 0]]]
      (is (not (sut/win? first-move))))))

(deftest line-alternating
  (testing "a line of alternating moves should not be a win"
    (let [w-top-left [nil 0]
          b-top-mid [nil 1]
          w-top-right [nil 2]
          moves [w-top-left
                 b-top-mid
                 w-top-right]]
      (is (not (sut/win? moves))))))

(deftest line
  (testing "a white line should be a win"
    (let [w-top-left [nil 0]
          b-mid-left [nil 3]
          w-top-mid [nil 1]
          b-mid-mid [nil 4]
          w-top-right [nil 2]
          moves [w-top-left
                 b-mid-left
                 w-top-mid
                 b-mid-mid
                 w-top-right]]
      (is (sut/win? moves)))))

(deftest adjacent?
  ;; 0 1 2
  ;; 3 4 5
  ;; 6 7 8
  (testing "horizontal cells should be adjacent"
   (are [from to] (sut/adjacent? from to)
        ;; horizontal
        0 1, 1 0
        1 2, 2 1

        3 4, 4 3
        4 5, 5 4

        6 7, 7 6
        7 8, 8 7))
  (testing "vertical cells should be adjacent"
    (are [from to] (sut/adjacent? from to)
        0 3, 3 0
        3 6, 6 3

        1 4, 4 1
        4 7, 7 4

        2 5, 5 2
        5 8, 8 5
        ;; diagonal
        0 4, 4 0
        1 5, 5 1

        3 7, 7 3
        4 8, 8 4

        1 3, 3 1
        2 4, 4 2

        4 6, 6 4
        5 7, 7 5))
  (testing "diagonal cells should be adjacent"
    (are [from to] (sut/adjacent? from to)
         0 4, 4 0
         1 5, 5 1

         3 7, 7 3
         4 8, 8 4

         1 3, 3 1
         2 4, 4 2

         4 6, 6 4
         5 7, 7 5))
  (testing "row overflow should not be adjacent"
    (are [from to] (not (sut/adjacent? from to))
         ;; horizontal
         2 3, 3 2
         5 6, 6 5
         ;; diagonal
         2 6, 6 2
         3 5, 5 3
         2 0, 0 2
         8 6, 8 6))
  ;; collisions
  (testing "the same cell should not be adjacent"
    (are [cell] (not (sut/adjacent? cell cell))
         0 1 2
         3 4 5
         6 7 8)))

(deftest allowed?
  (let [first-six-moves [[nil 0] [nil 1] [nil 2]
                         [nil 3] [nil 4] [nil 5]]]
    (testing "adjacent move is allowed"
      (is (sut/allowed? first-six-moves 4 7)))
    (testing "should not be able to move something to itself"
      (is (not (sut/allowed? first-six-moves 0 0))))
    (testing "should not be able to move something to an occupied spot")
    (testing "should not be able to move a piece you don't own")
    (testing "should not be able to move an unoccupied piece")))

(deftest get-board
  (let [first-six-moves [[nil 0] [nil 1] [nil 2]
                         [nil 3] [nil 4] [nil 5]]]
    (is (= [:white :black :white :black :white :black nil nil nil]
           (sut/get-board first-six-moves)))
    (is (= [nil :black :white :black :white :black nil nil :white]
           (sut/get-board (-> first-six-moves
                              (conj [0 8])))))))
