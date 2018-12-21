(ns adas.ana-test
  (:require [adas.ana :as ana :refer [af awhen aif acond]]
            [clojure.test :as t :refer [is deftest]]))

(deftest basic
  (is
   (=
    ((af
      (when 30 (if 40
                 (+ %t %tt)))))
    ((fn []
       (when 30 (if 40
                  (+ 40 30)))))
    (awhen 30 (aif 40
                   (+ %t %tt))))))

(deftest conds
  (is
   (=
    (cond 30 (+ 10 30))
    (acond 30 (+ 10 %t)))))
