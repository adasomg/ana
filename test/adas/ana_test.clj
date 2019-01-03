(ns adas.ana-test
  (:require [adas.ana :as ana :refer [af awhen aif acond aor sand sor aand]]
            [clojure.test :as t :refer [is deftest]]
            [clojure.walk :refer [macroexpand-all] :as walk]))

(defn pretty-expand [form]
  (let [r (atom {})
        c (atom 0)
        form (macroexpand-all form)]
    (letfn
        [(walker [a]
           (cond (and
                  (symbol? a)
                  (re-find #"((^G__)|(^ANA_BIND_)|(^ANA_COPY_)|(^temp__)|(^test_))"
                           (name a)))
                 (if (not (@r a)) (let [g (symbol (str "PRETTY_" (swap! c inc)))]
                                    (do (swap! r assoc a g)
                                        g)) a)
                 
                 (coll? a) (walk/walk walker identity a)
                 :else a))](walk/walk walker identity form)
        (walk/postwalk-replace @r form))))

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
    (acond 30 (+ 10 %t))
    (aif 5 (acond 30 (+ 5 %t %tt)))
    (awhen 5 (acond 30 (+ 5 %t %tt)))
    (awhen 14 (awhen 1 (acond 20 (+ 5 %t %tt %ttt))))
    ((af (when 14 (awhen 1 (cond 20 (+ 5 %t %tt %ttt)))))))))

(deftest ors
  (is
   (=
    (or false (not false))
    (aor false (not *1))
    ((af (or false (not false)))))))

(deftest ands
  (is
   (=
    30
    (aand 20 (+ 10 *1))
    (and 20 (+ 10 20))
    ((af (and 20 (+ 10 *1)))))))

(deftest crazy
  (is
   (=
    6
    ((af (if 1 (and 1 (+ *1 %test! %test %test %test %test!)))))
    (aif 1 (aand 1 (+ *1 %test! %test %test %test %test!)))
    (if 1 (and 1 (+ 1 1 1 1 1 1))))))

(deftest side-effects
  (is
   (=
    6
    (let [a (atom 0)]
      ((af (when (swap! a inc) (do %test %t %t! %t %t! %t! %t! %test! @a)))))
    (let [a (atom 0)]
      (when-let [t (swap! a inc) ] (do t t
                                       (swap! a inc) t
                                       (swap! a inc)
                                       (swap! a inc)
                                       (swap! a inc)
                                       (swap! a inc) @a)))
    (let [a (atom -10)]
      ((af (when (swap! a #(+ 2 %))
             %t %t (when 30 %tt %ttest! (acond (swap! a #(+ 2 %)) %test) %ttest!
                         (acond (swap! a #(+ 2 %)) %t!)) %test!  %t!
             @a)))))))

(deftest when-expansion
  (is (= (pretty-expand '(awhen 9 %test))
         (pretty-expand '(when-let [test_1 9] test_1)))))

(deftest if-expansion
  (is (= (pretty-expand '(aif false 2000 %test))
         (pretty-expand '(if-let [test_1 false] 2000 test_1)))))

(deftest if-expansion2
  (is (= (pretty-expand '(aif false 2000 %test!))
         (pretty-expand '(if false  2000 false)))))

(deftest acond-expansion
  (is (= (pretty-expand '(acond false 2000 :CLAUSE %test!))
         (pretty-expand '(cond false  2000 :CLAUSE :CLAUSE)))))

