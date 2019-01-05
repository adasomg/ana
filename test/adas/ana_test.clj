(ns adas.ana-test
  (:require [adas.ana :as ana :refer [af awhen aif acond aor sand sif sor aand]]
            [clojure.test :as t :refer [is deftest with-test]]
            [clojure.walk :as walk]))

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
    (aand 20 (+ 10 *-1))
    (aand 10 (aand 10 (+ 10 *-1 **-1)))
    (aand 3 99 3 3 1
          (aand 3 3 99 3 1
                (aand 3 3 3 99 1
                      (+ (+ *1 *2 *3 *5)             ;=10
                         (+ **1 **2  **4 **5)        ;=10
                         (+ ***-1 ***-2 ***-3 ***-5) ;=10
                         ))))
    (awhen 10 
           (acond
            false [%t %t %t %t! %tt %tt! (awhen %tt %ttt!)]
            5 (aand 3 99 3 3 1 true
                    (aand 3 3 99 3 1 (+ **-3 **-3 *1 *5)
                          (aand 3 3 3 99 1
                                (+ (+ *1 *2 *3 *5)             ;10
                                   (aor (not ****6) ***6)      ;10
                                   (+ ***-2 ***-3 ***-3 ***-3) ;10
                                   (- %ttest!)                 ;-10
                                   %t %test                    ;10
                                   ))))
            false [%t %t %t %t! %tt %tt! (awhen %tt %ttt!)]))
    (and 20 (+ 10 20))
    ((af (and 20 (+ 10 *1))))
    (aand 30
          (inc *-1) (inc *-1) (inc *-1)
          (dec *-1) (dec *-1) (dec *-1)))))

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



(deftest side-effects-then-else
  (is
   (=
    15
    (let [a (atom 0)]
      (aif 1 (do %else %else %else %t %test %else! %else! @a) (swap! a #(+ 5 %))))

    (let [a (atom 0)]
      (aif false (swap! a #(+ 5 %)) (do %then %then %then %t! %test! %then! %then! @a))))))

;; EXPANSIONS
(defn pretty-expand [form]
  (let [r (atom {})
        c (atom 0)
        form (walk/macroexpand-all form)]
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

(deftest when-expansion
  (is (= (pretty-expand '(adas.ana/awhen 9 %test))
         (pretty-expand '(when-let [test_1 9] test_1)))))

(deftest if-expansion
  (is (= (pretty-expand '(adas.ana/aif false 2000 %test))
         (pretty-expand '(if-let [test_1 false] 2000 test_1)))))

(deftest if-expansion2
  (is (= (pretty-expand '(adas.ana/aif false 2000 %test!))
         (pretty-expand '(if false  2000 false)))))

(deftest acond-expansion
  (is (= (pretty-expand '(adas.ana/acond false 2000 :CLAUSE %test!))
         (pretty-expand '(cond false  2000 :CLAUSE :CLAUSE)))))



