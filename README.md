# adas.ana
[![Clojars](https://img.shields.io/clojars/v/adas/ana.svg)](https://clojars.org/adas/ana)

Ana is a collection of general purpose macros with a focus on writing succint code. 
They are divided into 2 broad categories "anaphoric", and "quick".
While the anaphoras are inspired by the traditional lisp kind, popularized by Paul Graham's "On Lisp", they go much further.
Most importantly the problem of nesting is solved by "letter doubling" and using context appropriate symbols instead of following the `it` tradition as you'll see in examples.

The "quick" macros are simply other general purpose macros which aren't anaphoric.

The library is VERY MUCH WORK IN PROGRESS, things will break. Note that as of right now the symbols are replaced by copying in the expression it references, not by binding to a common variable. Hence not suitable for using with expressions that cause side-effects or involve a lot of computation. That will be changed soon.

If you have suggestions or existing macros that you think fit here please  submit an issue or a PR.
Anything that enables writing succint code and expands to resonable code is fair game. 
Resonable meaning that the resultant code is similar to that which you'd write by hand, runtime penalties should be avoided.

# require
```clojure
;; clojure - in your (ns ...) form
(:require [adas.ana :refer :all])
;; clojure or
(require '[adas.ana :refer :all]])
;; clojurescript - in your (ns ...) form
(:require-macros '[adas.ana :refer [qmap qstr acond aif awhen af aand aor]])
```

# a[naphoric] macros

## acond
```clojure
;; => indicates what the macro expands to roughly
(acond (+ 5 2) %test) ; => (cond (+ 5 2) (+ 5 2))
(acond (+ 5 2) %t) ; => (cond (+ 5 2) (+ 5 2))
(acond (+ 5 2) (acond 9 (+ %t %tt))) ; => (cond (+ 5 2) (cond 9 (+ 9 (+ 5 2))))
```

## aif
```clojure
(aif 9 %test false) ; => (if 9 9 false)
(aif 9 (+ 9 %else) (+ 10 %test)) ; => (if 9 (+ 9 (+ 10 9)) (+ 10 9))
```

## awhen
see [aif](#aif)

## af
```clojure
;; for af the => comment indicates the eval result instead, as the expansion is less obvious
((af [%1 %2]) 10 20) ; => [10 20]
((af [%:lol]) {:lol 20}) ; => [20]
((af [%1 %2:lol]) 10 {:lol 20}) ; => [10 20]

;;self referencing lambda with %self
(require '[clojure.walk :refer [walk]])
(clojure.walk/walk (af
                    (cond
                      (coll? %) (walk %self identity %)
                      (number? %) (inc %))) identity [1 2 3 [4]]) ; => [1 2 3 [5]]

```

## aand 

```clojure
(aand (+ 30 20) *1 ) ; => (and (+ 30 20) *1)
(aand 1 2 "third" (aand 33 **3)) ; => (and 1 2 "third" (aand 33 "third"))
```

## aor
see [aand](#aand)

# q[uick] macros

## qmap
```clojure
(qmap a b c) ; => {:a a :b b :c c}
(qmap a b {:c 10}) ; => {:a a :b b :c 10}
(qmap a b (+ c 20)) ; => {:a a :b b :c (+ c 20)}
```
## qstr
```clojure
(def A 10)
(qstr "console.log(~A)") ; => (str "console.log(" A ")")
;; ~~ means wrap with double quotes
(qstr "console.log(~~A)") ; => (str "console.log(\"" A "\")")
(qstr "console.log(~~(+ 10 10))") ; => (str "console.log(\"" (+ 10 10) "\")")
```
