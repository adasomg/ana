# adas.ana - Clojure[Script] productivity macros like you've never seen before 
[![Clojars](https://img.shields.io/clojars/v/adas/ana.svg)](https://clojars.org/adas/ana)

adas.ana is a collection of general purpose macros with a focus on writing succint code. 
They are divided into 2 broad categories __"anaphoric"__, and __"quick"__.

While the [anaphoras](https://en.wikipedia.org/wiki/Anaphoric_macro) are inspired by the traditional lisp kind, popularized by Paul Graham's *"On Lisp"*, they go much further.
Most importantly the problem of nesting is solved by "letter doubling" and using context appropriate symbols instead of following the `it` tradition as you'll see in examples.

The "quick" macros are simply other general purpose macros which aren't anaphoric.

All the macros expand to code very similar (or identitcal) to that which you'd write by hand.
This ensures you can use them __without guilt or hesitation__.

The library is __VERY MUCH WORK IN PROGRESS__, things will break and change. Note that as of right now the symbols are replaced by copying in the expression it references, not by binding to a common variable. Hence not suitable for using with expressions that cause side-effects or involve a lot of computation. That will be changed soon.

If you have suggestions or existing macros that you think fit here please  submit an issue or a PR.
Anything that enables succint code and expands to reasonable code is fair game. 

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

## general principles
In `acond` `aif`, and `awhen` `%test` or `%t` gets replaced with the test form.
`%then` gets replaced by the then form, and `%else` by the else form.
If nested you can access the previous level by doubling the first letter of the symbol.
For example `%ttest` would get you the previous test form, while `%eeelse` would get you the else form 2 levels up.
In the `aand` and `aor` macros you can reference arguments by using a symbol of form `*<num>` where num is the 1-index of the argument.
Previous levels are accessed by doubling the `*` character. So the second test form of an `aand` can by accessed with `*2` and the third argument of the previous `aand` would be `**3`

This sounds much harder than it is to use. The examples should be self explanatory. 

## aif
```clojure
(aif 9 %test false) ; => (if 9 9 false)
(aif 9 (+ 9 %else) (+ 10 %test)) ; => (if 9 (+ 9 (+ 10 9)) (+ 10 9))
```
## awhen
see [aif](#aif)

## acond
```clojure
;; => indicates what the macro expands to roughly
(acond (+ 5 2) %test) ; => (cond (+ 5 2) (+ 5 2))
(acond (+ 5 2) %t) ; => (cond (+ 5 2) (+ 5 2))
(acond (+ 5 2) (acond 9 (+ %t %tt))) ; => (cond (+ 5 2) (cond 9 (+ 9 (+ 5 2))))
```

## af
Like clojure's built-in `#(lambda macro)`. 
`%<num>` works the same, but it does nest.
`%self` refers to the function itself. 
Additionally you can do `%:key` which is like doing `(:key %)`.

```clojure
;; for af examples the => comment indicates the eval result instead, as the expansion is less obvious
((af [%1 %2]) 10 20) ; => [10 20]
((af [%:lol]) {:lol 20}) ; => [20]
((af [%1 %2:lol]) 10 {:lol 20}) ; => [10 20]

;;nesting works
((af (+ %
        ((af %) %2))) 1 2) ; => 3

;;self referencing lambda with %self
(require '[clojure.walk :refer [walk]])
(walk (af
       (cond
         (coll? %) (walk %self identity %)
         (number? %) (inc %))) identity [1 2 3 [4]]) ; => [1 2 3 [5]]
```

## aand 

```clojure
(aand (+ 30 20) *1 ) ; => (and (+ 30 20) (+ 30 20))
(aand 1 2 "third" (aand 33 **3)) ; => (and 1 2 "third" (and 33 "third"))
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
In clojure usually you'll use `str` to build simple strings. It makes for nasty looking code. 
Especially when you need to escape double quotes. We can use `format` but it's usually overkill.
`qstr` like javascript's template strings lets you easily insert clojure values into strings.
Being a macro, you don't suffer a runtime penalty vs. using `str`.
```clojure
;; ~ is like unquote in a syntax-quoted form
(qstr "console.log(~A)") ; => (str "console.log(" A ")")
;; ~~ wraps with double quotes
(qstr "console.log(~~A)") ; => (str "console.log(\"" A "\")")
(qstr "console.log(~~(+ 10 10))") ; => (str "console.log(\"" (+ 10 10) "\")")
```
