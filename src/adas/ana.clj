;; adas.ana
;; Clojure[Script] productivity macros like you've never seen before
;; Github (including more docs) https://github.com/adasomg/ana
;; ## general principles ##
;; In `acond` `aif`, and `awhen` `%test` or `%t` gets replaced with the test form.
;; `%then` gets replaced by the then form, and `%else` by the else form.
;; If nested you can access the previous level by doubling the first letter of the symbol.
;; For example `%ttest` would get you the previous test form, while `%eeelse` would get you the else form 2 levels up.
;; In the `aand` and `aor` macros you can reference arguments by using a symbol of form `*<num>` where num is the 1-index of the argument.
;; Previous levels are accessed by doubling the `*` character. So the second test form of an `aand` can by accessed with `*2` and the third argument of the previous `aand` would be `**3`

(ns adas.ana
  (:require
   [clojure.walk :as walk :refer [macroexpand-all]]
   [clojure.string :as s]
   [clojure.set :refer [difference intersection]]))

(defmacro double-quote [s]
  `(str "\"" ~s "\""))

(def terminators #{\space \. \/ \' \) \] \; \: \,})
(defmacro qstr [s]
  (let [pos (volatile! 0)
        start (volatile! nil)
        ranges (volatile! [])
        paren-count (volatile! 0)]
    (while  (< @pos (count s))
      (let [char (nth s @pos)]
        (if @start
          (cond
            (and
             (= @pos (dec (count s)))
             (not (contains? terminators char))) (do (vswap! ranges conj [@start (inc @pos)])
                                                     (vreset! start nil))
            (and (contains? terminators char) (= 0 @paren-count)) (do (vswap! ranges conj [@start @pos])
                                                                      (vreset! start nil))
            (and (= char \() paren-count) (vswap! paren-count inc)
            (= char \)) (when (= 0 (vswap! paren-count dec))
                          (vswap! ranges conj [@start (inc @pos)])
                          (vreset! start nil)
                          (vreset! paren-count 0)))
          (when (= char \~)
            (vreset! start @pos)))

        (vswap! pos inc)))
    (if (seq @ranges)
      (->> (for [i (range 0 (count @ranges))
                 :let [[start end] (nth @ranges i)
                       prev (if (= i 0)
                              (.substring s 0 start)
                              (.substring s (second (nth @ranges (dec i))) start))
                       form (.substring s (inc start) end)
                       form (if (s/starts-with? form "~")
                              `(double-quote ~(read-string (.substring form 1 (count form))))
                              (read-string form))]]
             (if (= i (dec (count @ranges)))
               [prev form (.substring s end (count s))]
               [prev form ]))
           (mapcat identity)
           (cons 'str))
      s)))

(defmacro qmap [& keys]
  (reduce #(cond
             (list? %2) (assoc %1 (keyword (name (second %2))) `(~(first %2) ~(second %2) ~@(rest (rest %2))))
             (symbol? %2) (assoc %1 (keyword (name %2)) %2)
             (map? %2) (merge %1 %2)) {}  keys))

(defn core-to-ana [x]
  (case x
    cond 'adas.ana/acond
    if 'adas.ana/aif
    when 'adas.ana/awhen
    and 'adas.ana/aand
    or 'adas.ana/aor
    false))

(defn get-anaphorizes [x]
  (->> (or (core-to-ana x) x) resolve meta ::anaphorizes))

(defn first-symbol-p [x]
  (and (seq? x) (symbol? (first x)) (first x)))
(defn first-symbol= [x sym]
  (= (first-symbol-p x) sym))

(defn get-regex [type {:syms [test else then *] :as d}]
  ;; (pprint d)
  (re-pattern (str (case type 
                     aif (qstr "^\\%((((t){~(+ 1 test)})(est)?)|(((e){~(+ 1 else)})(lse)?)|(((t){~(+ 1 then)})(hen)))")
                     awhen (qstr "^\\%((((t){~(+ 1 test)})(est)?)|(((e){~(+ 1 else)})(lse)?)|(((t){~(+ 1 then)})(hen$)))")
                     acond (qstr "^\\%((((t){~(+ 1 test)})(est)?))")
                     aand (qstr "^\\*{~(+ 1 *)}([0-9])$")
                     aor (qstr "^\\*{~(+ 1 *)}([0-9])$")
                     (throw (ex-info (qstr "Can't generate regex for ~type") {}))) "(!)?$")))



(def anaphorizes '{aif #{test else then}
                   awhen #{test}
                   acond #{test}
                   aand #{*}})

(def empty-ana-depths '{test 0
                        else 0
                        then 0
                        * 0})

(defn walk
  "like clojure.walk/walk but indexed"
  {:added "1.1"}
  [inner outer form]
  (cond
    (list? form) (outer (apply list (map-indexed inner form)))
    (instance? clojure.lang.IMapEntry form) (outer (vec (map-indexed inner form)))
    (seq? form) (outer (doall (map-indexed inner form)))
    (instance? clojure.lang.IRecord form)
    (outer (reduce (fn [r x] (conj r (inner x))) form form))
    (map? form) (outer (into {} (map-indexed #(inner (key (nth (into [] form) %1)) %2) form)))
    (coll? form) (outer (into (empty form) (map-indexed inner form)))
    :else (outer form)))

(defn get-path [coll path]
  (loop [coll coll
         path path]
    (if path
      (let [a (first path)]
        (recur (cond
                 (map? coll) (coll a)
                 (number? a) (nth coll a)
                 )
               (next path)))
      coll)))

(defn bind-sym [x]
  (nth x 1))

(defn bind-val [x]
  (nth x 2))

(defmacro scond
  [& [c :as clauses]]
  (when clauses
    (cond (first-symbol= c 'adas.ana/bind)
          (list 'if-let [(bind-sym c) (bind-val c)]
                (if (next clauses)
                  (second clauses)
                  (throw (IllegalArgumentException.
                          "cond requires an even number of forms")))
                (cons 'adas.ana/scond (next (next clauses))))
          :else
          (list 'if c
                (if (next clauses)
                  (second clauses)
                  (throw (IllegalArgumentException.
                          "cond requires an even number of forms")))
                (cons 'adas.ana/scond (next (next clauses)))))))

(defmacro sand
  ([] true)
  ([x] (cond (symbol? x) x
             (seq? x) (nth x 2)
             :else x))
  ([x & next]
   (cond
     (and (symbol? x) (re-find #"^ANA_BIND_.*" (name x)))
     `(if ~x (sand ~@next) ~x)
     :else
     `(let [~(nth x 1) ~(nth x 2)]
        (if ~(nth x 1) (sand ~@next) ~(nth x 1))))))

(defmacro sif
  ([test & [then else :as branches]]
   (cond
     (= (first-symbol-p test) 'adas.ana/bind)
     `(if-let [~(nth test 1) ~(nth test 2)]
        ~@branches)
     :else `(if ~test ~@branches))))

(defmacro swhen
  ([test & then]
   (cond
     (= (first-symbol-p test) 'adas.ana/bind)
     `(when-let [~(nth test 1) ~(nth test 2)]
        ~@then)
     :else `(when ~test ~@then))))

(defmacro sor
  ([] true)
  ([x] (cond (symbol? x) x
             (seq? x) (nth x 2)
             :else x))
  ([x & next]
   (cond
     (and (symbol? x) (re-find #"^ANA_BIND_.*" (name x)))
     `(if ~x ~x (sand ~@next))
     :else
     `(let [~(nth x 1) ~(nth x 2)]
        (if ~(nth x 1) ~(nth x 1) (sand ~@next))))))

(defn build-next-path [path index x]
  (cond
    (coll? x) (conj path index)))

(defmacro cur-path []
  `(conj ~'path ~'index))

(defmacro acond-clause-index []
  '(dec (if (= depth 0) index top-index)))

(defmacro ensure-recur []
  '(swap! replaces assoc (gensym) "fake"))

(defmacro anaph-test []
  '(if copy
     (swap! replaces assoc g test)
     (if-let [b (@binds [0])]
       (swap! replaces assoc g (if (list? b) (second b) b))
       (do
         (ensure-recur)
         (swap! binds assoc [0] g)))))

(defn get-walker [type replaces binds then-c else-c uniq]
  (letfn [(walker [body top-index path [test then else :as form] depth ana-depths index x]
            ;; (pprint (qmap (deref then-c ) (deref else-c)))
            (let [rexp (get-regex type ana-depths)]
              (cond

                (and @else-c @then-c)
                (throw (ex-info "Cannot use both %then and %else" {}))
                
                (and (= (cur-path) [2])
                     (= @then-c :first))
                (do
                  (reset! then-c :done)
                  (ensure-recur)
                  `(let [~uniq ~then]
                     ~else))

                (and (= (cur-path) [1])
                     (= @else-c :first))
                (do
                  (reset! else-c :done)
                  (ensure-recur)
                  `(let [~uniq ~else]
                     ~then))
                
                (symbol? (@binds (cur-path)))
                (do
                  (ensure-recur)
                  (let [ret `(bind ~(@binds (cur-path)) ~(walk (partial walker body (if (= 0 depth) index top-index) (build-next-path path index x) form (inc depth) ana-depths) identity x))]
                    (swap! binds assoc (cur-path) (list 'already-bound (@binds (cur-path))))
                    ret))
                
                (= (first-symbol-p x) 'adas.ana/bind)
                (cond (coll? (nth x 2))
                      `(bind ~(nth x 1) ~(walk (partial walker body (if (= 0 depth) index top-index) (build-next-path path index (nth x 2)) form (inc depth) ana-depths) identity (nth x 2)))

                      :else x)

                (and (or (= type 'aand)
                         (= type 'aor))  (= (first-symbol-p x) 'adas.ana/nth-form))
                (do
                  (ensure-recur)
                  (@binds (second x))
                  ;; (nth body (second x))
                  )

                (and (= depth 0)
                     (not (= (first-symbol-p x) 'adas.ana/bind))
                     (not (= (first-symbol-p x) 'adas.ana/nth-form))

                     (or (not (symbol? x))
                         (and (not (re-find rexp (name x)))
                              (not (re-find #"^ANA_BIND_.*" (name x)))))
                     (and (or (= type 'aand)
                              (= type 'aor))))
                (let [g (gensym "ANA_BIND_")]
                  (swap! binds assoc index g)
                  (ensure-recur)
                  (list 'adas.ana/bind g (cond (coll? x)
                                               (walk (partial walker body
                                                              (if (= 0 depth) index top-index)
                                                              (build-next-path path index x) form (inc depth) ana-depths) identity x)
                                               :else x)))

                (first-symbol-p x)
                (walk (partial walker body (if (= 0 depth) index top-index) (build-next-path path index x) form (inc depth)
                               (reduce (fn [a b] (update a b #(or (and (number? %) (inc %)) 1))) ana-depths
                                       (->> x first get-anaphorizes (intersection (anaphorizes type))))) identity x)



                (coll? x)
                (walk (partial walker body (if (= 0 depth) index top-index) (build-next-path path index x) form (inc depth) ana-depths) identity x)

                (not (symbol? x)) x

                (re-find rexp (name x))
                (let [[m n _ _ l  :as res] (re-find rexp (name x))
                      copy (last res)
                      g (if copy (gensym "ANA_COPY_") (gensym "ANA_BIND_"))]
                  (and (cond
                         (nil? res) x
                         (or (= type 'aand)
                             (= type 'aor)) (swap! replaces assoc g `(nth-form ~(dec (read-string n))))
                         (or (re-find #"est!?" m)
                             (= l "t")) (if (= type 'acond)
                                          (if copy
                                            (swap! replaces assoc g (nth body (acond-clause-index)))
                                            (do
                                              (ensure-recur)
                                              (swap! binds assoc [(acond-clause-index)] g)))
                                          (anaph-test))
                         (re-find #"hen!?" m) (if copy
                                                (swap! replaces assoc g then)
                                                (do (when-not (or (= :done @then-c)
                                                                  (= :first @then-c))
                                                      (reset! then-c :first))
                                                    (swap! replaces assoc g uniq)))
                         (re-find #"lse!?" m) (if copy
                                                (swap! replaces assoc g else)
                                                (do (when-not (or (= :done @else-c)
                                                                  (= :first @else-c))
                                                      (reset! else-c :first))
                                                    (swap! replaces assoc g uniq)))
                         :else (swap! replaces assoc g else))
                       g))
                :else x)))]
    walker))

(defmacro defanaphora [aname oname]
  `(defmacro ~(with-meta aname `{::anaphorizes (anaphorizes '~aname)}) [& body#]
     (let [replaces# (atom {})
           then-c# (atom nil)
           else-c# (atom nil)
           uniq# (gensym)
           binds# (atom {})
           r# (loop [body-i# body#
                     old-c# nil]
                (let [new-c# (count @replaces#)
                      body-ii# (walk/postwalk-replace @replaces# body-i#)]
                  (if (= old-c# new-c#)
                    body-ii#
                    (recur (walk (partial (get-walker '~aname replaces# binds# then-c# else-c# uniq#) body-ii# 0 [] body-ii# 0 empty-ana-depths) identity body-ii#) new-c#))
                  ))]
       `(~'~oname ~@r#))))

(defanaphora aand adas.ana/sand)
(defanaphora aor adas.ana/sor)
(defanaphora aif adas.ana/sif)
(defanaphora awhen adas.ana/swhen)
(defanaphora acond adas.ana/scond)

(defn no-stop? [x]
  (and (seq? x)
       (not (= (first x) 'af))))

(defmacro af [& body]
  (let [replaces (atom {})
        args [(gensym) (gensym) (gensym) (gensym) :as 'af-args]
        rest (gensym)
        self (gensym)
        af-args (gensym)]
    (letfn [(walker [form x]
                                        ;(pprint (qmap form x))
              (acond
               (no-stop? x) (walk/walk (partial walker form) identity x)
               (= '% x) (args 0)
               (= '%1 x) (args 0)
               (= '%2 x) (args 1)
               (= '%3 x) (args 2)
               (= '%4 x) (args 3)
               (= '%5 x) (args 4)
               (= '%6 x) (args 5)
               (= '%7 x) (args 6)
               (= '%8 x) (args 7)
               (= '%9 x) (args 8)
               (= '%self x) self
               (= '%args x) 'af-args

               (and (symbol? x)
                    (or (and (resolve x)
                             (= (.-ns (resolve x)) (the-ns 'clojure.core)))
                        (= x 'if)))
               (aif (core-to-ana x)
                    (do
                      (ensure-recur)
                      %test)
                    x)

               (and (symbol? x)
                    (re-find #"^%([0-9])?(:.+)?$" (name x))) (let [s (gensym)
                                                                   [_ n key] %test
                                                                   n (if (and n (not (= ":" n))) (read-string n) 1)
                                                                   key (read-string key)]
                                                               (if (keyword? key)
                                                                 (list key (args (dec n)))
                                                                 (list (args (dec n)) key)))
               :else x))]
      (let [r (loop [body body ;; (walk/macroexpand-all body)
                     old-cr nil]
                (let [new-cr (count @replaces)
                      body (walk/postwalk-replace @replaces body)]
                  (if (= old-cr new-cr)
                    body
                    (recur (walk/walk (partial walker body) identity body) new-cr))
                  ))]
        `(fn* ~self ([& ~rest] (let [~args ~rest]
                                 ~@r)))))))


;; WIP
;; (defmacro a->>
;;   [x & forms]
;;   (loop [x x
;;          forms forms
;;          prev nil]
;;     (if forms
;;       (let [threaded
;;             (cond
;;               (and (coll? x) (= 'speciallet-2 (first x)))
;;               (let [ret (last x)
;;                     form (first forms)]
;;                 `(~'speciallet-2 ~@(next (butlast x)) (~@form ~ret)))

;;               (and (coll? x) (= 'speciallet (first x)))
;;               (let [var (first (second x))
;;                     form (first forms)]
;;                 `(~'speciallet-2 ~@(next (butlast x)) (~@form ~var)))

;;               (= '=> (first (next forms)))
;;               (let [var (first (next (next forms)))
;;                     form (first forms)]
;;                 (if (seq? form)
;;                   (with-meta `(~'speciallet [~var (~(first form) ~@(next form)  ~x)] ~var ) (meta form))
;;                   (list form x)))
;;               (= '!! (first forms)) (let [form (first (next forms))]
;;                                       (if (seq? form)
;;                                         `((fn* [arg#] (~@form arg#) arg#) ~@(next (next form)) ~x )
;;                                         `((fn* [arg#] (~form arg#) arg#)  ~x )))
;;               :else (let [form (first forms)]
;;                       (if (seq? form)
;;                         (with-meta `(~(first form) ~@(next form)  ~x) (meta form))
;;                         (list form x))))
;;             nx (cond
;;                  (= '=> (first (next forms))) (next (next (next forms)))
;;                  (= '!! (first forms)) (next (next forms))
;;                  :else (next forms))]
;;         (recur threaded nx (first forms)))
;;       (walk/postwalk-replace '{speciallet clojure.core/let
;;                                speciallet-2 clojure.core/let } x))))
