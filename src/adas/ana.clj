(ns adas.ana
  (:require
   [clojure.walk :as walk]
   [clojure.string :as s]
   [clojure.set :refer [difference]]))

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

(def aif-anaphorizes #{'%test '%else '%then})
(defmacro ^{::anaphorizes aif-anaphorizes} aif [& body]
  (let [replaces (atom {})]
    (letfn [(walker [[test then else :as form] depth x]
              (cond
                (and (coll? x) (list? x) (->> x first resolve meta ::anaphorizes (difference aif-anaphorizes) not-empty))
                (walk/walk (partial walker form depth) identity x)

                (coll? x) (walk/walk (partial walker form (inc depth)) identity x)

                (not (symbol? x)) x

                (re-find (re-pattern (qstr "^\\%[te]{~(+ 1 depth)}(est|hen|lse)?")) (name x))
                (let [g (gensym)
                      [_ m] (re-find (re-pattern (qstr "^\\%[te]{~(+ 1 depth)}(est|hen|lse)?")) (name x))]
                  (println m)
                  (case m
                    "est" (swap! replaces assoc g test)
                    nil (swap! replaces assoc g test)
                    "hen" (swap! replaces assoc g then)
                    "lse" (swap! replaces assoc g else))
                  g)
                
                :else x))]
      (let [r (loop [body body
                     old-c nil]
                (let [new-c (count @replaces)
                      body (walk/postwalk-replace @replaces body)]
                  (if (= old-c new-c)
                    body
                    (recur (walk/walk (partial walker body 0) identity body) new-c))
                  ))]
        `(if ~@r)))))

(def awhen-anaphorizes #{'%test '%then})
(defmacro ^{::anaphorizes awhen-anaphorizes} awhen [& body]
  (let [replaces (atom {})]
    (letfn [(walker [[test then else :as form] depth x]
              (cond
                (and (coll? x) (list? x) (->> x first resolve meta ::anaphorizes (difference awhen-anaphorizes) not-empty))
                (walk/walk (partial walker form depth) identity x)

                (coll? x) (walk/walk (partial walker form (inc depth)) identity x)

                (not (symbol? x)) x

                (re-find (re-pattern (qstr "^\\%[te]{~(+ 1 depth)}(est|hen)?")) (name x))
                (let [g (gensym)
                      [_ m] (re-find (re-pattern (qstr "^\\%[te]{~(+ 1 depth)}(est|hen)?")) (name x))]
                  
                  (case m
                    "est" (swap! replaces assoc g test)
                    nil (swap! replaces assoc g test)
                    "hen" (swap! replaces assoc g then))
                  g)
                
                :else x))]
      (let [r (loop [body body
                     old-c nil]
                (let [new-c (count @replaces)
                      body (walk/postwalk-replace @replaces body)]
                  (if (= old-c new-c)
                    body
                    (recur (walk/walk (partial walker body 0) identity body) new-c))
                  ))]
        `(when ~@r)))))

(def acond-anaphorizes #{'%test})
(defmacro ^{::anaphorizes acond-anaphorizes} acond [& body]
  (let [replaces (atom {})]
    (letfn [(walker [form [test then :as cond-form] depth ana-depth x]
              (println (qmap depth ana-depth x form cond-form))
              (cond
                (and (coll? x) (list? x) (->> x first resolve meta ::anaphorizes (difference acond-anaphorizes) empty?))
                (walk/walk (partial walker form (if (= depth 0)
                                                  x
                                                  cond-form) (inc depth) (inc ana-depth)) identity x)
                
                (coll? x) (walk/walk (partial walker form (if (= depth 0)
                                                            x
                                                            cond-form) (inc depth) ana-depth) identity x)

                (not (symbol? x)) x

                (re-find (re-pattern (qstr "^\\%[te]{~(+ 1 ana-depth)}(est)")) (name x))
                (let [g (gensym)
                      [_ m] (re-find (re-pattern (qstr "^\\%[te]{~(+ 1 ana-depth)}(est)")) (name x))]
                  (case m
                    "est" (swap! replaces assoc g test))
                  g)
                :else x))]
      (let [r (loop [body (partition 2 body)
                     old-c nil]
                (let [new-c (count @replaces)
                      body (walk/postwalk-replace @replaces body)]
                  (if (= old-c new-c)
                    body
                    (recur (walk/walk (partial walker body nil 0 0) identity body) new-c))
                  ))
            r (apply concat r)]
        `(cond ~@r)))))

(def aand-anaphorizes #{'*})
(defmacro ^{::anaphorizes aand-anaphorizes} aand [& body]
  (let [replaces (atom {})
        shadow-form (atom {})]
    (letfn [(walker [[a b c d e f g :as form] nth-pass depth ana-depth x]
              (let [sym (when (symbol? x) (name x))
                    g (gensym)]
                (cond

                  (and (coll? x) (list? x) (not (->> x first meta ::anaphorizes (difference aand-anaphorizes) not-empty)))
                  (walk/walk (partial walker form nth-pass (inc depth) ana-depth) identity x)

                  (coll? x) (walk/walk (partial walker form nth-pass (inc depth) (inc ana-depth)) identity x)


                  (not (symbol? x)) x

                  (re-find (re-pattern (qstr "^\\*{~(+ 1 depth)}([0-9])")) sym) (let [n (dec (read-string (second (re-find #"\*([0-9]{1,2})" sym))))]
                                                                                  (swap! replaces assoc g (nth form n))
                                                                                  g)
                  :else x
                  )))]
      (let [r (loop [body body
                     old-cr nil
                     nth-pass 0]
                (let [new-cr (count @replaces)
                      body (walk/postwalk-replace @replaces body)]
                  (if (= old-cr new-cr)
                    body
                    (recur (walk/walk (partial walker body nth-pass 0 0) identity body) new-cr (inc nth-pass)))
                  ))]
        `(and ~@r)))))

(defmacro aor [& body]
  (conj (rest (macroexpand-1 `(aand ~@body))) 'or))

(defn no-stop? [x]
  (and (coll? x)
       (not (= (first x) 'af))))

(defmacro af [& body]
  (let [replaces (atom {})
        args [(gensym) (gensym) (gensym) (gensym)]
        rest (gensym)
        self (gensym)]
    (letfn [(walker [form x]
              (cond
                (no-stop? x) (walk/walk (partial walker form) identity x)
                (= '% x) (args 0)
                (= '%1 x) (args 0)
                (= '%2 x) (args 1)
                (= '%3 x) (args 2)
                (= '%self x) self
                
                (and (symbol? x)
                     
                     (re-find #"^%([0-9])?(.+)" (name x))) (let [s (gensym)
                                                                 [_ num key] (re-find #"^%([0-9])?(.+)" (name x))
                                                                 num (if num (read-string num) 1)
                                                                 key (read-string key)]
                                                             (if (keyword? key)
                                                               (list key (args (dec num)))
                                                               (list (args (dec num)) key)))
                :else x))]
      (let [r (loop [body (walk/macroexpand-all body)
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

(comment
  (awhen 100 (+ 20 ( 999999999 %ttest)))
  (let [a 10
        b 9
        c "lol"]
    (qmap a b (str c "hehe") {:d (qstr "console.log(~~c)")}))
  
  ((af (aand 30 [%:lol %:omg])) {:lol 20 :omg 99})
  (aand 9 43 (aand **1 **2 (+ **1 **1))))
