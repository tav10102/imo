(ns imo.analysis.core
  (:refer-clojure :exclude [* + sequence alter])
  (:require [imo.analysis.context :refer [ctx?] :as ctx]
            [imo.analysis.exception :refer [analysis-ex ex-position]]
            [imo.util :refer [node? node->source simple-name-str?]]
            [imo.logger :refer [warn]]
            [clojure.string :as string])
  (:import (imo AnalysisException SpecState)
           (clojure.lang IDeref)))

;;;; analysis core

(declare analyze-node-with default-node-analyzer specify)

(def ^:private node-analyzers (volatile! {}))

(defn- analyze-meta-nodes [ctx nodes]
  (when (some? nodes)
    (loop [ctx ctx
           [node & xs] nodes
           result (transient [])]
      (if (some? node)
        (case (first node)
          (:space :newline :comment) (recur ctx xs (conj! result node))
          (:meta :discard) (let [[ctx' node'] (analyze-node-with default-node-analyzer ctx node)]
                             (recur ctx' xs (conj! result node'))))
        [ctx (seq (persistent! result))]))))

(defn- generic-node-analyzer [ctx node]
  (loop [result (transient [(first node)])
         [child & xs :as children] (next node)
         ctx ctx]
    (if children
      (if (node? child)
        (let [[ctx' child'] (analyze-node-with default-node-analyzer ctx child)]
          (recur (conj! result child') xs ctx'))
        (recur (conj! result child) xs ctx))
      [ctx (persistent! result)])))

(defn default-node-analyzer
  "Analyzer that analyzes the given node by using registered
   default node analysis functions"
  [ctx node]
  {:pre [(node? node)]}
  (let [analyzer (get @node-analyzers (first node) generic-node-analyzer)]
    (analyzer ctx node)))

(defn analyze-node-with
  "Analyzes the given node and its meta nodes using the supplied
   analyzer function and context"
  [analyzer ctx node]
  {:pre [(ifn? analyzer)
         (ctx? ctx)
         (node? node)]}
  (as-> [ctx (transient (meta node))] state+meta+node
        (let [[ctx m] state+meta+node]
          (if-let [[ctx' nodes] (analyze-meta-nodes ctx (:pre m))]
            [ctx' (assoc! m :pre nodes)]
            state+meta+node))
        (try
          (let [[ctx m] state+meta+node
                [ctx' node'] (analyzer ctx node)
                _ (assert (ctx? ctx') (str "invalid result context while analyzing ast node: " (node->source node)))
                _ (assert (vector? node') (str "invalid result node while analyzing ast node: " (node->source node)))
                m' (if-some [node-m (meta node')]
                     (reduce-kv assoc! m node-m)
                     m)]
            [ctx' m' node'])
          (catch AnalysisException ex
            (when (= :eval (:mode ctx))
              (warn (ex-position ex node) (ex-message ex)))
            (let [[ctx m] state+meta+node
                  [ctx' node'] (generic-node-analyzer ctx node)
                  m' (assoc! m :formatting :whitespace)]
              [ctx' m' node'])))
        (let [[ctx m node] state+meta+node]
          (if-let [[ctx' nodes] (analyze-meta-nodes ctx (:hidden m))]
            [ctx' (assoc! m :hidden nodes) node]
            state+meta+node))
        (let [[ctx m node] state+meta+node]
          (if-let [[ctx' nodes] (analyze-meta-nodes ctx (:post m))]
            [ctx' (assoc! m :post nodes) node]
            state+meta+node))
        (let [[ctx-out m node'] state+meta+node
              node-out (with-meta node' (persistent! m))]
          (assert (let [no-nils (fn no-nils [node]
                                  (->> (map #(if (vector? %) (no-nils %) %) node)
                                       (filterv some?)))]
                    (= (no-nils node-out) node))
                  (str "analyzed node does not match with node " (node->source node)
                       "\n analyzed-node: " (pr-str node-out)
                       "\n ast-node:      " (pr-str node)))
          [ctx-out node-out])))

(definterface Spec
  (expectations [])
  (accept [node])
  (advance [state]))

(defn- init-state [ctx node]
  {:pre [(node? node)]}
  (let [[node-type & children] node]
    (SpecState. (transient [node-type]) children ctx)))

(defn- flush-state! [^SpecState state]
  (when-let [extra-node (first (.-remaining state))]
    (throw (analysis-ex "no more forms expected" extra-node)))
  (let [ctx (.-ctx state)
        node (persistent! (.-analyzed state))]
    [ctx node]))

(defn- expectations->message [expectations]
  (if (= 1 (count expectations))
    (str "expected " (first expectations))
    (str "expected one of" (string/join ", " (sort (set expectations))))))

(defn- peek-node [^SpecState state]
  (first (.-remaining state)))

(defn- advance! [^SpecState state]
  (set! (.-remaining state) (next (.-remaining state))))

(defn- push-analyzed! [^SpecState state analyzed]
  (set! (.-analyzed state) (conj! (.-analyzed state) analyzed)))

(defn- get-ctx [^SpecState state]
  (.-ctx state))

(defn- set-ctx! [^SpecState state ctx]
  (set! (.-ctx state) ctx))

(defn- spec-ex [^Spec spec ^SpecState state]
  (let [msg (expectations->message (.expectations spec))
        caused-by (peek-node state)]
    (analysis-ex msg caused-by)))

(defonce ^:private spec-registry
  (volatile! {}))

(defrecord OptionalSpec [^Spec inner]
  Spec
  (expectations [_] nil)
  (accept [this node]
    (or (.accept inner node) this))
  (advance [_ state]
    (push-analyzed! state nil)))

(defn ? [spec]
  (->OptionalSpec (specify spec)))

(defrecord ZeroToManySpec [^Spec inner]
  Spec
  (expectations [_] nil)
  (accept [this _] this)
  (advance [_ state]
    (loop [accepted ^Spec (.accept inner (peek-node state))]
      (when accepted
        (.advance accepted state)
        (recur (.accept inner (peek-node state)))))))

(defn * [spec]
  (->ZeroToManySpec (specify spec)))

(defrecord AtLeastOneSpec [^Spec inner]
  Spec
  (expectations [_] (.expectations inner))
  (accept [this node]
    (when (.accept inner node) this))
  (advance [_ state]
    (loop [spec ^Spec (.accept inner (peek-node state))]
      (when spec
        (.advance spec state)
        (recur (.accept spec (peek-node state)))))))

(defn + [spec]
  (->AtLeastOneSpec (specify spec)))

(defrecord AltSpec [specs]
  Spec
  (expectations [_]
    (mapcat #(.expectations ^Spec %) specs))
  (accept [_ node]
    (loop [[spec & xs] specs]
      (when spec
        (or (.accept ^Spec spec node)
            (recur xs))))))

(defn alt [& specs]
  (let [specs (map specify specs)]
    (case (count specs)
      0 (throw (AssertionError. "at least one spec required"))
      1 (first specs)
      (->AltSpec specs))))

(defrecord SeqSpec [specs]
  Spec
  (expectations [_] (.expectations ^Spec (first specs)))
  (accept [this node]
    (when (.accept ^Spec (first specs) node)
      this))
  (advance [_ state]
    (loop [[spec & xs] specs]
      (if-let [accepted (.accept ^Spec spec (peek-node state))]
        (.advance ^Spec accepted state)
        (throw (spec-ex spec state)))
      (when xs (recur xs)))))

(defn sequence [& specs]
  (let [specs (->> (map specify specs)
                   (mapcat #(if (instance? SeqSpec %) (:specs %) [%])))]
    (case (count specs)
      0 (throw (AssertionError. "at least one spec required"))
      1 (first specs)
      (->SeqSpec specs))))

(defrecord NodeSpec [predicate expecation analyzer]
  Spec
  (expectations [_] [expecation])
  (accept [this node]
    (when (predicate node) this))
  (advance [_ state]
    (let [ctx (get-ctx state)
          node (peek-node state)
          [ctx' node'] (analyze-node-with analyzer ctx node)]
      (advance! state)
      (push-analyzed! state node')
      (set-ctx! state ctx'))))

(defn node [predicate expecation analyzer]
  {:pre [(ifn? predicate)
         (string? expecation)
         (ifn? analyzer)]}
  (->NodeSpec predicate expecation analyzer))

(defn literal [s]
  {:pre [(string? s)]}
  (let [expectation (format "\"%s/\"" s)]
    (node #(= s (second %)) expectation default-node-analyzer)))

(defrecord InScopeSpec [^Spec inner]
  Spec
  (expectations [_] (.expectations inner))
  (accept [this node]
    (when (.accept inner node) this))
  (advance [_ state]
    (set-ctx! state (ctx/push-lexical-scope (get-ctx state)))
    (.advance inner state)
    (set-ctx! state (ctx/pop-lexical-scope (get-ctx state)))))

(defn in-scope [& specs]
  (->InScopeSpec (apply sequence specs)))

(defrecord NamedSpec [^Spec inner ^String expectation]
  Spec
  (expectations [_] [expectation])
  (accept [_ node]
    (.accept inner node)))

(defn named [spec expectation]
  {:pre [(string? expectation)]}
  (->NamedSpec (specify spec) expectation))

(defrecord RecursiveSpec [^IDeref derefable]
  Spec
  (expectations [_] (.expectations ^Spec @derefable))
  (accept [_ node]
    (.accept ^Spec @derefable node)))

(defn recursive [spec]
  {:pre [(or (instance? IDeref spec)
             (keyword? spec))]}
  (if (keyword? spec)
    (->RecursiveSpec (delay (specify spec)))
    (->RecursiveSpec (delay (specify @spec)))))

(defn- specify [x]
  (cond
    (instance? Spec x) x
    (keyword? x) (or (get @spec-registry x) (throw (AssertionError. (str "No spec found: " x))))
    (vector? x) (apply sequence x)
    (string? x) (literal x)
    :else (throw (AssertionError. (str "Invalid spec: " (pr-str x))))))

(defn analyze
  "Analyzes the given spec against supplied context and node and
   returns the analysis result as a tuple of `[ctx node]` or throws
   an `AnalysisException` if analysis fails for some reason."
  [spec ctx node]
  (let [spec ^Spec (specify spec)
        state ^SpecState (init-state ctx node)]
    (when-not (.accept spec (peek-node state))
      (throw (spec-ex spec state)))
    (.advance spec state)
    (flush-state! state)))

;;; core node analyzers

(defn- bound-ns-symbol-node-analyzer [ctx [_ s]]
  {:pre [(simple-name-str? s)]}
  (let [local-name (symbol s)
        fq-name (symbol (:current-ns ctx) s)
        binding (ctx/create-binding local-name fq-name)]
    [(ctx/add-binding ctx binding) [:symbol s]]))

(defn- bound-local-symbol-node-analyzer [ctx [_ s]]
  {:pre [(simple-name-str? s)]}
  (let [local-name (symbol s)
        binding (ctx/create-binding local-name local-name)]
    [(ctx/add-binding ctx binding) [:symbol s]]))

(defn- bound-local-keyword-node-analyzer [ctx [_ s]]
  (let [name-s (if (simple-name-str? s)
                 (subs s 1)
                 (second (string/split s #"/")))
        local-name (symbol name-s)
        binding (ctx/create-binding local-name local-name)]
    [(ctx/add-binding ctx binding) [:keyword s]]))

(defn- literal-node-analyzer [ctx node]
  [ctx (with-meta node nil)])

(defn- coll-node-analyzer [ctx node]
  (loop [result (transient [(first node)])
         [child & xs :as children] (next node)
         ctx ctx]
    (if children
      (let [[ctx' child'] (analyze-node-with default-node-analyzer ctx child)]
        (recur (conj! result child') xs ctx'))
      [ctx (persistent! result)])))

(defn- wrapper-node-analyzer [ctx [type inner]]
  (let [[ctx' inner'] (analyze-node-with default-node-analyzer ctx inner)]
    [ctx' [type inner']]))

(def ^:private form-analyzers
  (volatile! {}))

(defn- list-node-analyzer [ctx [_ i :as node]]
  (if (and (not= :quote (:mode ctx))
           (= :symbol (first i)))
    (let [local-name (symbol (second i))
          invocation (first (ctx/resolve-fq-name ctx local-name))
          form-name (get (:sym-resolution ctx) invocation invocation)
          analyzer (get @form-analyzers form-name generic-node-analyzer)
          [ctx' node'] (analyzer ctx node)]
      [ctx' (vary-meta node' assoc :invocation invocation :resolve-as form-name)])
    (generic-node-analyzer ctx node)))

(defn- body-expr-node-analyzer [ctx node]
  (let [[ctx' node'] (default-node-analyzer ctx node)]
    [ctx' (vary-meta node' assoc :body-expr true)]))

(defn- quote-analyzer [ctx [_ quoted]]
  (let [current-mode (:mode ctx)
        [ctx' quoted'] (analyze-node-with default-node-analyzer (ctx/set-mode ctx :quote) quoted)]
    [(ctx/set-mode ctx' current-mode)
     [:quote quoted']]))

(defn- unquote-analyzer [ctx [type quoted]]
  (let [current-mode (:mode ctx)
        unquoted-ctx (if (= :syntax-quote current-mode)
                       (ctx/set-mode ctx :eval)
                       ctx)
        [ctx' quoted'] (analyze-node-with default-node-analyzer unquoted-ctx quoted)]
    [(ctx/set-mode ctx' current-mode)
     [type quoted']]))

(defn- syntax-quote-analyzer [ctx [_ quoted]]
  (let [current-mode (:mode ctx)
        [ctx' quoted'] (analyze-node-with default-node-analyzer (ctx/set-mode ctx :syntax-quote) quoted)]
    [(ctx/set-mode ctx' current-mode)
     [:syntax-quote quoted']]))

(doseq [lit-type [:symbol :keyword :string :number :boolean :char :regex :nil]]
  (vswap! node-analyzers assoc lit-type literal-node-analyzer))

(doseq [coll-type [:map :set :vector :ns-map :anon-fn]]
  (vswap! node-analyzers assoc coll-type coll-node-analyzer))

(doseq [wrapper-type [:deref :var-quote :meta]]
  (vswap! node-analyzers assoc wrapper-type wrapper-node-analyzer))

(vswap! node-analyzers assoc :quote quote-analyzer)
(vswap! node-analyzers assoc :syntax-quote syntax-quote-analyzer)
(vswap! node-analyzers assoc :unquote unquote-analyzer)
(vswap! node-analyzers assoc :unquote-splice unquote-analyzer)
(vswap! node-analyzers assoc :list list-node-analyzer)

;;;; preset specs

;;; higher order

(defn list-node [spec]
  (let [spec (specify spec)]
    (node #(= :list (first %)) "list" #(analyze spec %1 %2))))

(defn vec-node [spec]
  (let [spec (specify spec)]
    (node #(= :vector (first %)) "vector" #(analyze spec %1 %2))))

(defn map* [spec]
  (let [spec (specify spec)]
    (node #(= :map (first %)) "map" #(analyze spec %1 %2))))

(defn defspec [name spec]
  {:pre [(qualified-keyword? name)]}
  (vswap! spec-registry assoc name (specify spec)))

;;; basic nodes

(defn- simple-symbol-node? [node]
  (and (= :symbol (first node))
       (simple-name-str? (second node))))

(defspec ::any (node some? "any-node" default-node-analyzer))
(defspec ::body-expr (node some? "any-node" body-expr-node-analyzer))
(defspec ::symbol (node #(= :symbol (first %)) "symbol" default-node-analyzer))
(defspec ::simple-symbol (node simple-symbol-node? "simple-symbol" default-node-analyzer))
(defspec ::keyword (node #(= :keyword (first %)) "keyword" literal-node-analyzer))
(defspec ::string (node #(= :string (first %)) "string" literal-node-analyzer))
(defspec ::map (node #(= :map (first %)) "map" default-node-analyzer))
(defspec ::vector (node #(= :vector (first %)) "vector" default-node-analyzer))

;;; destructurable bindings

;; leafs
(defspec ::ns-sym-binding (node simple-symbol-node? "name to bind" bound-ns-symbol-node-analyzer))
(defspec ::local-sym-binding (node simple-symbol-node? "name to bind" bound-local-symbol-node-analyzer))
(defspec ::local-keyword-binding (node #(= :keyword (first %)) "name to bind" bound-local-keyword-node-analyzer))

;; seq
(defspec ::seq-binding.elems (* (alt ["&" (recursive ::any-binding)] (recursive ::any-binding))))
(defspec ::seq-binding (node #(= :vector (first %)) "sequence binding" #(analyze ::seq-binding.elems %1 %2)))

;; map
(defspec ::map-binding.keys (* (alt ::local-sym-binding ::local-keyword-binding)))
(defspec ::map-binding.syms (* ::local-sym-binding))
(defspec ::map-binding.strs (* ::local-sym-binding))
(defspec ::map-binding.elems (* (alt [":keys" (node #(= :vector (first %)) "keys" #(analyze ::map-binding.keys %1 %2))]
                                     [":as" (named ::local-sym-binding "alias")]
                                     [":or" (named ::map "defaults map")]
                                     ; {::foo/keys [lol bal]}
                                     [(node #(and (= :keyword (first %))
                                                  (re-find #"/keys$" (second %)))
                                            "ns-keys"
                                            default-node-analyzer)
                                      (named (vec-node ::map-binding.keys) "keys")]
                                     ; {::foo/syms [lol bal]}
                                     [(node #(and (= :keyword (first %))
                                                  (re-find #"/syms$" (second %)))
                                            "ns-syms"
                                            default-node-analyzer)
                                      (named (vec-node ::map-binding.syms) "syms")]
                                     ; {::foo/strs [lol bal]}
                                     [(node #(and (= :keyword (first %))
                                                  (re-find #"/syms$" (second %)))
                                            "ns-strs"
                                            default-node-analyzer)
                                      (named (vec-node ::map-binding.strs) "strs")]
                                     ; {foo :foo}
                                     [(recursive ::any-binding) ::any])))
(defspec ::map-binding (node #(= :map (first %)) "map binding" #(analyze ::map-binding.elems %1 %2)))

;; any
(defspec ::any-binding (named (alt ::local-sym-binding ::seq-binding ::map-binding) "binding"))

;;; bindings vec

(defspec ::bindings-vec (named (vec-node (* [::any-binding ::any])) "bindings"))

;;;; extensible forms analysis

(defn defform [fq-sym analyzer]
  {:pre [(symbol? fq-sym)]}
  (vswap! form-analyzers assoc fq-sym analyzer))

(def ^:private quote-form-analyzer
  (let [spec (sequence ::symbol ::any)]
    (fn quote-form-analyzer [ctx node]
      (let [current-mode (:mode ctx)
            [ctx' node'] (analyze spec (ctx/set-mode ctx :quote) node)]
        [(ctx/set-mode ctx' current-mode) node']))))

(def ^:private unquote-form-analyzer
  (let [spec (sequence ::symbol ::any)]
    (fn quote-form-analyzer [ctx node]
      (let [current-mode (:mode ctx)
            unquoted-ctx (if (= :syntax-quote current-mode)
                           (ctx/set-mode ctx :eval)
                           ctx)
            [ctx' node'] (analyze spec unquoted-ctx node)]
        [(ctx/set-mode ctx' current-mode) node']))))

(vswap! form-analyzers assoc 'quote quote-form-analyzer)
(vswap! form-analyzers assoc 'unquote unquote-form-analyzer)
(vswap! form-analyzers assoc 'unquote-splicing unquote-form-analyzer)

;;;; entrypoint

(defn analyze-root
  "Analyses the given root node using the given symbol resolution map"
  [symbol-resolution root-node]
  {:pre [(map? symbol-resolution)
         (node? root-node)
         (= :$ (first root-node))]}
  (let [ctx (ctx/create-context symbol-resolution {})]
    (second (analyze-node-with coll-node-analyzer ctx root-node))))
