(ns imo.analysis.core
  (:require [imo.analysis.context :refer [ctx? create-binding]]
            [imo.util :refer [node? node->source start-of]]
            [imo.logger :refer [warn]])
  (:import (imo AnalysisException)))

(defn analysis-ex [{:keys [line col]} msg & rest]
  {:pre [(pos-int? line)
         (pos-int? col)
         (string? msg)]}
  (let [message (apply str (cons msg rest))]
    (AnalysisException. message line col)))

(defn ex->position [^AnalysisException ex]
  {:line (.-line ex)
   :col  (.-col ex)})

(declare analyze-with)

(defmulti default-node-analyzer (fn [_ctx node] (first node)))

(defn default-analyzer [ctx [type & children]]
  (loop [ctx ctx
         [child & rem] children
         analyzed (transient [type])]
    (cond
      (vector? child) (let [[ctx' child'] (analyze-with default-node-analyzer ctx child)]
                        (recur ctx' rem (conj! analyzed child')))
      (string? child) (recur ctx rem (conj! analyzed child))
      :else [ctx (persistent! analyzed)])))

(defmethod default-node-analyzer :default [ctx node]
  (default-analyzer ctx node))

(defn- analyze-meta-nodes [ctx nodes]
  (when (some? nodes)
    (loop [ctx ctx
           [node & xs] nodes
           result (transient [])]
      (if (some? node)
        (case (first node)
          (:space :newline :comment) (recur ctx xs (conj! result node))
          (:meta :discard) (let [[ctx' node'] (analyze-with default-node-analyzer ctx node)]
                             (recur ctx' xs (conj! result node'))))
        [ctx (seq (persistent! result))]))))

(defn- without-nils [node]
  (->> (map #(if (vector? %) (without-nils %) %) node)
       (filterv some?)))

(defn analyze-with [analyzer ctx node]
  {:pre [(ifn? analyzer)
         (ctx? ctx)
         (node? node)]}
  (as-> [ctx (transient (meta node))] state
        (let [[ctx m] state]
          (if-let [[ctx' nodes] (analyze-meta-nodes ctx (:pre m))]
            [ctx' (assoc! m :pre nodes)]
            state))
        (try
          (let [[ctx m] state
                [ctx' node'] (analyzer ctx node)
                _ (assert (ctx? ctx') (str "invalid result context while analyzing ast node: " (node->source node)))
                _ (assert (vector? node') (str "invalid result node while analyzing ast node: " (node->source node)))
                m' (if-some [node-m (meta node')]
                     (reduce-kv assoc! m node-m)
                     m)]
            [ctx' m' node'])
          (catch AnalysisException ex
            (when (= :eval (:mode ctx))
              (warn (ex->position ex) (.getMessage ex)))
            (let [[ctx m] state
                  [ctx' node'] (loop [ctx ctx
                                      [child & xs] (next node)
                                      result (transient [(first node)])]
                                 (if (some? child)
                                   (let [[ctx' child'] (analyze-with default-node-analyzer ctx child)]
                                     (recur ctx' xs (conj! result child')))
                                   [ctx (persistent! result)]))
                  m' (assoc! m :formatting :whitespace)]
              [ctx' m' node'])))
        (let [[ctx m node] state]
          (if-let [[ctx' nodes] (analyze-meta-nodes ctx (:hidden m))]
            [ctx' (assoc! m :hidden nodes) node]
            state))
        (let [[ctx m node] state]
          (if-let [[ctx' nodes] (analyze-meta-nodes ctx (:post m))]
            [ctx' (assoc! m :post nodes) node]
            state))
        (let [[ctx-out m node'] state
              node-out (with-meta node' (persistent! m))]
          (assert (= (without-nils node-out) node)
                  (str "analyzed node does not match with node " (node->source node)
                       "\n analyzed-node: " (pr-str node-out)
                       "\n ast-node:      " (pr-str node)))
          [ctx-out node-out])))

(defn get-node-type [ctx node]
  (first node))

(defn get-literal-content [ctx node]
  (second node))
