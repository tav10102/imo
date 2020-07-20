(ns imo.forms.core.ns
  (:require [imo.analysis.spec :as s]
            [imo.util :refer [start-of node?]]
            [imo.logger :refer [warn]]
            [imo.analysis :as a]
            [clojure.string :as string]))

(defn- on-node [f]
  (fn [ctx node]
    [ctx (f node)]))

(def ^:private vec-libspec-spec
  (a/node-spec :vector "libspec"
    (-> (s/seq
          (s/choose
            (a/type= :symbol) (a/simple-symbol-node-spec "lib")
            (a/type= :string) (a/string-node-spec "js-lib"))
          (s/* (s/choose
                 ; :refer :all
                 ; :refer [refers*]
                 (a/val= ":refer")
                 (s/seq (a/keyword-node-spec ":refer")
                        (s/choose
                          (a/val= ":all")
                          (a/keyword-node-spec ":all" (on-node #(vary-meta % assoc :refer :all)))
                          (a/type= :vector)
                          (a/node-spec :vector "refers"
                            (s/* (a/simple-symbol-node-spec "refered-symbol")))))
                 ; :rename {oldname newname}
                 (a/val= ":rename")
                 (s/seq (a/keyword-node-spec ":rename")
                        (a/node-spec :map "renamings"
                          (s/* (s/seq (a/simple-symbol-node-spec "old-name")
                                      (a/simple-symbol-node-spec "new-name")))))
                 ; :as alias
                 (a/val= ":as")
                 (s/seq (a/keyword-node-spec ":as")
                        (a/simple-symbol-node-spec "alias")))))
        (s/as-analyzer
          (fn [ctx [_ lib-node & opt-nodes :as node]]
            (let [l (loop [libspec {:lib  (second lib-node)
                                    :js   (= :string (first lib-node))
                                    :node node}
                           [[k v :as opt] & xs] (partition 2 2 opt-nodes)]
                      (if opt
                        (recur
                          (case (second k)
                            ":refer" (if (= ":all" (second v))
                                       (assoc libspec :refer :all)
                                       (let [refs (mapv (comp symbol second) (next v))]
                                         (assoc libspec :refer refs)))
                            ":rename" (let [renames (-> (next v)
                                                        (map (comp symbol second))
                                                        (partition-all 2)
                                                        (map vec)
                                                        (into {}))]
                                        (assoc libspec :rename renames))
                            ":as" (assoc libspec :alias (symbol (second v))))
                          xs)
                        libspec))]
              [ctx (vary-meta node assoc :libspecs [l])]))))))

(def ^:private sym-libspec-spec
  (a/simple-symbol-node-spec
    "libname"
    (fn [ctx node]
      (let [libspec {:lib  (second node)
                     :js   false
                     :node node}]
        [ctx (vary-meta node assoc :libspecs [libspec])]))))

(def ^:private prefix-list-libspec-spec
  (a/node-spec :list "prefix-list"
    (-> (s/seq (a/simple-symbol-node-spec "prefix")
               (s/+ (s/choose
                      (a/type= :vector) vec-libspec-spec
                      (a/type= :symbol) sym-libspec-spec)))
        (s/as-analyzer
          (fn [ctx [_ prefix-node & libspec-nodes :as node]]
            (let [prefix (second prefix-node)
                  libspecs (->> (mapcat (comp :libspecs meta) libspec-nodes)
                                (mapv (fn [libspec]
                                        (let [lib (str prefix "." (:lib libspec))]
                                          (assoc libspec :lib lib)))))]
              [ctx (vary-meta node assoc :libspecs libspecs)]))))))

(def ^:private flag-libspec-spec
  (a/keyword-node-spec "flag"))


(def ^:private ns-require-spec
  (s/seq (a/keyword-node-spec ":require")
         (s/+ (a/reader-cond-spec
                (s/choose
                  (a/type= :vector) vec-libspec-spec
                  (a/type= :symbol) sym-libspec-spec
                  (a/type= :list) prefix-list-libspec-spec
                  (a/type= :keyword) flag-libspec-spec)))))

(defn- value-as-meta [attr]
  (on-node #(vary-meta % assoc attr (symbol (second %)))))

(defn- find-metas [node attr]
  (if-let [matched-meta (get (meta node) attr)]
    [matched-meta]
    (mapcat #(when (node? %) (find-metas % attr)) (next node))))

(def ^:private ns-refer-clojure-spec
  (s/seq
    (a/keyword-node-spec ":refer-clojure")
    (s/*
      (s/choose
        (a/val= ":only")
        (s/seq
          (a/keyword-node-spec ":only")
          (a/node-spec :vector "includes"
            (s/* (a/simple-symbol-node-spec "require" (value-as-meta :only)))))
        (a/val= ":exclude")
        (s/seq
          (a/keyword-node-spec ":exclude")
          (a/node-spec :vector "excludes"
            (s/* (a/simple-symbol-node-spec "exclude" (value-as-meta :exclude)))))
        (a/val= ":rename")
        (s/seq
          (a/keyword-node-spec ":rename")
          (a/node-spec :map "renames"
            (-> (s/* (s/seq (a/simple-symbol-node-spec "old-name")
                            (a/simple-symbol-node-spec "new-name")))
                (s/as-analyzer
                  (fn [ctx [_ & children :as node]]
                    (let [renames (->> (map (comp symbol second) children)
                                       (partition 2)
                                       (map vec)
                                       (into {}))]
                      [ctx (vary-meta node assoc :rename renames)]))))))))))

(def ^:private import-single-class-spec
  (a/simple-symbol-node-spec
    "class-name"
    (fn [ctx node]
      (let [parts (string/split (second node) #"\.")
            [pkg cls] (if (= 1 (count parts))
                        [nil (first parts)]
                        [(string/join "." (pop parts))
                         (peek parts)])
            import {:package pkg
                    :class   cls}]
        [ctx (vary-meta node assoc :imports [import])]))))

(def ^:private import-many-classes-spec
  (a/node-spec :list "package-list"
    (-> (s/seq (a/simple-symbol-node-spec "package-name")
               (s/+ (a/simple-symbol-node-spec "class-name")))
        (s/as-analyzer
          (fn [ctx [_ pkg-node & class-nodes :as node]]
            (let [pkg (second pkg-node)
                  classes (map second class-nodes)
                  imports (map (fn [c] {:package pkg :class c}) classes)]
              [ctx (vary-meta node assoc :imports imports)]))))))


(def ^:private ns-import-spec
  (s/seq (a/keyword-node-spec ":import")
         (s/* (s/choose
                (a/type= :symbol) import-single-class-spec
                (a/type= :list) import-many-classes-spec
                (a/type= :vector) import-many-classes-spec))))

(def ^:private ns-use-spec
  (s/seq (a/keyword-node-spec ":use")
         (s/custom (fn [parent input]
                     (warn (start-of parent) ":use is not supported (at least for now), use :require instead")
                     input))
         (s/* (a/any-node-spec "libspec"))))

(def ^:private ns-gen-class-spec
  (s/seq (a/keyword-node-spec ":gen-class")
         (s/* (a/any-node-spec "option"))))

(def ^:private ns-load-spec
  (s/seq (a/keyword-node-spec ":load")
         (s/* (a/string-node-spec "lib"))))

(def ^:private ns-refer-spec
  (s/seq (a/keyword-node-spec ":refer")
         (a/simple-symbol-node-spec "lib")
         (s/custom (fn [parent input]
                     (warn (start-of parent) ":refer is not supported (at least for now)")
                     input))
         (s/* (a/any-node-spec "filter"))))

(def ^:private ns-analyzer
  (-> (s/seq (a/symbol-node-spec "ns")
             (a/simple-symbol-node-spec "ns-name" (value-as-meta :ns-name))
             (s/? (a/string-node-spec "doc-string"))
             (s/? (a/map-node-spec "attr-map"))
             (s/* (a/reader-cond-spec
                    (a/node-spec :list
                      "ns-clause"
                      (-> (s/choose
                            (a/val= ":require") ns-require-spec
                            (a/val= ":refer-clojure") ns-refer-clojure-spec
                            (a/val= ":import") ns-import-spec
                            (a/val= ":gen-class") ns-gen-class-spec
                            (a/val= ":use") ns-use-spec
                            (a/val= ":refer") ns-refer-spec
                            (a/val= ":load") ns-load-spec)
                          (s/as-analyzer
                            (fn [ctx [_ clause :as node]]
                              (let [clause (keyword (subs (second clause) 1))
                                    node' (vary-meta node assoc :ns-clause clause)]
                                [ctx node'])))))
                    (fn [ctx node]
                      (when (= :reader-cond-splice (first node))
                        (throw (a/analysis-ex #(start-of node) "imo does not support spliced reader conditional here")))
                      (let [clauses (->> (a/cond-features node)
                                         (keep (comp :ns-clause meta))
                                         (set))]
                        (when (> (count clauses) 1)
                          (throw (a/analysis-ex #(start-of node) "unambiguous conditional")))
                        [ctx (vary-meta node assoc :ns-clause (first clauses))])))))
      (s/as-analyzer
        (fn [ctx [_ _ ns-name-node _ _ & clause-nodes :as node]]
          (let [ns-name-s (second ns-name-node)
                refer-clj-node (first (filter #(= :refer-clojure (:ns-clause (meta %))) clause-nodes))
                clj-exports (as-> (a/get-ns-exports ctx 'clojure.core) exports
                                  (if-let [only (seq (find-metas refer-clj-node :only))]
                                    (filter (set only) exports)
                                    exports)
                                  (if-let [exclude (seq (find-metas refer-clj-node :exclude))]
                                    (remove (set exclude) exports)
                                    exports)
                                  (let [rename (or (first (find-metas refer-clj-node :rename)) {})]
                                    (map #(if-let [renamed (get rename %)]
                                            (a/create-binding renamed (symbol "clojure.core" (name %)))
                                            (a/create-binding % (symbol "clojure.core" (name %))))
                                         exports)))
                imports (->> (filter #(= :import (:ns-clause (meta %))) clause-nodes)
                             (mapcat #(find-metas % :imports))
                             (mapcat identity)
                             (mapcat (fn [{:keys [package class]}]
                                       (let [fq-s (if package
                                                    (str package "." class)
                                                    class)]
                                         [; class name
                                          (a/create-binding (symbol class) (symbol fq-s))
                                          ; constructor
                                          (a/create-binding (symbol (str class ".")) (symbol (str fq-s ".")))]))))
                libspecs (->> (filter #(= :require (:ns-clause (meta %))) clause-nodes)
                              (mapcat #(find-metas % :libspecs))
                              (mapcat identity))
                bindings (-> (mapcat (fn [{:keys [refer rename lib js node]}]
                                       (->> (if (and (= :all refer))
                                              (or (when-not js (a/get-ns-exports ctx (symbol lib)))
                                                  (do (warn (start-of node) "could not find exports for " lib)
                                                      []))
                                              refer)
                                            (map #(let [local-name (get rename % %)
                                                        fq-name (symbol lib (str %))]
                                                    (a/create-binding local-name fq-name)))))
                                     libspecs)
                             (concat clj-exports imports))
                aliases (keep (fn [{:keys [alias js lib]}]
                                (when (and alias (not js))
                                  (a/create-alias alias (symbol lib))))
                              libspecs)
                ctx-in-ns (a/set-ns ctx ns-name-s aliases bindings)]
            [ctx-in-ns node])))))

(a/set-form-analyzer! 'clojure.core/ns ns-analyzer)


(comment

  (-> (repl/ast "(ns foo (:require #?(:clj [clojure.string :refer [join split]]
                                      :cljs [cljs.string :refer [#_split join]])))
                 (join (split 1))")
      (repl/explain))

  (detect-platforms xx)
  (-> (repl/ast*
        (ns imo.forms.core.ns
          (:require [imo.analysis.spec :as s]
                    [imo.util :refer [start-of node?]]
                    [imo.logger :refer [warn]]
                    [imo.analysis :as a]
                    (imo [core :as core]
                         [forms :as forms])
                    imo.main
                    [clojure.string :as string]))
        (start-of nil))
      (repl/explain))

  '-)