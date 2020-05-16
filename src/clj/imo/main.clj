(ns imo.main
  (:require [clojure.tools.cli :as cli]
            [clojure.java.io :as io]
            [cognitect.transit :as transit]
            [imo.core :as imo]
            [imo.logger :refer [v vv vvv warn] :as logger]
            [imo.config :as config]
            [clojure.string :as string]
            [clojure.edn :as edn]
            [clojure.spec.alpha :as s])
  (:import (java.io File ByteArrayInputStream Closeable)
           (imo ImoException)
           (java.security MessageDigest)
           (java.util Map))
  (:gen-class))

; For tests
(defonce ^:dynamic *exit-jvm* true)

(defn imo-ex [errors]
  (ImoException. (string/join "\n" errors)))

; CLI options and help

(def ^:private cli-options
  [["-h" "--help" "Show help"]
   ["-c" "--config-file FILE" "Path to configuration file, defaults to imo.edn"]
   [nil "--config-edn EDN" "Overrides to the configuration with EDN string"]
   [nil "--check" "Run IMO in check mode and report unformatted files as failures"]
   ["-v" nil "Increment verbosity level (-v or -vv or -vvv)"
    :id :verbosity
    :default 0
    :update-fn inc]])

(defn- exit [code]
  (if *exit-jvm*
    (do (shutdown-agents)
        (System/exit code))
    code))

(defn print-out [& xs]
  (binding [*out* logger/*debug-out*]
    (doseq [x (filter some? xs)]
      (print x))))

(defn- print-help [opts-summary]
  (println "Usage: imo [options ...] files...     format specified files in-place")
  (println "   or: imo [options ...] -            read contents from stdin and print formatted content to stdout")
  (println "")
  (println "Options:")
  (println opts-summary))

; Config

(defn- parse-config [input]
  (try
    (binding [*read-eval* false]
      (-> (slurp (io/input-stream input))
          (edn/read-string)))
    (catch Exception ex
      (throw (imo-ex ["Config parsing failed: " (.getMessage ex)])))))

(defn- load-config [opts]
  (let [user-config (if-let [c (get opts :config-file)]
                      (let [f (io/file c)]
                        (when-not (and (.exists ^File f) (.isFile ^File f))
                          (throw (imo-ex [(str "Config file does not exist: " c)])))
                        (parse-config f))
                      (let [f (io/file "imo.edn")]
                        (if (and (.exists ^File f) (.isFile ^File f))
                          (parse-config f)
                          {})))
        cli-overrides (if-let [overrides (get opts :config-edn)]
                        (parse-config (ByteArrayInputStream. (.getBytes ^String overrides)))
                        {})]
    (config/merge-config user-config cli-overrides)))

; Caching

(def ^:private ^:const cache-file-version
  (str (or (System/getProperty "imo.version") "dev") "_1"))

; V1 spec
(s/def ::version string?)
(s/def ::checksums (s/map-of string? string?))

(def ^:private generic-cache-file-spec
  (s/keys :req-un [::version]))

(def ^:private current-version-cache-file-spec
  (s/keys :req-un [::version ::checksums]))

(defprotocol ICache
  (cached? [_ source-file ^String contents])
  (cache! [_ source-file ^String contents])
  (write-to-disk! [_]))

(def ^:private noop-cache
  (reify
    ICache
    (cached? [_ _ _] false)
    (cache! [_ _ _] nil)
    Closeable
    (close [_])))

(defn- md5 [^String s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        raw (.digest algorithm (.getBytes s))]
    (format "%032x" (BigInteger. 1 raw))))

(defrecord FileCache [^File cache-file ^Map checksums write-at-close?]
  ICache
  (cached? [_ source-file contents]
    (if (instance? File source-file)
      (let [checksum (md5 contents)
            path (.getAbsolutePath ^File source-file)]
        (= checksum (get @checksums path)))
      false))
  (cache! [_ source-file contents]
    (when (instance? File source-file)
      (let [checksum (md5 contents)
            path (.getAbsolutePath ^File source-file)]
        (swap! checksums assoc path checksum))))
  Closeable
  (close [_]
    (when write-at-close?
      (try
        (vv "writing cache file to disk: " (.getPath cache-file))
        (-> (.getParentFile cache-file)
            (.mkdirs))
        (with-open [os (io/output-stream cache-file)]
          (let [contents {:version cache-file-version :checksums @checksums}
                writer (transit/writer os :json)]
            (transit/write writer contents)))
        (catch Exception ex
          (warn nil "cache file writing failed: " (.getMessage ^Exception ex)))))))

(defn- check-cache-file-contents [spec contents filename]
  (when-not (s/valid? spec contents)
    (throw (imo-ex [(str "Invalid cache file: " filename)]))))

(defn- load-cache [filename write-at-close?]
  (try
    (vv "loading cache file: " filename)
    (let [file (io/file filename)
          contents (when (.exists ^File file)
                     (let [is (io/input-stream file)
                           val (transit/read (transit/reader is :json))]
                       (check-cache-file-contents generic-cache-file-spec val filename)
                       val))]
      (if (= cache-file-version (:version contents))
        (do (check-cache-file-contents current-version-cache-file-spec contents filename)
            (->FileCache file (atom (:checksums contents)) write-at-close?))
        (->FileCache file (atom {}) write-at-close?)))
    (catch ImoException ex
      (throw ex))
    (catch Exception ex
      (throw (imo-ex [(str "Cache file load failure: " (.getMessage ^Exception ex))])))))

(defn- open-cache [config write-at-close?]
  (if-let [cache-filename (:cache config)]
    (load-cache cache-filename write-at-close?)
    noop-cache))

; Input/output

(defn- parse-files-seq [args]
  (if (= ["-"] args)
    [[] true]
    (if-let [errors (->> (map io/file args)
                         (keep #(cond
                                  (not (.exists ^File %)) (str "File does not exist: " %)
                                  (not (.isFile ^File %)) (str "Not a file: " %)
                                  :else nil))
                         (seq))]
      (throw (imo-ex errors))
      [(map io/file args) false])))

(defn- format-files! [config inputs+outputs]
  (let [n-total (count inputs+outputs)
        n-cached (atom 0)
        n-changed (atom 0)
        start-t (System/nanoTime)]
    (with-open [cache (open-cache config true)]
      (doseq [[in out name] inputs+outputs]
        (binding [logger/*current-file* name]
          (v "start formatting file...")
          (let [src-in (slurp in)]
            (if (cached? cache in src-in)
              (do (v "found from cached, skipping")
                  (swap! n-cached inc))
              (let [src-out (imo/format-source config src-in)]
                (when-not (= src-out src-in)
                  (spit out src-out)
                  (cache! cache out src-out)
                  (swap! n-changed inc))
                (v "file formatted")))))))
    (print-out
      (format "Formatting ready, took %.2f secs" (/ (- (System/nanoTime) start-t) 1000000000.0))
      " ✨"
      "\ntotal files: " n-total
      "\n     cached: " @n-cached
      "\n   modified: " @n-changed
      "\n")
    0))

(defn- check-files! [config inputs+outputs]
  (let [n-total (count inputs+outputs)
        n-failed (atom 0)
        n-cached (atom 0)
        start-t (System/nanoTime)]
    (with-open [cache (open-cache config false)]
      (doseq [[in _ name] inputs+outputs]
        (binding [logger/*current-file* name]
          (v "start checking file...")
          (let [src-in (slurp in)]
            (if (cached? cache in src-in)
              (do (v "found from cached, skipping")
                  (swap! n-cached inc))
              (let [src-out (imo/format-source config src-in)
                    failed? (not= src-in src-out)]
                (when failed?
                  (binding [*out* *err*]
                    (println (str "ERROR " name ": check failed")))
                  (swap! n-failed inc))
                (v "file checked, failed? = " failed?)))))))
    (print-out
      (if (pos-int? @n-failed) "\n" "")
      (format "Check ready, took %.2f secs" (/ (- (System/nanoTime) start-t) 1000000000.0))
      " \uD83D\uDD0E"
      "\nchecked files: " n-total
      "\n       cached: " @n-cached
      "\n     failures: " @n-failed
      "\n")
    @n-failed))

; Entrypoint

(defn -main
  "Command line entry point for imo"
  [& args]
  (try
    (let [{:keys [options
                  arguments
                  summary
                  errors]} (cli/parse-opts args cli-options)]
      (cond
        (:help options)
        (print-help summary)

        (seq errors)
        (throw (imo-ex errors))

        :else
        (-> (let [[files stdin?] (parse-files-seq arguments)
                  config (load-config options)
                  inputs+outputs (if-not stdin?
                                   (map #(do [% % (.getName ^File %)]) files)
                                   [[*in* *out* "STDIN"]])
                  log-level (get options :verbosity 0)
                  check-mode? (true? (get options :check))]
              (binding [logger/*debug-out* (if stdin? *err* *out*)
                        logger/*log-level* log-level]
                (vv "using config: " config)
                (if check-mode?
                  (check-files! config inputs+outputs)
                  (format-files! config inputs+outputs))))
            (exit))))
    (catch ImoException ex
      (binding [*out* *err*]
        (println (.getMessage ex)))
      (exit 1))
    (catch Exception ex
      (binding [*out* *err*]
        (println "Unexpected error occurred, please raise an issue at https://github.com/milankinen/imo/issues/new")
        (.printStackTrace ^Throwable ex))
      (exit -1))))

(comment
  (alter-var-root #'*exit-jvm* (constantly false))
  (-main "--check" "foo.clj")

  '-)