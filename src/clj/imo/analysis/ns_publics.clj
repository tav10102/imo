(ns imo.analysis.ns-publics)

(def clojure-core
  '[clojure.core/primitives-classnames
    clojure.core/+'
    clojure.core/decimal?
    clojure.core/restart-agent
    clojure.core/sort-by
    clojure.core/macroexpand
    clojure.core/ensure
    clojure.core/chunk-first
    clojure.core/eduction
    clojure.core/tree-seq
    clojure.core/unchecked-remainder-int
    clojure.core/seq
    clojure.core/reduce
    clojure.core/when-first
    clojure.core/find-ns
    clojure.core/get-thread-bindings
    clojure.core/contains?
    clojure.core/every?
    clojure.core/proxy-mappings
    clojure.core/keep-indexed
    clojure.core/cond->>
    clojure.core/subs
    clojure.core/ref-min-history
    clojure.core/set
    clojure.core/take-last
    clojure.core/bit-set
    clojure.core/reader-conditional
    clojure.core/gen-class
    clojure.core/qualified-keyword?
    clojure.core/while
    clojure.core/->Eduction
    clojure.core/butlast
    clojure.core/satisfies?
    clojure.core/line-seq
    clojure.core/unchecked-subtract-int
    clojure.core/*print-namespace-maps*
    clojure.core/take-nth
    clojure.core/first
    clojure.core/re-groups
    clojure.core/seq?
    clojure.core/dec'
    clojure.core/ns-unmap
    clojure.core/println-str
    clojure.core/with-bindings*
    clojure.core/inst-ms
    clojure.core/iterator-seq
    clojure.core/iterate
    clojure.core/slurp
    clojure.core/newline
    clojure.core/short-array
    clojure.core/fn?
    clojure.core/doall
    clojure.core/prefers
    clojure.core/enumeration-seq
    clojure.core/dedupe
    clojure.core/dissoc
    clojure.core/atom
    clojure.core/import
    clojure.core/bit-shift-right
    clojure.core/print-method
    clojure.core/peek
    clojure.core/aget
    clojure.core/pvalues
    clojure.core/bound-fn
    clojure.core/vswap!
    clojure.core/last
    clojure.core/pr
    clojure.core/namespace
    clojure.core/push-thread-bindings
    clojure.core/bases
    clojure.core/=
    clojure.core/read+string
    clojure.core/dosync
    clojure.core/remove-ns
    clojure.core/take
    clojure.core/vector?
    clojure.core/thread-bound?
    clojure.core/send-via
    clojure.core/boolean
    clojure.core/bit-shift-left
    clojure.core/any?
    clojure.core/find-var
    clojure.core/rand-int
    clojure.core/aclone
    clojure.core/PrintWriter-on
    clojure.core/vreset!
    clojure.core/chunk
    clojure.core/dec
    clojure.core/future-call
    clojure.core/resultset-seq
    clojure.core/struct
    clojure.core/map
    clojure.core/juxt
    clojure.core/ns-publics
    clojure.core/<
    clojure.core/*source-path*
    clojure.core/with-loading-context
    clojure.core/test
    clojure.core/rest
    clojure.core/ex-data
    clojure.core/compile
    clojure.core/isa?
    clojure.core/boolean?
    clojure.core/..
    clojure.core/munge
    clojure.core/delay
    clojure.core/set-error-mode!
    clojure.core/re-seq
    clojure.core/char?
    clojure.core/make-hierarchy
    clojure.core/set-agent-send-executor!
    clojure.core/swap-vals!
    clojure.core/keep
    clojure.core/char
    clojure.core/mapcat
    clojure.core/unchecked-long
    clojure.core/aset-long
    clojure.core/some?
    clojure.core/unchecked-negate
    clojure.core/remove-tap
    clojure.core/gen-interface
    clojure.core/*command-line-args*
    clojure.core/reverse
    clojure.core/inst?
    clojure.core/range
    clojure.core/sort
    clojure.core/-cache-protocol-fn
    clojure.core/unchecked-inc-int
    clojure.core/map-indexed
    clojure.core/with-bindings
    clojure.core/rand-nth
    clojure.core/comp
    clojure.core/await
    clojure.core/spit
    clojure.core/future-done?
    clojure.core/*read-eval*
    clojure.core/dorun
    clojure.core/simple-symbol?
    clojure.core/disj
    clojure.core/*2
    clojure.core/eval
    clojure.core/cons
    clojure.core/refer
    clojure.core/print-dup
    clojure.core/-reset-methods
    clojure.core/floats
    clojure.core/pos?
    clojure.core/fnil
    clojure.core/merge-with
    clojure.core/nthrest
    clojure.core/load
    clojure.core/if-not
    clojure.core/*verbose-defrecords*
    clojure.core/sequential?
    clojure.core/*print-level*
    clojure.core/shuffle
    clojure.core/boolean-array
    clojure.core/find
    clojure.core/alength
    clojure.core/bit-xor
    clojure.core/deliver
    clojure.core/doseq
    clojure.core/unsigned-bit-shift-right
    clojure.core/neg?
    clojure.core/var-set
    clojure.core/unchecked-float
    clojure.core/pmap
    clojure.core/error-mode
    clojure.core/num
    clojure.core/reduced?
    clojure.core/disj!
    clojure.core/float?
    clojure.core/aset-float
    clojure.core/deftype
    clojure.core/bean
    clojure.core/booleans
    clojure.core/ns-unalias
    clojure.core/when-let
    clojure.core/int-array
    clojure.core/set?
    clojure.core/inc'
    clojure.core/cat
    clojure.core/StackTraceElement->vec
    clojure.core/*suppress-read*
    clojure.core/flush
    clojure.core/take-while
    clojure.core/vary-meta
    clojure.core/<=
    clojure.core/alter
    clojure.core/-'
    clojure.core/if-some
    clojure.core/conj!
    clojure.core/repeatedly
    clojure.core/zipmap
    clojure.core/reset-vals!
    clojure.core/alter-var-root
    clojure.core/biginteger
    clojure.core/remove
    clojure.core/*
    clojure.core/re-pattern
    clojure.core/min
    clojure.core/pop!
    clojure.core/chunk-append
    clojure.core/prn-str
    clojure.core/with-precision
    clojure.core/format
    clojure.core/reversible?
    clojure.core/shutdown-agents
    clojure.core/conj
    clojure.core/bound?
    clojure.core/transduce
    clojure.core/lazy-seq
    clojure.core/*print-length*
    clojure.core/*file*
    clojure.core/compare-and-set!
    clojure.core/*use-context-classloader*
    clojure.core/await1
    clojure.core/let
    clojure.core/ref-set
    clojure.core/pop-thread-bindings
    clojure.core/interleave
    clojure.core/printf
    clojure.core/map?
    clojure.core/->
    clojure.core/defstruct
    clojure.core/*err*
    clojure.core/get
    clojure.core/doto
    clojure.core/identity
    clojure.core/into
    clojure.core/areduce
    clojure.core/long
    clojure.core/double
    clojure.core/volatile?
    clojure.core/definline
    clojure.core/nfirst
    clojure.core/meta
    clojure.core/find-protocol-impl
    clojure.core/bit-and-not
    clojure.core/*default-data-reader-fn*
    clojure.core/var?
    clojure.core/method-sig
    clojure.core/unchecked-add-int
    clojure.core/unquote-splicing
    clojure.core/hash-ordered-coll
    clojure.core/future
    clojure.core/reset-meta!
    clojure.core/cycle
    clojure.core/fn
    clojure.core/seque
    clojure.core/empty?
    clojure.core/short
    clojure.core/definterface
    clojure.core/add-tap
    clojure.core/filterv
    clojure.core/hash
    clojure.core/quot
    clojure.core/ns-aliases
    clojure.core/read
    clojure.core/unchecked-double
    clojure.core/key
    clojure.core/longs
    clojure.core/not=
    clojure.core/string?
    clojure.core/uri?
    clojure.core/aset-double
    clojure.core/unchecked-multiply-int
    clojure.core/chunk-rest
    clojure.core/pcalls
    clojure.core/*allow-unresolved-vars*
    clojure.core/remove-all-methods
    clojure.core/ns-resolve
    clojure.core/as->
    clojure.core/aset-boolean
    clojure.core/trampoline
    clojure.core/double?
    clojure.core/when-not
    clojure.core/*1
    clojure.core/vec
    clojure.core/*print-meta*
    clojure.core/when
    clojure.core/int
    clojure.core/map-entry?
    clojure.core/ns-refers
    clojure.core/rand
    clojure.core/second
    clojure.core/vector-of
    clojure.core/hash-combine
    clojure.core/>
    clojure.core/replace
    clojure.core/int?
    clojure.core/associative?
    clojure.core/unchecked-int
    clojure.core/set-error-handler!
    clojure.core/inst-ms*
    clojure.core/keyword?
    clojure.core/force
    clojure.core/bound-fn*
    clojure.core/namespace-munge
    clojure.core/group-by
    clojure.core/prn
    clojure.core/extend
    clojure.core/unchecked-multiply
    clojure.core/some->>
    clojure.core/default-data-readers
    clojure.core/->VecSeq
    clojure.core/even?
    clojure.core/unchecked-dec
    clojure.core/Inst
    clojure.core/tagged-literal?
    clojure.core/double-array
    clojure.core/in-ns
    clojure.core/create-ns
    clojure.core/re-matcher
    clojure.core/defn
    clojure.core/ref
    clojure.core/bigint
    clojure.core/extends?
    clojure.core/promise
    clojure.core/aset-char
    clojure.core/rseq
    clojure.core/ex-cause
    clojure.core/construct-proxy
    clojure.core/agent-errors
    clojure.core/*compile-files*
    clojure.core/ex-message
    clojure.core/*math-context*
    clojure.core/float
    clojure.core/pr-str
    clojure.core/concat
    clojure.core/aset-short
    clojure.core/set-agent-send-off-executor!
    clojure.core/ns
    clojure.core/symbol
    clojure.core/to-array-2d
    clojure.core/mod
    clojure.core/amap
    clojure.core/pop
    clojure.core/use
    clojure.core/unquote
    clojure.core/declare
    clojure.core/dissoc!
    clojure.core/reductions
    clojure.core/aset-byte
    clojure.core/indexed?
    clojure.core/ref-history-count
    clojure.core/-
    clojure.core/assoc!
    clojure.core/hash-set
    clojure.core/reduce-kv
    clojure.core/or
    clojure.core/cast
    clojure.core/reset!
    clojure.core/name
    clojure.core/ffirst
    clojure.core/sorted-set
    clojure.core/counted?
    clojure.core/byte-array
    clojure.core/tagged-literal
    clojure.core/println
    clojure.core/extend-type
    clojure.core/macroexpand-1
    clojure.core/assoc-in
    clojure.core/char-name-string
    clojure.core/bit-test
    clojure.core/defmethod
    clojure.core/requiring-resolve
    clojure.core/EMPTY-NODE
    clojure.core/time
    clojure.core/memoize
    clojure.core/alter-meta!
    clojure.core/future?
    clojure.core/zero?
    clojure.core/simple-keyword?
    clojure.core/require
    clojure.core/unchecked-dec-int
    clojure.core/persistent!
    clojure.core/nnext
    clojure.core/add-watch
    clojure.core/not-every?
    clojure.core/class?
    clojure.core/rem
    clojure.core/agent-error
    clojure.core/some
    clojure.core/future-cancelled?
    clojure.core/memfn
    clojure.core/neg-int?
    clojure.core/struct-map
    clojure.core/drop
    clojure.core/*data-readers*
    clojure.core/nth
    clojure.core/sorted?
    clojure.core/nil?
    clojure.core/extend-protocol
    clojure.core/split-at
    clojure.core/*e
    clojure.core/load-reader
    clojure.core/random-sample
    clojure.core/cond->
    clojure.core/dotimes
    clojure.core/select-keys
    clojure.core/bit-and
    clojure.core/bounded-count
    clojure.core/update
    clojure.core/list*
    clojure.core/reify
    clojure.core/update-in
    clojure.core/prefer-method
    clojure.core/aset-int
    clojure.core/*clojure-version*
    clojure.core/ensure-reduced
    clojure.core/*'
    clojure.core/instance?
    clojure.core/with-open
    clojure.core/mix-collection-hash
    clojure.core/re-find
    clojure.core/run!
    clojure.core/val
    clojure.core/defonce
    clojure.core/unchecked-add
    clojure.core/loaded-libs
    clojure.core/->Vec
    clojure.core/bytes?
    clojure.core/not
    clojure.core/with-meta
    clojure.core/unreduced
    clojure.core/the-ns
    clojure.core/record?
    clojure.core/type
    clojure.core/identical?
    clojure.core/unchecked-divide-int
    clojure.core/ns-name
    clojure.core/max-key
    clojure.core/*unchecked-math*
    clojure.core/defn-
    clojure.core/*out*
    clojure.core/file-seq
    clojure.core/agent
    clojure.core/ns-map
    clojure.core/set-validator!
    clojure.core/ident?
    clojure.core/defprotocol
    clojure.core/swap!
    clojure.core/vals
    clojure.core/unchecked-subtract
    clojure.core/tap>
    clojure.core/*warn-on-reflection*
    clojure.core/sorted-set-by
    clojure.core/sync
    clojure.core/qualified-ident?
    clojure.core/assert
    clojure.core/*compile-path*
    clojure.core/true?
    clojure.core/release-pending-sends
    clojure.core/print
    clojure.core/empty
    clojure.core/remove-method
    clojure.core/*in*
    clojure.core/print-ctor
    clojure.core/letfn
    clojure.core/volatile!
    clojure.core//
    clojure.core/read-line
    clojure.core/reader-conditional?
    clojure.core/bit-or
    clojure.core/clear-agent-errors
    clojure.core/vector
    clojure.core/proxy-super
    clojure.core/>=
    clojure.core/drop-last
    clojure.core/not-empty
    clojure.core/distinct
    clojure.core/partition
    clojure.core/loop
    clojure.core/add-classpath
    clojure.core/bit-flip
    clojure.core/long-array
    clojure.core/descendants
    clojure.core/merge
    clojure.core/accessor
    clojure.core/integer?
    clojure.core/mapv
    clojure.core/partition-all
    clojure.core/partition-by
    clojure.core/numerator
    clojure.core/object-array
    clojure.core/with-out-str
    clojure.core/condp
    clojure.core/derive
    clojure.core/load-string
    clojure.core/special-symbol?
    clojure.core/ancestors
    clojure.core/subseq
    clojure.core/error-handler
    clojure.core/gensym
    clojure.core/cond
    clojure.core/ratio?
    clojure.core/delay?
    clojure.core/intern
    clojure.core/print-simple
    clojure.core/flatten
    clojure.core/doubles
    clojure.core/halt-when
    clojure.core/with-in-str
    clojure.core/remove-watch
    clojure.core/ex-info
    clojure.core/ifn?
    clojure.core/some->
    clojure.core/nat-int?
    clojure.core/proxy-name
    clojure.core/ns-interns
    clojure.core/all-ns
    clojure.core/find-protocol-method
    clojure.core/subvec
    clojure.core/for
    clojure.core/binding
    clojure.core/partial
    clojure.core/chunked-seq?
    clojure.core/find-keyword
    clojure.core/replicate
    clojure.core/min-key
    clojure.core/reduced
    clojure.core/char-escape-string
    clojure.core/re-matches
    clojure.core/array-map
    clojure.core/unchecked-byte
    clojure.core/with-local-vars
    clojure.core/ns-imports
    clojure.core/send-off
    clojure.core/defmacro
    clojure.core/every-pred
    clojure.core/keys
    clojure.core/rationalize
    clojure.core/load-file
    clojure.core/distinct?
    clojure.core/pos-int?
    clojure.core/extenders
    clojure.core/unchecked-short
    clojure.core/methods
    clojure.core/odd?
    clojure.core/->ArrayChunk
    clojure.core/float-array
    clojure.core/*3
    clojure.core/alias
    clojure.core/frequencies
    clojure.core/read-string
    clojure.core/proxy
    clojure.core/rsubseq
    clojure.core/inc
    clojure.core/get-method
    clojure.core/with-redefs
    clojure.core/uuid?
    clojure.core/bit-clear
    clojure.core/filter
    clojure.core/locking
    clojure.core/list
    clojure.core/+
    clojure.core/split-with
    clojure.core/aset
    clojure.core/->VecNode
    clojure.core/keyword
    clojure.core/*ns*
    clojure.core/destructure
    clojure.core/*assert*
    clojure.core/defmulti
    clojure.core/chars
    clojure.core/str
    clojure.core/next
    clojure.core/hash-map
    clojure.core/if-let
    clojure.core/underive
    clojure.core/ref-max-history
    clojure.core/Throwable->map
    clojure.core/false?
    clojure.core/*print-readably*
    clojure.core/ints
    clojure.core/class
    clojure.core/some-fn
    clojure.core/case
    clojure.core/*flush-on-newline*
    clojure.core/to-array
    clojure.core/bigdec
    clojure.core/list?
    clojure.core/simple-ident?
    clojure.core/bit-not
    clojure.core/io!
    clojure.core/xml-seq
    clojure.core/byte
    clojure.core/max
    clojure.core/==
    clojure.core/*agent*
    clojure.core/lazy-cat
    clojure.core/comment
    clojure.core/parents
    clojure.core/count
    clojure.core/supers
    clojure.core/*fn-loader*
    clojure.core/sorted-map-by
    clojure.core/apply
    clojure.core/interpose
    clojure.core/deref
    clojure.core/assoc
    clojure.core/rational?
    clojure.core/transient
    clojure.core/clojure-version
    clojure.core/chunk-cons
    clojure.core/comparator
    clojure.core/sorted-map
    clojure.core/send
    clojure.core/drop-while
    clojure.core/proxy-call-with-super
    clojure.core/realized?
    clojure.core/char-array
    clojure.core/resolve
    clojure.core/compare
    clojure.core/complement
    clojure.core/*compiler-options*
    clojure.core/*print-dup*
    clojure.core/defrecord
    clojure.core/with-redefs-fn
    clojure.core/sequence
    clojure.core/constantly
    clojure.core/get-proxy-class
    clojure.core/make-array
    clojure.core/shorts
    clojure.core/completing
    clojure.core/update-proxy
    clojure.core/unchecked-negate-int
    clojure.core/hash-unordered-coll
    clojure.core/repeat
    clojure.core/unchecked-inc
    clojure.core/*reader-resolver*
    clojure.core/nthnext
    clojure.core/and
    clojure.core/create-struct
    clojure.core/get-validator
    clojure.core/number?
    clojure.core/await-for
    clojure.core/chunk-next
    clojure.core/print-str
    clojure.core/not-any?
    clojure.core/into-array
    clojure.core/qualified-symbol?
    clojure.core/init-proxy
    clojure.core/chunk-buffer
    clojure.core/seqable?
    clojure.core/symbol?
    clojure.core/when-some
    clojure.core/unchecked-char
    clojure.core/->>
    clojure.core/future-cancel
    clojure.core/var-get
    clojure.core/commute
    clojure.core/coll?
    clojure.core/get-in
    clojure.core/fnext
    clojure.core/denominator
    clojure.core/bytes
    clojure.core/refer-clojure])
