(ns run-rtfspec
  (:use rtfspec-domain rtfspec-prettyprint)
  (:import (java.io File)))

(defn- path-from [file] (. file getAbsolutePath))

(defn- is-clojure-source? [file]
  (and (. file isFile)
       (re-find #"_spec\.clj$"  (. file getName))))

(defn- target-type [target-path] (if (. (File. target-path) isDirectory) :dir :file))

(defn- all-source-files-from [dir]
  (filter is-clojure-source? (file-seq dir)))

(defmulti load-specs-from target-type)

(defmethod load-specs-from :file [file] (load-file file) :file)

(defmethod load-specs-from :dir [dir]
  (let [src-files (all-source-files-from (File. dir))]
  (doseq [f src-files]
    (load-file (path-from f)))) :dir)

(defn- translate-status-to-exit-code [status]
  (cond
    (= :success status) 0
    (= :failure status) 1
    (= :should-success status) 0
    (= :should-failure status) 0))

(defn- verify-loaded-specs []
  (let [verification-result (verify (all-specs) pretty-print-result)]
    (pretty-print-verification-results verification-result)
    (pretty-print-stats verification-result)
    (translate-status-to-exit-code (:status verification-result))))

(load-specs-from (first *command-line-args*))

(System/exit (verify-loaded-specs))