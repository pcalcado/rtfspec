(ns run-rtfspec 
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

(defn- run-loaded-specs [] true)

(load-specs-from (first *command-line-args*))
(run-loaded-specs)