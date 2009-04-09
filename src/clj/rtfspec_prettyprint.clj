(ns rtfspec-prettyprint
  (:use rtfspec-domain))

(defmulti pretty-print verification-status)

(defmethod pretty-print :success [spec-list]
  (println "yay"))

(defmethod pretty-print :failure [spec-list]
  (println "nay"))

(defmethod pretty-print :default [spec-list]
  (println "?"))