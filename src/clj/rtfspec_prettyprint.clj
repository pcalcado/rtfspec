(ns rtfspec-prettyprint
  (:use rtfspec-domain))

(defmulti pretty-print :status)

(defmethod pretty-print :success [spec-list]
  (println "yay"))

(defmethod pretty-print :failure [spec-list]
  (println "nay"))

(defmethod pretty-print :should-success [spec-list]
  (println "Pending [Success]"))

(defmethod pretty-print :should-failure [spec-list]
  (println "Pending [Failure]"))

(defmethod pretty-print :default [spec-list]
  (println (:status spec-list) "?"))