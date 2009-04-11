(ns rtfspec-prettyprint
  (:use rtfspec-domain))

(defmulti pretty-print-verification-results :status)

(defmethod pretty-print-verification-results :success [spec-list]
  (println "yay"))

(defmethod pretty-print-verification-results :failure [spec-list]
  (println "nay"))

(defmethod pretty-print-verification-results :should-success [spec-list]
  (println "Pending [Success]"))

(defmethod pretty-print-verification-results :should-failure [spec-list]
  (println "Pending [Failure]"))

(defmethod pretty-print-verification-results :default [spec-list]
  (println (:status spec-list) "?"))

(defn pretty-print-stats [spec-list]
  (let [all-test-results (all-results-in spec-list)]
    (println (count all-test-results) "Tests:"
	     (count (all-successful-among all-test-results)) "Successful" 
	     (count (all-failed-among all-test-results)) "Failed" 
	     (count (all-pending-among all-test-results)) "Pending" )))