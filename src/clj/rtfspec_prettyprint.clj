(ns rtfspec-prettyprint
  (:use rtfspec-domain))

;;common print
(def ansi-red     "\033[31m")
(def ansi-green   "\033[32m")
(def ansi-brown   "\033[33m")
(def ansi-default "\033[0m")
(def ansi-purple  "\033[35m")
(def ansi-light-gray "\033[37m")

(defn- wrap-in-color [text color]
  (str color text ansi-default))

(defn- println-success [text]
  (println "\n" (wrap-in-color text ansi-green)))

(defn- println-failure [text]
  (println "\n" (wrap-in-color text ansi-red)))

(defn- println-pending [text]
  (println "\n" (wrap-in-color text ansi-brown)))

(defn- println-unknown [text]
  (println "\n" (wrap-in-color text ansi-purple)))

(defmulti pretty-print-verification-results :status)

(defmethod pretty-print-verification-results :success [spec-list]
  (println-success "yay"))

(defmethod pretty-print-verification-results :failure [spec-list]
  (println-failure "nay"))

(defmethod pretty-print-verification-results :should-success [spec-list]
  (println-pending "Pending [Success]"))

(defmethod pretty-print-verification-results :should-failure [spec-list]
  (println-pending "Pending [Failure]"))

(defmethod pretty-print-verification-results :default [spec-list]
  (println-unknown (str spec-list "?")))

(defn pretty-print-stats [spec-list]
  (let [all-test-results (all-results-in spec-list)]
    (println (count all-test-results) "Tests ("
	     (wrap-in-color (str (count (all-successful-among all-test-results)) " Successful") ansi-green)
	     (wrap-in-color (str (count (all-failed-among all-test-results)) " Failed") ansi-red)
	     (wrap-in-color (str (count (all-pending-among all-test-results)) " Pending" ) ansi-brown) 
	     ")" )))


(defmulti pretty-print-result :status)

(defmethod pretty-print-result :success [_]
  (print (wrap-in-color "." ansi-green)))

(defmethod pretty-print-result :failure [_]
  (print (wrap-in-color "F" ansi-red)))

(defmethod pretty-print-result :should-success [_]
  (print (wrap-in-color "P" ansi-brown)))

(defmethod pretty-print-result :should-failure [_]
  (print (wrap-in-color "P" ansi-brown)))

(defmethod pretty-print-result :default [_]
  (print (wrap-in-color "?" ansi-purple)))
