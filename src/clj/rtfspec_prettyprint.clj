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

(defn- println-exception [text]
  (println "\n" (wrap-in-color text ansi-purple)))

(defn- println-failure [text]
  (println "\n" (wrap-in-color text ansi-red)))

(defn- println-pending [text]
  (println "\n" (wrap-in-color text ansi-brown)))

(defn- println-unknown [text]
  (println "\n" (wrap-in-color text ansi-purple)))

(defn- pretty-formatted-description [imperative]
  (str (.toUpperCase (str (:kind imperative))) " " (:description imperative)))

(defn- pretty-print-failures-in [specification results]
  (doseq [failed-imperative (map :imperative (all-failed-among results))]
    (println-failure
     (str "FAILED:"
	  "\n"
	  (:name specification)
	  " - "
	  (pretty-formatted-description failed-imperative) ":"
	  "\n\t"
	  (:code failed-imperative)))))

(defn- pretty-print-exceptions-in [specification results]
  (doseq [exception-results (all-exceptions-among results)]
    (let [exception-imperative (exception-results :imperative)]
      (println-exception
       (str "EXCEPTION: "
	    "\n"
	    (:name specification)
	    " - "
	    (pretty-formatted-description exception-imperative) ":"
	    "\n\t"
	    (:code exception-imperative)
	    "\n"))
      (.printStackTrace (:extra-info exception-results))
      (flush))))


(defn- pretty-print-pending-in [specification results]
  (doseq [pending-imperative (map :imperative (all-pending-among results))]
    (println-pending 
     (str "Pending: "
	  (:name specification)
	  " - "
	  (pretty-formatted-description pending-imperative)))))

(defn pretty-print-verification-results [spec-list-result]
  (doseq [given-specification-result (:results spec-list-result)]
    (let [specification (:specification given-specification-result)
	  results (:results given-specification-result)]
      (pretty-print-exceptions-in  specification results)
      (pretty-print-failures-in  specification results)
      (pretty-print-pending-in specification results))))

(defn pretty-print-stats [spec-list]
  (let [all-test-results (all-results-in spec-list)]
    (println "\n" (count all-test-results) "Tests ("
	     (wrap-in-color (str (count (all-successful-among all-test-results)) " Successful") ansi-green)
	     (wrap-in-color (str (count (all-failed-among all-test-results)) " Failed") ansi-red)
	     (wrap-in-color (str (count (all-pending-among all-test-results)) " Pending" ) ansi-brown)
	     (wrap-in-color (str (count (all-exceptions-among all-test-results)) " Exceptions" ) ansi-purple) 
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

(defmethod pretty-print-result :exception [_]
  (print (wrap-in-color "E" ansi-purple)))

(defmethod pretty-print-result :default [unknown-result]
  (print (wrap-in-color (str "(" (:status unknown-result) ")?") ansi-purple)))
