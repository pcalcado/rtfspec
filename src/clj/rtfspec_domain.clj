(ns rtfspec-domain
  (:use struct-quack))

(defstruct imperative :kind :description :code)
(defstruct specification :name :imperatives)
(defstruct imperative-result :imperative :status :extra-info)
(defstruct specification-result :specification :results :status)
(defstruct specification-list-results :specifications :results :status)

(def *specs* (ref '()))

(defn- consolidate-result [consolidated current]
   (let [result (:status current)]
     (cond
       (= :success result) (if (= consolidated :success)
			     :success
			     consolidated)							  
       (= :failure result) (if (not (= consolidated :exception))
			     :failure
			     consolidated)
       (= :should-success result) (if (= consolidated :success)
				    :should-success
				    consolidated)
       (= :should-failure result) (if (= consolidated :success)
				    :should-failure
				    consolidated)
       :else current)))

(defn- convert-must-return [status]
  (cond
    status :success
    (not status) :failure))

(defn- convert-should-return [status]
  (cond
    status :should-success
    (not status) :should-failure))

(defmacro eval-with-callback [imperative callback imperative-execution-code]
  `(try
    (let [execution-result# (eval (list ~@imperative-execution-code))]
      (let [result# 
	    (struct-quack imperative-result
			  :imperative ~imperative
			  :status execution-result#)]
	(~callback result#)
	result#))
    (catch Exception e#
      (let [result# (struct-quack imperative-result
				  :imperative ~imperative
				  :status :exception
				  :extra-info e#)]
	(~callback result#)
	result#)  
      )))

(defmulti verify-imperative (fn find-kind [imperative _] (imperative :kind)))

(defmethod verify-imperative :must [imperative callback]
  (eval-with-callback imperative
		      callback
		      (convert-must-return (eval (:code imperative)))))

(defmethod verify-imperative :must-not [imperative callback]
  (eval-with-callback imperative
		      callback
		      (convert-must-return (not (eval (:code imperative))))))

(defmethod verify-imperative :should [imperative callback]
  (eval-with-callback imperative
		      callback
		      (convert-should-return (eval (:code imperative)))))

(defmethod verify-imperative :should-not [imperative callback]
  (eval-with-callback imperative
		      callback
		      (convert-should-return (not (eval (:code imperative))))))

(defn- verify-spec [spec result-callback]
  (let [imperative-results (for [imperative (:imperatives spec)] (verify-imperative imperative result-callback ))]
    (struct-quack specification-result	
		  :specification spec
		  :results imperative-results
		  :status (reduce 
			   consolidate-result :success 
			     imperative-results))))

(defn- verification-status [spec-result-list]
  (reduce consolidate-result :success spec-result-list))

;; API
(defn add-spec [spec]
  (dosync (alter *specs* conj spec)))

(defn all-specs []
  @*specs*)

(defn verify [specs result-callback]
  (let [specs-results (for [spec specs] (verify-spec spec result-callback))]
    (struct-quack specification-list-results :specifications specs :results specs-results  :status (verification-status specs-results))))

(defn make-imperative [type description code]
  (struct-quack imperative :kind type :description description :code code))

(defn make-spec [name imperatives]
  (struct-quack specification :name name :imperatives imperatives))

(defn all-results-in [spec-list-result]
  (if spec-list-result
    (reduce concat (map :results (:results spec-list-result)))
    '()))

(defn all-successful-among [results]
  (filter #(= :success (:status %)) results))

(defn all-failed-among [results]
  (filter #(= :failure (:status %)) results))

(defn all-exceptions-among [results]
  (filter #(= :exception (:status %)) results))

(defn all-pending-among [results]
  (filter #(or
	    (= :should-failure (:status %))
	    (= :should-success (:status %)))
	    results))