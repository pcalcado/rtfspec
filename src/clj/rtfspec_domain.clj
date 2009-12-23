(ns rtfspec-domain
  (:use struct-quack))

(defstruct requirement :kind :description :code)
(defstruct specification :name :requirements)
(defstruct requirement-result :requirement :status :extra-info)
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

(defmacro eval-with-callback [requirement callback requirement-execution-code]
  `(try
    (let [execution-result# (eval (list ~@requirement-execution-code))]
      (let [result# 
	    (struct-quack requirement-result
			  :requirement ~requirement
			  :status execution-result#)]
	(~callback result#)
	result#))
    (catch Exception e#
      (let [result# (struct-quack requirement-result
				  :requirement ~requirement
				  :status :exception
				  :extra-info e#)]
	(~callback result#)
	result#)  
      )))

(defmulti verify-requirement (fn find-kind [requirement _] (requirement :kind)))

(defmethod verify-requirement :must [requirement callback]
  (eval-with-callback requirement
		      callback
		      (convert-must-return (eval (:code requirement)))))

(defmethod verify-requirement :must-not [requirement callback]
  (eval-with-callback requirement
		      callback
		      (convert-must-return (not (eval (:code requirement))))))

(defmethod verify-requirement :should [requirement callback]
  (eval-with-callback requirement
		      callback
		      (convert-should-return (eval (:code requirement)))))

(defmethod verify-requirement :should-not [requirement callback]
  (eval-with-callback requirement
		      callback
		      (convert-should-return (not (eval (:code requirement))))))

(defn- verify-spec [spec result-callback]
  (let [requirement-results (for [requirement (:requirements spec)] (verify-requirement requirement result-callback ))]
    (struct-quack specification-result	
		  :specification spec
		  :results requirement-results
		  :status (reduce 
			   consolidate-result :success 
			     requirement-results))))

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

(defn make-requirement [type description code]
  (struct-quack requirement :kind type :description description :code code))

(defn make-spec [name requirements]
  (struct-quack specification :name name :requirements requirements))

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