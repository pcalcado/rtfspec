(ns rtfspec-domain
  (:use struct-quack))

(defstruct imperative :kind :description :code)
(defstruct specification :name :imperatives)
(defstruct imperative-result :imperative :status)
(defstruct specification-result :specification :status)
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


(defmulti verify-imperative :kind)

(defn- convert-must-return [status]
  (cond
    status :success
    (not status) :failure))

(defmethod verify-imperative :must [imperative]
  (struct-quack imperative-result :imperative imperative :status (convert-must-return (eval (:code imperative)))))

(defmethod verify-imperative :must-not [imperative]
  (struct-quack imperative-result :imperative imperative :status (convert-must-return (not (eval (:code imperative))))))

(defn- convert-should-return [status]
  (cond
    status :should-success
    (not status) :should-failure))

(defmethod verify-imperative :should [imperative]
  (struct-quack imperative-result :imperative imperative :status (convert-should-return (eval (:code imperative)))))

(defmethod verify-imperative :should-not [imperative]
  (struct-quack imperative-result :imperative imperative :status (convert-should-return (not (eval (:code imperative))))))

(defn- verify-spec [spec]
  (struct-quack specification-result 
		:specification spec 
		:status (reduce consolidate-result :success 
				(map verify-imperative (:imperatives spec)))))

(defn- verification-status [spec-result-list]
  (reduce consolidate-result :success spec-result-list))

;; API
(defn add-spec [spec]
  (dosync (alter *specs* conj spec)))

(defn all-specs []
  @*specs*)

(defn verify [specs]
  (let [specs-results (map verify-spec specs)]
    (struct-quack specification-list-results :specifications specs :results specs-results  :status (verification-status specs-results))))

(defn make-imperative [type description code]
  (struct-quack imperative :kind type :description description :code code))

(defn make-spec [name imperatives]
  (struct-quack specification :name name :imperatives imperatives))

