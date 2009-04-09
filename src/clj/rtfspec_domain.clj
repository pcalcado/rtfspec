(ns rtfspec-domain
  (:use struct-quack))

(defstruct imperative :kind :description :code)
(defstruct specification :name :imperatives)
(defstruct imperative-result :imperative :status)
(defstruct specification-result :specification :status)

(def *specs* (ref '()))

(defn- consolidate-result [consolidated current]
   (let [result (:status current)]
     (cond
       (= :success result) (if (and (= consolidated :success))
			     :success
			     consolidated)							  
       (= :failure result) (if (not (= consolidated :exception))
			     :failure
			     consolidated)
       :else current)))

(defn- convert [status]
  (cond
    status :success
    (not status) :failure))

(defmulti verify-imperative :kind)

(defmethod verify-imperative :must [imperative]
  (struct-quack imperative-result :imperative imperative :status (convert (eval (:code imperative)))))

(defmethod verify-imperative :must-not [imperative]
  (struct-quack imperative-result  :imperative imperative :status (convert (not (eval (:code imperative))))))

(defn- verify-spec [spec]
  (struct-quack specification-result 
		:specification spec 
		:status (reduce consolidate-result :success 
				(map verify-imperative (:imperatives spec)))))

;; API
(defn add-spec [spec]
  (dosync (alter *specs* conj spec)))

(defn all-specs []
  @*specs*)

(defn verify [specs]
  (map verify-spec specs))

(defn make-imperative [type description code]
  (struct-quack imperative :kind type :description description :code code))

(defn make-spec [name imperatives]
  (struct-quack specification :name name :imperatives imperatives))

(defn verification-status [spec-result-list]
  (reduce consolidate-result :success spec-result-list))