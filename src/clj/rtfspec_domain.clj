(ns rtfspec-domain
  (:use struct-quack))

(defstruct imperative :type :description :code)
(defstruct specification :name :imperatives)

(defn make-imperative [type description code]
  (struct-quack imperative :type type :description description :code code))

(defn make-spec [name imperatives]
  (struct-quack specification :name name :imperatives imperatives))

(def *specs* (ref '()))

(defn add-spec [spec]
  (dosync (alter *specs* conj spec)))

(defn all-specs []
  @*specs*)

