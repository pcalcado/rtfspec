(ns rtfspec-domain
  (:use struct-quack))

(defstruct imperative :type :description :code)
(defstruct specification :name :imperatives)

(defn make-imperative [type description code]
  (struct-quack imperative :type type :description description :code code))

(defn make-spec [name imperatives]
  (struct-quack specification :name name :imperatives imperatives))

(defn add-spec [spec] )