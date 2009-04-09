(ns rtfspec
  (:use rtfspec-domain))

(defn an-imperative [type description code]
  (make-imperative type description code))

(defn a-spec [name imperatives]
  (add-spec (make-spec name imperatives)))

(defn spec [name & imperatives]
  (a-spec name imperatives))

(defmacro must [description & impl]
  `(make-imperative :must ~description ~@impl))