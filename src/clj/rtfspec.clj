(ns rtfspec
  (:use rtfspec-domain))

(defn an-imperative [type description code]
  (make-imperative type description code))

(defn a-spec [name imperatives]
  (add-spec (make-spec name imperatives)))

(defn spec [name & imperatives]
  (a-spec name imperatives))

(defmacro must [description & impl]
  `(make-imperative :must ~description '~@impl))

(defmacro must-not [description & impl]
  `(make-imperative :must-not ~description '~@impl))

(defmacro should [description & impl]
  `(make-imperative :should ~description '~@impl))

(defmacro should-not [description & impl]
  `(make-imperative :should-not ~description '~@impl))