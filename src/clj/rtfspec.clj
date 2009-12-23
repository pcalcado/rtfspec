(ns rtfspec
  (:use rtfspec-domain))

(defn an-requirement [type description code]
  (make-requirement type description code))

(defn a-spec [name requirements]
  (add-spec (make-spec name requirements)))

(defn spec [name & requirements]
  (a-spec name requirements))

(defmacro must [description & impl]
  `(make-requirement :must ~description '~@impl))

(defmacro must-not [description & impl]
  `(make-requirement :must-not ~description '~@impl))

(defmacro should [description & impl]
  `(make-requirement :should ~description '~@impl))

(defmacro should-not [description & impl]
  `(make-requirement :should-not ~description '~@impl))