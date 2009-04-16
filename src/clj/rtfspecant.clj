;right now this is a shameless hack
(ns rtfspecant
  (:gen-class
   ; this shouldn't extend that class. it's a hack as i could not get
   ;  setters to work
   :extends org.apache.tools.ant.taskdefs.Java
   :init constructor
   :state state
   :main false
   :exposes-methods {execute executeSuper})
   (:use run-rtfspec)
   (:import (java.io File)))

(defn -constructor [& _]
  [[] (ref "")])

(defn -setDir [this #^File new-value]
  (dosync
   (ref-set (.state this) new-value)))

(defn -execute [ this ]
  (.setClassname this "run_rtfspec")
  (.setValue (.createArg this) (.getAbsolutePath @(.state this)))
  (.executeSuper this))