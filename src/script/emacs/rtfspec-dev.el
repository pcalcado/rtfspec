(setq swank-clojure-classpath
      (append
       swank-clojure-classpath
       (list "/Users/pcalcado/code/foss/clojure/rtfspec/src/clj")
       (directory-files "/Users/pcalcado/Documents/code/foss/clojure/rtfspec/lib" t ".jar$")
       (directory-files "/Users/pcalcado/Documents/code/foss/clojure/rtfspec/deps" t ".jar$")))
