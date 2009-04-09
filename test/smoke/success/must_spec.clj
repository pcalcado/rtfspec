(use 'rtfspec)

(spec "1. MUST 
       This word, or the terms 'REQUIRED' or 'SHALL', mean that the definition is 
       an absolute requirement of the specification."

      (must "be success if test returns true"
	    (= true true))

      (must "allow multiple lines"
	    (and
	     (and true
		  (= 1 1)
		  (> 1 0))
	     true)))