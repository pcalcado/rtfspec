(use 'rtfspec)

(spec "1. MUST 
       This word, or the terms 'REQUIRED' or 'SHALL', mean that the definition is 
       an absolute requirement of the specification."

      (must "fail if test returns false"
	    (= true false)))