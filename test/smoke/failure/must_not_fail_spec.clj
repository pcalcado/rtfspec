(use 'rtfspec)

(spec "2. MUST NOT   This phrase, or the phrase 'SHALL NOT', mean that the
       definition is an absolute prohibition of the specification."

      (must-not "avoid failure if false"
	    (= true true))

      (must-not "have trouble when ailing in multiple  lines"
		(or
		 false
		 true
		 false)))