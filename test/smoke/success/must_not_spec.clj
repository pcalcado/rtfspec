(use 'rtfspec)

(spec "2. MUST NOT   This phrase, or the phrase 'SHALL NOT', mean that the
       definition is an absolute prohibition of the specification."
      (must-not "be success if test returns false"
	    (= true false)))