# RTFSpec - A Spec-like Testing Framework for Clojure

A testing framework based on RFC-2119 ( http://www.ietf.org/rfc/rfc2119.txt )

##Example

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
(spec "4. SHOULD NOT   This phrase, or the phrase 'NOT RECOMMENDED' mean that
       there may exist valid reasons in particular circumstances when the
       particular behavior is acceptable or even useful, but the full
       implications should be understood and the case carefully weighed
       before implementing any behavior described with this label."

      (should-not "break the build because a should block failed"
            true))				


## TODO



###For beta

[ ] Documentation :)
[ ] Proper ant task
[ ] Should list failed specs
[ ] Should list exceptions
[ ] Cleanup result feedback function passing - it's a mess
[ ] Nice error msgs for malformed facts
[ ] Some way to expect an exception

### Nice to have

[ ] Autospec
[ ] Code coverage
[ ] Run tests in parallel
[ ] More smoke tests, tests for core.clj would be lovely