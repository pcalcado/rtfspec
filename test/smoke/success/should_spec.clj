(use 'rtfspec)

(spec "3. SHOULD   This word, or the adjective 'RECOMMENDED', mean that there
       may exist valid reasons in particular circumstances to ignore a
       particular item, but the full implications must be understood and
       carefully weighed before choosing a different course."

      (should "Not break the build because a should block failed"
	      false))