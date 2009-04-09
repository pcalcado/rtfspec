(use 'rtfspec)

(spec "4. SHOULD NOT   This phrase, or the phrase 'NOT RECOMMENDED' mean that
       there may exist valid reasons in particular circumstances when the
       particular behavior is acceptable or even useful, but the full
       implications should be understood and the case carefully weighed
       before implementing any behavior described with this label."

      (should-not "break the build because a should block failed"
	      true))