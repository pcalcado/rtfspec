(use 'rtfspec)

(spec "Exceptions thrown during tests"
      (must "Mark this test as exception"
	    (throw (Exception. "Expected exception")))

      (must "Fail the build because of exceptions"
	    (throw (Exception. "Build failure ahead")))

      (must "handle null pointer exceptions just like any other"
	    (.toString nil)))