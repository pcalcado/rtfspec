(use 'rtfspec)

(spec "List Comprehensions"
      (must "return a list applying the given function"
	    (= (list 11 12 13 14 15)
	       (for [a (list 1 2 3 4 5)] (+ 10 a))))

      (must "include elements that are not matched by the :while clause"
		(empty?
		   (for [a (list 1 2 3 4 5) :when (= a (+ 10 a))] a))))
