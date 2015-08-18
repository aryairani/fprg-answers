package fp_in_scala

import annotation.tailrec

object chapter2 {
	def fib(n: Int): Int = {
		require(n >= 1)
		@tailrec def loop(x: Int, a1: Int, a2: Int): Int = x match {
			case 1 => a1
			case n => loop(n-1, a2, a2+a1)
		}
		loop(n,0,1)
	}

	def isSorted[A](as: Vector[A], ordered: (A,A) => Boolean): Boolean = {
		@tailrec def loop(idx: Int): Boolean = {
			if (idx >= 0)
			  ordered(as(idx), as(idx+1)) && loop(idx-1)
		  else true
		}
		loop(as.length-2)
	}

	def curry[A,B,C](f: (A,B) => C): A => (B => C) =
		a => f(a,_) // or a => b => f(a,b)

	def uncurry[A,B,C](f: A => B => C): (A, B) => C =
		(a,b) => f(a)(b)


	def compose[A,B,C](f: B => C, g: A => B): A => C =
		a => f(g(a))
}
