package fp_in_scala

import annotation.tailrec

object chapter2 {
	def fib(n: Int): Int = {
		require(n >= 1)
    // Generates a stream of Fibonacci numbers.
    // `n` is the number of Fibonacci numbers to generate.
    // `a1` and `a2` are the next two numbers in the stream.
		@tailrec def fibHelper(n: Int, a1: Int, a2: Int): Int =
      if (n == 1) a1
      else fibHelper(n-1, a2, a1+a2)
		fibHelper(n,0,1)
	}

	def fibIter(_n: Int) = {
    require(_n >= 1)
    var n = _n
    var a = 0
    var b = 1
    while(n > 1) {
      val temp = a
      a = b
      b = temp + a
      n -= 1
    }

    a
  }

  def fibIterator(n: Int): Int = {
    require(n >= 1)
    Iterator.iterate((0,1)) { case (a,b) => (b, a+b) }.map(_._1).drop(n-1).next()
  }

  def isSorted[A](v: Vector[A], ordered: (A,A) => Boolean): Boolean = {
    @tailrec def loop(idx: Int): Boolean =
      if (idx >= v.length - 1) true
      else ordered(v(idx), v(idx+1)) && loop(idx+1)
    loop(0)
  }

  def isSortedIter[A](v: Vector[A], ordered: (A,A) => Boolean): Boolean = {
    var idx = 0
    while (idx < v.length-1) {
      if (!(ordered(v(idx), v(idx+1)))) return false
      idx += 1
    }
    return true
  }

  def isSortedLib[A](v: Vector[A], ordered: (A,A) => Boolean): Boolean = {
    0 to v.length - 2 forall { i => ordered(v(i), v(i+1)) }
  }


  def curry[A,B,C](f: (A,B) => C): A => (B => C) =
		a => f(a,_) // or a => b => f(a,b)

	def uncurry[A,B,C](f: A => B => C): (A, B) => C =
		(a,b) => f(a)(b)


	def compose[A,B,C](f: B => C, g: A => B): A => C =
		a => f(g(a))
}
