package fp_in_scala

import org.scalacheck._

object chapter2test extends Properties("chapter2") {
	import chapter2._

	val smallInteger = Gen.choose(1,100)

  property("fib") = Prop.forAll(smallInteger) { n => 
  	fib(n) == (n match {
  		case 1 => 0
  		case 2 => 1
  		case n => fib(n-1) + fib(n-2)
  	})
  }

  property("isSorted") = Prop.forAll { (as: Vector[Int]) =>
  	val intLT: (Int,Int) => Boolean = _ <= _
  	isSorted(as, intLT) == (as == as.sortWith(intLT))
  }
}
