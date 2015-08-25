package fp_in_scala

import org.scalacheck._ // libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.0" % "test"

object chapter2test extends Properties("chapter2") {
	import chapter2._

	val smallInteger = Gen.choose(1,10)

	testFib("fib", fib)
  testFib("fibIter", fibIter)
  testFib("fibIterator", fibIterator)

  testSort("isSorted", isSorted)
  testSort("isSortedIter", isSortedIter)
  testSort("isSortedLib", isSortedLib)

  def testFib(name: String, testFib: Int => Int) =
    property(name) = Prop.forAll(smallInteger) { n =>
      def referenceFib: Int => Int = {
        case 1 => 0
        case 2 => 1
        case n => referenceFib(n-1) + referenceFib(n-2)
      }
      testFib(n) == referenceFib(n)
		}

	def testSort(name: String, isSorted: (Vector[Int], (Int,Int) => Boolean) => Boolean) =
		property(name) = Prop.forAll {
			(as: Vector[Int]) =>
				isSorted(as, _ <= _) == (as == as.sortWith(_ < _)) &&
        isSorted(as.sortWith(_ < _), _ <= _)
		} // a little confusing; method should take lteq, sort takes

}
