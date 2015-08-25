package fp_in_scala

class chapter5spec extends org.specs2.Specification {
  import fp_in_scala.chapter5._

  def is = s2"""
  chapter 5
    stream tests
      foldLeft    ${ s.foldLeft(0)(_+_) must_== 15 }
      drop        ${ s.drop(3).toList   must_== List(4,5) }
      take        ${ s.take(3).toList   must_== List(1,2,3) }
      takeWhile53   ${ s.takeWhile53(_ < 3).toList must_== List(1,2) }
      takeWhile   ${ s.takeWhile(_ < 3).toList must_== List(1,2) }
      headOption some ${ s.headOption must_== Some(1) }
      headOption none ${ e.headOption must_== None }
      map ${ s.map(_ * 2).toList must_== List(2,4,6,8,10)}
      filter ${ s.filter(_ % 2 == 0).toList must_== List(2,4) }
      append ${ s.append[AnyVal](Stream(())).toList must_== List(1,2,3,4,5,())}
      flatMap ${ s.flatMap(a => Stream.fill(a)(a)).toList must_== List(1, 2,2, 3,3,3, 4,4,4,4, 5,5,5,5,5) }

      fibs ${ Stream.fibs.take(7).toList must_== List(0,1,1,2,3,5,8)}

      startsWith 123 ${ Stream(1,2,3).startsWith(Stream(1,2)) must_== true }
      startsWith 12 ${ Stream(1,2).startsWith(Stream(1,2)) must_== true }
      startsWith 1 ${ Stream(1).startsWith(Stream(1,2)) must_== false }

      tails ${ Stream(1,2,3).tails.map(_.toList).toList must_== List(List(1,2,3), List(2,3), List(3)) }
      scanRight ${ Stream(1,2,3).scanRight(0)(_ + _).toList must_== List(6,5,3,0) }
      """

  private val e = Stream.empty[Int]
  private val s: Stream[Int] = Stream(1, 2, 3, 4, 5)
}

//import org.scalacheck._
//
//object chapter5test extends Properties("chapter2") {
//  import chapter5._
//
//  val smallInteger = Gen.choose(1,100)
//
//  property("Stream.take/drop") = Prop.forAll(smallInteger) { n =>
//    fib(n) == (n match {
//      case 1 => 0
//      case 2 => 1
//      case n => fib(n-1) + fib(n-2)
//    })
//  }
//
//  property("isSorted") = Prop.forAll { (as: Vector[Int]) =>
//    val intLT: (Int,Int) => Boolean = _ <= _
//    isSorted(as, intLT) == (as == as.sortWith(intLT))
//  }
//}
