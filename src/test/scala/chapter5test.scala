package fp_in_scala.chapter5

class chapter5spec extends org.specs2.Specification {

  def is = s2"""
  chapter 5
    stream tests
      foldLeft    ${ s.foldLeft(0)(_+_) must_== 15 }
      drop        ${ s.drop(3).toList   must_== List(4,5) }

      take        ${ s.take(3).toList   must_== List(1,2,3) }
      take2       ${ s.take2(3).toList   must_== List(1,2,3) }

      takeWhile53   ${ s.takeWhile53(_ < 3).toList must_== List(1,2) }
      takeWhile2   ${ s.takeWhile2(_ < 3).toList must_== List(1,2) }
      takeWhile   ${ s.takeWhile(_ < 3).toList must_== List(1,2) }

      headOption some ${ s.headOption must_== Some(1) }
      headOption none ${ e.headOption must_== None }

      map ${ s.map(_ * 2).toList must_== List(2,4,6,8,10)}
      filter ${ s.filter(_ % 2 == 0).toList must_== List(2,4) }
      append ${ s.append[AnyVal](Stream(())).toList must_== List(1,2,3,4,5,())}
      flatMap ${ s.flatMap(a => Stream.fill(a)(a)).toList must_== List(1, 2,2, 3,3,3, 4,4,4,4, 5,5,5,5,5) }

      constant ${ Stream.constant(3).take(3).toList must_== List(3,3,3) }
      from ${ Stream.from(7).take(3).toList must_== List(7,8,9) }
      fibs ${ Stream.fibs.take(7).toList must_== List(0,1,1,2,3,5,8)}

      ones512 ${ Stream.ones512.take(3).toList must_== List(1,1,1) }
      constant512 ${ Stream.constant512(3).take(3).toList must_== List(3,3,3) }
      from512 ${ Stream.from512(7).take(3).toList must_== List(7,8,9) }
      fibs512 ${ Stream.fibs512.take(7).toList must_== List(0,1,1,2,3,5,8)}

      map513        ${ Stream.map(s)(_ * 2).toList must_== List(2,4,6,8,10) }
      take513       ${ Stream.take(3)(s).toList must_== List(1,2,3) }
      takeWhile513  ${ Stream.takeWhile(s)(_ < 3).toList must_== List(1,2) }
      zipWith513(s,s)    ${ Stream.zipWith(s,s)( (a,b) => a+b ).toList must_== List(2,4,6,8,10) }
      zipWith513(s,e)    ${ Stream.zipWith(s,e)( (a,b) => 1 ).toList must_== Nil }
      zipAll513     ${ Stream.zipAll(s,e).toList must_== List(1,2,3,4,5).map(i => (Some(i),None)) }

      startsWith 123 ${ Stream(1,2,3).startsWith(Stream(1,2)) must_== true }
      startsWith 12 ${ Stream(1,2).startsWith(Stream(1,2)) must_== true }
      startsWith 1 ${ Stream(1).startsWith(Stream(1,2)) must_== false }

      Stream().tails ${ Stream().tails.map(_.toList).toList must_== List(List()) }
      Stream(1,2,3).tails ${ Stream(1,2,3).tails.map(_.toList).toList must_== List(List(1,2,3), List(2,3), List(3), List()) }
      Stream(1,2,3).scanRight(0)(_ + _) ${ Stream(1,2,3).scanRight(0)(_ + _).toList must_== List(6,5,3,0) }
      Stream(1,2,3).scanRight(4)(_ + _) ${ Stream(1,2,3).scanRight(4)(_ + _).toList must_== List(10,9,7,4) }
      Stream(1,2,3).scanRight("!")( _.toString + _ ) ${ Stream(1,2,3).scanRight("!")( _.toString + _ ).toList must_== List("123!", "23!", "3!", "!") }

      scanRight2 ${ Stream(1,2,3).scanRight2("!")( _.toString + _ ).toList must_== List("123!", "23!", "3!", "!") }
      """

  private val e = Stream.empty[Int]
  private val s: Stream[Int] = Stream(1, 2, 3, 4, 5)
}

object LazyCheck extends App {
  import org.apache.commons.lang3.mutable.MutableInt

  /** an infinite, recursive stream that counts the number of cells which evaluate the head/car */
  def inf: (Stream[Int], MutableInt) = {
    val evals = new MutableInt(0)
    def loop: Stream[Int] = Stream.cons({ evals.increment(); evals.getValue}, loop)
    (loop, evals)
  }

  /** an infinite, unfolded stream that counts the number of cells that are created/unfolded/cdr */
  def unf: (Stream[Int], MutableInt) = {
    val evals = new MutableInt(0)
    val stream = Stream.unfold(evals) { evals ⇒
      evals.increment()
      Some((evals.getValue.toInt, evals))
    }
    (stream, evals)
  }

  /** apply a function to an "inf" stream and render the number of evaluations */
  def e(f: Stream[Int] ⇒ Any): String = {
    val (s, evals) = inf
    f(s)
    s"$evals evals"
  }

  /** apply a function to an "unf" stream and render the number of unfolds */
  def u(f: Stream[Int] ⇒ Any): String = {
    val (s, evals) = unf
    f(s)
    s"$evals unfolds"
  }
  /** measure "unf" and "inf" */
  def ue(f: Stream[Int] ⇒ Any): String =
    s"${u(f)}, ${e(f)}"

  /** apply a function that passes a lazy arg to a stream and render how many times that arg is evaluated */
  def a[A](a: ⇒ A)(f: (⇒A) ⇒ Stream[A]) = {
    var evals = 0
    f({ evals += 1; a })
    s"$evals argument evals"
  }

  /** apply a function to two "inf" streams and render how many head/car evaluations are performed */
  def e2(f: (Stream[Int],Stream[Int]) ⇒ Any): String = {
    val (s1, evals1) = inf
    val (s2, evals2) = inf

    f(s1,s2)
    s"$evals1 + $evals2 evals"
  }
  /** apply a function to two "unf" streams and render how many unfolds are performed */
  def u2(f: (Stream[Int],Stream[Int]) ⇒ Any): String = {
    val (s1, evals1) = unf
    val (s2, evals2) = unf

    f(s1,s2)
    s"$evals1 + $evals2 unfolds"
  }

  /** measure "unf" and "inf" for functions of two streams */
  def ue2(f: (Stream[Int],Stream[Int]) ⇒ Any): String =
    s"${u2(f)}, ${e2(f)}"

  val N = 100
  val k = 20
  println(
    s"""drop($N)                 ${ ue(_.drop(N)) }
       |take($N)                 ${ ue(_.take(N)) }
       |take2($N)                ${ ue(_.take2(N)) }
       |
       |takeWhile53(_ < $k)       ${ ue(_.takeWhile53(_ < k)) }
       |takeWhile2(_ < $k)        ${ ue(_.takeWhile2(_ < k)) }
       |takeWhile(_ < $k)         ${ ue(_.takeWhile(_ < k)) }
       |
       |headOption                ${ ue(_.headOption) }
       |
       |fill('x')($N)            ${ a('x')(Stream.fill(N)) }
       |
       |map(_ * 2)                ${ ue(_.map(_ * 2)) }
       |filter(_ % 2 == 0)        ${ ue(_.filter(_ % 2 == 0)) }
       |filter(_ < $k)            ${ ue(_.filter(_ < k)) }
       |
       |$N/append/$N            ${ ue2( (a,b) ⇒ a.take2(N) append b.take2(N)) }
       |$N/flatMap(a => Stream.fill(a)(a)) ${ ue(_.take2(N).flatMap(a ⇒ Stream.fill(a)(a))) }
       |
       |zipall(/$N,/$k)          ${ ue2( (a,b) ⇒ Stream.zipAll(a.take2(N), b.take2(k)) ) }
       |zipall(/$k,/$N)          ${ ue2( (a,b) ⇒ Stream.zipAll(a.take2(k), b.take2(N)) ) }
       |
       |$N/startsWith/$k         ${ ue2( (a,b) ⇒ a.take2(N) startsWith b.take2(k)) }
       |$N/startsWith2/$k        ${ ue2( (a,b) ⇒ a.take2(N) startsWith2 b.take2(k)) }
       |
       |tails                     ${ ue(_.tails) }
       |
       |$N/scanRight(0)(_ + _)   ${ ue(_.take2(N).scanRight(0)(_ + _)) }
     """.stripMargin
  )
}
