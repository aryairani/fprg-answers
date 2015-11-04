package fp_in_scala.chapter3

import scala.annotation.tailrec

/**
 * A first class function from List[A] to List[A], for any A.
 * (Not natively supported by Scala's first-class function type.)
 *
 * It's emulated here by having a polymorphic *method* create monomorphic Function1 object
 * at the time apply[A] is called.
 */
trait ListFunc {
  def apply[A]: List[A] ⇒ List[A]
}

object RunTests {
  val init = new ListFunc { def apply[A] = chapter3.init(_) }

  val tail = new ListFunc { def apply[A] = chapter3.tail(_) }

  val reverse = new ListFunc { def apply[A] = stacksafe.reverse(_) }

  def drop(n: Int) = new ListFunc { def apply[A] = chapter3.drop(_, n) }

  def main(args: Array[String]): Unit = {
    test("drop(0)", List(1,2), drop(0))
    test("drop(0)", Nil, drop(0))
    testSuite("tail", tail)
    testSuite("init", init)
    testSuite("drop(2)", drop(2))
  }

  def printResult[A,B](name: String, a: A, b: B): Unit = {
    println(s"$name($a) => $b")
  }

  def test[A](name: String, list: List[A], f: ListFunc): Unit = {
    printResult(name, show(list), show(f.apply(list)))
  }

  def testSuite(name: String, f: ListFunc): Unit = {
    println(s"testing $name on three examples:")
    test(name, List(1,2,3,4), init)
    test(name, List("one","two","three"), init)
    test(name, List('a','b'), init)
  }

  // pretty-print a list
  def show[A](l: List[A]) = {
    @tailrec def loop(l: List[A], z: String): String = l match {
      case Nil ⇒ s"List($z)"
      case Cons(a, as) ⇒ loop(as, s"$z,$a")
    }
    l match {
      case Nil ⇒ "Nil"
      case Cons(a, as) ⇒ loop(as, a.toString)
    }
  }
}

/*
scala> fp_in_scala.polyfunc.RunTests.main
drop(0)(List(1,2)) => List(1,2)
drop(0)(Nil) => Nil
testing tail on three examples:
tail(List(1,2,3,4)) => List(1,2,3)
tail(List(one,two,three)) => List(one,two)
tail(List(a,b)) => List(a)
testing init on three examples:
init(List(1,2,3,4)) => List(1,2,3)
init(List(one,two,three)) => List(one,two)
init(List(a,b)) => List(a)
testing drop(2) on three examples:
drop(2)(List(1,2,3,4)) => List(1,2,3)
drop(2)(List(one,two,three)) => List(one,two)
drop(2)(List(a,b)) => List(a)
 */
