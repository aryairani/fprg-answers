package fp_in_scala

object chapter5 {
  sealed trait Stream[+A] {
    import Stream._

    def headOption_ : Option[A] = this match { 
      case Empty => None
      case Cons(h, t) => Some(h())
    }

    // exercise 5.1
    def foldLeft[B](z: B)(f: (B, A) => B): B = this match {
      case Empty => z
      case Cons(h, t) => t().foldLeft(f(z, h()))(f)
    }
    def reverse: Stream[A] = foldLeft(empty[A])( (l,a) => cons(a,l) )
    def toList: List[A] = reverse.foldLeft[List[A]](Nil)(_.::(_))

    // exercise 5.2
    def take(n: Int): Stream[A] = {
      def loop(i: Int, in: Stream[A], out: Stream[A]): Stream[A] =
      in match {
        case Cons(h, t) if i > 0 => loop(i - 1, t(), cons(h(), out))
        case _ => out
      }
      loop(n, this, empty).reverse
    }
    def drop(n: Int): Stream[A] = this match {
      case Empty => empty
      case Cons(h,t) => if (n > 0) t().drop(n-1) else this
    }

    // exercise 5.3
    def takeWhile53(p: A => Boolean): Stream[A] = {
      def loop(in: Stream[A], out: Stream[A]): Stream[A] =
        in match {
          case Cons(h,t) =>
            val hEval = h()
            if (p(hEval)) loop(t(), cons(hEval, out))
            else out
          case Empty => out
        }

      loop(this, empty).reverse
    }

    // pg 70
//    def exists(p: A => Boolean): Boolean =
//      this match {
//        case Cons(h, t) => p(h()) || t().exists(p)
//        case _ => false
//      }

    // pg 71
    def foldRight[B](z: => B)(f: (A, => B) => B): B =
      this match {
        case Cons(h, t) => f(h(), t().foldRight(z)(f))
        case Empty => z
      }

    def exists(p: A => Boolean): Boolean =
      foldRight(false)((a, b) => p(a) || b)

    // exercise 5.4
    def forAll(p: A => Boolean): Boolean =
      foldRight(true)((a, b) => p(a) && b)

    // exercise 5.5
    def takeWhile(p: A => Boolean): Stream[A] =
      foldRight(empty[A])((a, z) => if (p(a)) cons(a, z) else z.reverse)

    // exercise 5.6
    def headOption: Option[A] =
      foldRight(Option.empty[A])((a, _) => Option(a))

    // exercise 5.7
    def map[B](f: A => B): Stream[B] = foldRight(empty[B])((a, z) => cons(f(a), z))
    def filter(f: A => Boolean): Stream[A] = foldRight(empty[A])((a, z) => if (f(a)) cons(a, z) else z)
    def append[B>:A](that: => Stream[B]): Stream[B] = foldRight(that)((a, z) => cons(a,z))
    def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((a,z) => f(a) append z)

    // exercise 5.14
    def startsWith[B>:A](s: Stream[B]): Boolean =
      zipAll(this, s).foldRight(true) {
        case ((Some(a1), Some(a2)), b) => a1 == a2 && b
        case ((_, None), _) => true
        case _ => false
      }

    // exercise 5.15
    def tails: Stream[Stream[A]] =
      unfold(this) {
        case z @ Cons(h,t) => Some((z,t()))
        case Empty => None
      }

    // exercise 5.16  // no idea
    def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] =
      unfold[B,Stream[A]](this) {
        case Empty => None
        case Cons(h,t) =>
          lazy val b = f(h(), t().foldRight(z)(f))
          Some(b, t())
      } append Stream(z)
  }

  case object Empty extends Stream[Nothing]
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

    def fill[A](n: Int)(a: => A): Stream[A] = if (n <= 0) empty else cons(a, fill(n-1)(a))

    // exercise 5.8
    def constant[A](a: A): Stream[A] = cons(a, constant(a))

    // exercise 5.9
    def from(n: Int): Stream[Int] = cons(n, from(n+1))

    // exercise 5.10
    def fibs: Stream[Int] = {
      def loop(a: Int, b: Int): Stream[Int] = cons(a, loop(b, a+b))
      loop(0,1)
    }

    // exercise 5.11
    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
      f(z) match {
        case None => Stream.empty
        case Some((a,s)) => cons(a, unfold(s)(f))
      }

    // exercise 5.12
    def fibs512: Stream[Int] = unfold((0,1)) {
      case (a,b) => Some(a -> (b,a+b))
    }
    def from512(n: Int): Stream[Int] = unfold(n)(n => Some(n, n+1))
    def constant512[A](a: A): Stream[A] = unfold(a)(a => Some(a,a))
    def ones512: Stream[Int] = constant512(1)

    // exercise 5.13
    def map[A,B](fa: Stream[A])(f: A => B): Stream[B] =
      unfold(fa) {
        case Empty => None
        case Cons(h, t) => Some(f(h()), t())
      }
    def take[A](n: Int)(s: Stream[A]): Stream[A] =
      unfold((n,s)) {
        case (n, Cons(h,t)) if n > 0 => Some(h(), (n-1, t()))
        case _ => None
      }
    def takeWhile[A](s: Stream[A])(f: A => Boolean) =
      unfold(s) {
        case Cons(h,t) =>
          val a = h()
          if (f(a)) Some(a -> t()) else None
        case Empty => None
      }
    def zipWith[A,B,C](as: Stream[A], bs: Stream[B])(f: (A,B) => C): Stream[C] =
      unfold((as,bs)) {
        case (Cons(ha,ta), Cons(hb,tb)) => Some(f(ha(),hb()) -> (ta(),tb()))
        case _ => None
      }
    def zipAll[A,B](as: Stream[A], bs: Stream[B]): Stream[(Option[A], Option[B])] =
      unfold((as,bs)) {
        case (Empty, Empty) => None
        case (Cons(h,t), Empty) => Some((Some(h()), None), (t(), Empty))
        case (Empty, Cons(h,t)) => Some((None, Some(h())), (Empty, t()))
        case (Cons(ha,ta), Cons(hb,tb)) => Some((Some(ha()), Some(hb())), (ta(),tb()))
      }

  }
//
//  def expensive(x: Any) = println(x)
//  val x = Cons(() => expensive(x), tl)
//  val h1 = x.headOption
//  val h2 = x.headOption
}
