package fp_in_scala

import annotation.tailrec

object chapter3 {

  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]
  object List {
    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x,xs) => x + sum(xs)
    }
    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x,xs) => x * product(xs)
    }
    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
  }

  val ex1: List[Double] = Nil
  val ex2: List[Int] = Cons(1, Nil)
  val ex3: List[String] = Cons("a", Cons("b", Nil))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + List.sum(t)
    case _ => 101
  }

  assert(x == 3)

  // exercise 3.2
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }

  // exercise 3.3
  def setHead[A](l: List[A], head: A): List[A] = 
    Cons(head, tail(l))

  def drop[A](l: List[A], n: Int): List[A] =
    if (n > 0) drop(tail(l), n-1) else l

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => if (f(x)) dropWhile(xs, f) else l
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B): B = 
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)

  // exercise 3.7:  No, foldRight can't short-circuit; f can short-circuit a little
  // exercise 3.8:  ??? [A]foldRight(_: List[A], Nil: List[A])(Cons(_,_)) == identity[A]
  // exercise 3.9:
  def length[A](as: List[A]): Int =
    foldRight(as, 0)( (_,count) => count+1 )

}

object chapter3stacksafe {
  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {

    def apply[A](as: A*): List[A] =
      as.foldRight(Nil: List[A])(Cons(_,_))

    // exercise 3.10
    @tailrec
    def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B =
      as match {
        case Nil => z
        case Cons(x, xs) => foldLeft(xs, f(z,x))(f)
      }

    // exercise 3.11
    def sum(ns: List[Int]): Int =
      foldLeft(ns, 0)(_ + _)

    def product(ns: List[Double]): Double =
      foldLeft(ns, 1.0)(_ * _)

    def length[A](as: List[A]): Int =
      foldLeft(as, 0)( (count, _) => count+1 )

    // exercise 3.12
    def reverse[A](l: List[A]): List[A] =
      foldLeft(l, Nil: List[A])( (z, e) => Cons(e, z))

    // exercise 3.13
    def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B): B =
      foldLeft(reverse(as), z)( (b,a) => f(a,b))

    // exercise 3.14
    def append[A](a: List[A], b: List[A]): List[A] =
      foldRight(a, b)(Cons(_,_))

    // exercise 3.15
    def flatten[A](l: List[List[A]]): List[A] =
      foldLeft(l, Nil: List[A])(append)

    // exercise 3.16
    def addOne(l: List[Int]): List[Int] =
      foldLeft(l, Nil: List[Int])( (z,i) => Cons(i+1,z))

    // exercise 3.17
    def doublesToString(l: List[Double]): List[String] =
      foldLeft(l, Nil: List[String])( (z,d) => Cons(d.toString,z))

    // exercise 3.18
    def map[A,B](l: List[A])(f: A => B): List[B] =
      foldLeft(l, Nil: List[B])( (z,i) => Cons(f(i),z))

    // exercise 3.19
    def filter[A](l: List[A])(f: A => Boolean): List[A] =
      foldLeft(l, Nil: List[A])( (z,a) => if (f(a)) Cons(a,z) else z )

    // exercise 3.20
    def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
      flatten(map(l)(f))

    // exercise 3.21
    def filter_21[A](l: List[A])(f: A => Boolean): List[A] =
      flatMap(l)( a => if (f(a)) apply(a) else Nil)

    // exercise 3.22, 3.23
    def addLists(a: List[Int], b: List[Int]): List[Int] = zipWith(a, b)(_ + _)
    def zipWith_v1[A,B,C](a: List[A], b: List[B])(f: (A,B) => C): List[C] =
      (a,b) match {
        case (Cons(a,as), Cons(b,bs)) => Cons(f(a,b), zipWith(as,bs)(f))
        case _ => Nil
      }

    def zipWith[A,B,C](a: List[A], b: List[B])(f: (A,B) => C): List[C] = {
      @tailrec def loop(a: List[A], b: List[B], z: List[C] = Nil): List[C] = 
        (a,b) match {
          case (Cons(a,as), Cons(b,bs)) => loop(as, bs, Cons(f(a,b), z))
          case _ => z
        }
      reverse(loop(a,b))
    }

    // exercise 3.24
    def hasSubsequence[A](s: List[A], t: List[A]): Boolean = {
      @tailrec
      def isPrefix(s: List[A], t: List[A]): Boolean = (s,t) match {
        case (_, Nil) => true
        case (Nil, _) => false
        case (Cons(s1, ss), Cons(t1, ts)) => 
          if (s1 == t1) isPrefix(ss, ts)
          else false
      }
      @tailrec
      def loop(s: List[A]): Boolean = (s,t) match {
        case (Nil, Nil) => true
        case (Nil, _) => false
        case (Cons(s1,ss), _) => isPrefix(s, t) || loop(ss)
      }
      loop(s)
    }

    sealed trait Tree[+A]
    case class Leaf[A](value: A) extends Tree[A]
    case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

    // exercise 3.25
    def size[A](t: Tree[A]): Int = {
      def loop[A](s: Tree[A], o: List[Tree[A]], count: Int): Int = 
        (s,o) match {
          case (Leaf(_), Nil) => count+1
          case (Leaf(_), Cons(head,rest)) => loop(head, rest, count+1)
          case (Branch(l,r), o) => loop(l, Cons(r, o), count+1)
        }
      loop(t, Nil, 0)
    }

    // exercise 3.26
    def maximum(t: Tree[Int]): Int = {
      def loop(t: Tree[Int], s: List[Tree[Int]], max: Option[Int]): Int =
        (t, s, max) match {
          case (Leaf(i), Nil, None) => 
            i
          case (Leaf(i), Nil, Some(j)) => 
            i max j
          case (Leaf(i), Cons(head, rest), None) => 
            loop(head, rest, Some(i))
          case (Leaf(i), Cons(head, rest), Some(j)) => 
            loop(head, rest, Some(i max j))          
          case (Branch(l, r), o, m) =>
            loop(l, Cons(r, o), m)
        }
      loop(t, Nil, None)  
    }

    // exercise 3.27
    def depth[A](t: Tree[A]): Int = t match {
      case Leaf(_) => 1
      case Branch(l,r) => depth(l) max depth(r)
    } // not stack-safe

    def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
      case Leaf(a) => Leaf(f(a))
      case Branch(l,r) => Branch(map(l)(f), map(r)(f))
    } // not stack-safe

    // exercise 3.29
    def fold[A,B](t: Tree[A])(leaf: A => B)(branch: (B,B) => B): B = {
      def loop(t: Tree[A]): B = t match {
        case Leaf(a) => leaf(a)
        case Branch(l,r) => branch(loop(l), loop(r))
      }
      loop(t)
    } // not stack-safe
    def size_[A](t: Tree[A]): Int = fold(t)(_ => 1)(_ + _)
    def maximum_(t: Tree[Int]): Int = fold(t)(identity)(_ max _)
    def depth_[A](t: Tree[A]): Int = fold(t)(_ => 1)(_ max _)
    def map_[A,B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_,_))
  }

}