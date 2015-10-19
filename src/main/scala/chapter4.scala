package fp_in_scala

import annotation.tailrec

object chapter4 {
  case class Some[+A](get: A) extends Option[A] {
    def fold[B](none: => B)(some: A => B): B = some(get)
  }
  case object None extends Option[Nothing] {
    def fold[B](none: => B)(some: Nothing => B): B = none
  }

  // exercise 4.1
  sealed trait Option[+A] {
    def fold[B](none: => B)(some: A => B): B

    def map[B](f: A => B): Option[B] = this match {
      case None ⇒ None
      case Some(a) ⇒ Some(f(a))
    }

    def flatMap[B](f: A => Option[B]): Option[B] = this match {
      case None ⇒ None
      case Some(a) ⇒ f(a)
    }

    def getOrElse[B >: A](default: => B): B =
      fold[B](default)(identity)

    def orElse[B >: A](ob: => Option[B]): Option[B] =
      fold[Option[B]](ob)(Some.apply)

    def filter(f: A => Boolean): Option[A] = this match {
      case None ⇒ None
      case Some(a) ⇒ if (f(a)) Some(a) else None    
    }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // exercise 4.2
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => 
      mean(
        xs.map(x => math.pow(x-m, 2))
      )
    )

  // exercise 4.3
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    (a,b) match {
      case (Some(a), Some(b)) => Some(f(a,b))
      case _ => None
    }

  // exercise 4.4
   import chapter3._, stacksafe._
  def sequence_[A](a: List[Option[A]]): Option[List[A]] = {
    @tailrec def loop(a: List[Option[A]], z: List[A]): Option[List[A]] =
      a match {
        case Nil => Some(reverse(z))
        case Cons(None,_) => None
        case Cons(Some(a), os) => loop(os, Cons(a, z))
      }
    loop(a, Nil)
  }

  // exercise 4.5
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    @tailrec def loop(a: List[A], z: List[B]): Option[List[B]] =
      a match {
        case Nil => Some(reverse(z))
        case Cons(a, as) => f(a) match {
          case Some(b) => loop(as, Cons(b, z))
          case None => None
        }
      }
    loop(a, Nil)
  }

  def traverse_1[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    stacksafe.foldLeft[A,Option[List[B]]](a, Some(Nil)) {
      case (Some(bs), a) => f(a).map(Cons(_, bs))
      case (shortCircuit, _) => shortCircuit
    } map (reverse)

  def sequence[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(identity)

  // exercise 4.6
  sealed trait Either[+E, +A] {
    def fold[B](l: E => B, r: A => B): B
    def map[B](f: A => B): Either[E,B] =
      fold(Left.apply, f andThen Right.apply)
    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
      fold(Left.apply, f)
    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
      fold(_ => b, Right.apply)
    def map2[EE >:E, B, C](b: Either[EE, B])(f: (A,B) => C): Either[EE, C] =
      for {
        aa <- this
        bb <- b
      } yield (f(aa,bb))
  }
  case class Left[+E](value: E) extends Either[E,Nothing] {
    def fold[B](l: E => B, r: Nothing => B):B = l(value)
  }
  case class Right[+A](value: A) extends Either[Nothing, A] {
    def fold[B](l: Nothing => B, r: A => B): B = r(value)
  }

  object Either {
    // exercise 4.7
    def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
      @tailrec def loop(as: List[A], z: List[B]): Either[E, List[B]] =
        as match {
          case Nil => Right(reverse(z))
          case Cons(a, as) => f(a) match {
            case x @ Left(e) => x
            case Right(b) => loop(as, Cons(b, z))
          }
        }
      loop(as, Nil)
    }
    def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(identity)
  }

  // Listing 4.4
  case class Person(name: Name, age: Age)
  sealed class Name(val value: String)
  sealed class Age(val value: Int)

  def mkName(name: String): Either[String, Name] =
    if (name == "" || name == null) Left("Name is empty.")
    else Right(new Name(name))

  def mkAge(age: Int): Either[String, Age] =
    if (age < 0) Left("Age is out of range.")
    else Right(new Age(age))

  def mkPerson(name: String, age: Int): Either[String, Person] =
    mkName(name).map2(mkAge(age))(Person(_, _))

  // Exercise 4.8: Capture all error messages
  case class Success[A](a: A) extends ValidationNEL[Nothing, A]
  case class Failure[E](es: List[E]) extends ValidationNEL[E, Nothing] // takes a list

  trait ValidationNEL[+E,+A] {
    def fold[B](fail: List[E] => B, success: A => B): B =
      this match {
        case Success(a) ⇒ success(a)
        case Failure(es) ⇒ fail(es)
      }

    def map[B](f: A => B): ValidationNEL[E,B] =
      fold(Failure.apply, f andThen Success.apply)

    def flatMap[EE >: E, B](f: A => ValidationNEL[EE, B]): ValidationNEL[EE, B] =
      fold(Failure.apply, f)

    // should this carry old errors?
    def orElse[EE >: E, B >: A](that: => ValidationNEL[EE, B]): ValidationNEL[EE, B] =
      fold(_ => that, Success.apply)

    def map2[EE >:E, B, C](that: ValidationNEL[EE, B])(f: (A,B) => C): ValidationNEL[EE, C] =
      (this, that) match {
        case (Success(a), Success(b)) ⇒ Success(f(a,b))
        case (Failure(e1), Failure(e2)) ⇒ Failure(append(e1,e2))
        case (Success(a), Failure(e)) ⇒ Failure(e)
        case (Failure(e), Success(b)) ⇒ Failure(e)
      }
  }

}
