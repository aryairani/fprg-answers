package fp_in_scala

class chapter4test extends org.specs2.mutable.Specification {

  "chapter 4" >> {
    "traverse examples" >> {
      import fp_in_scala.chapter3.List
      import fp_in_scala.chapter4.{Either, Left, Right}
      val l1 = List[Either[Int,Int]](Right(1), Right(2), Right(3))
      val l2 = List[Either[Int,Int]](Left(1),  Right(2), Right(3))
      val l3 = List[Either[Int,Int]](Right(1), Right(2), Left(3))


      Either.sequence(l1) must_== Right(List(1,2,3))
      Either.sequence(l2) must_== Left(1)
      Either.sequence(l3) must_== Left(3)
    }
  }
}

class chapter4spec extends org.specs2.Specification {
  import fp_in_scala.chapter3.List
  import fp_in_scala.chapter4.{Either, Left, Right}
  val l1 = List[Either[Int,Int]](Right(1), Right(2), Right(3))
  val l2 = List[Either[Int,Int]](Left(1),  Right(2), Right(3))
  val l3 = List[Either[Int,Int]](Right(1), Right(2), Left(3))

  def is = s2"""
  chapter 4
    sequence tests
      all rights    $allRights
      early left    $earlyLeft
      late left     $lateLeft
  """

  def allRights = Either.sequence(l1) must_== Right(List(1,2,3))
  def earlyLeft = Either.sequence(l2) must_== Left(1)
  def lateLeft = Either.sequence(l3) must_== Left(3)
}
