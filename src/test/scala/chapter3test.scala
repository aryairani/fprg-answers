package fp_in_scala.chapter3

class chapter3test extends org.specs2.mutable.Specification {

  "chapter 3 tests" >> {
    import chapter3._

    "tail" >> {
      tail(List(1,2,3)) must_== List(2,3)
      tail(List(1)) must_== Nil
      tail(Nil) must_== Nil
    }

    "setHead" >> {
      setHead(Nil, 3) must_== List(3)
      setHead(List(1,2,3), 5) must_== List(5,2,3)
    }

    "drop" >> {
      drop(List(1,2,3),3) must_== Nil
      drop(Nil,3) must_== Nil
      drop(List(1,2,3),2) must_== List(3)
    }

    "dropWhile" >> {
      dropWhile[Int](List(1,2,3,4,5), _ < 4) must_== List(4,5)
    }

    "init" >> {
      init(List(1,2,3,4)) must_== List(1,2,3)
    }
  }
}
