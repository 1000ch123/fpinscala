package test.fpinscala.errorhandling

import org.scalatest._
import fpinscala.errorhandling._


class TestOption extends FunSuite with Matchers{
  test("basic option usage"){
    assert(Some(1).get == 1)
    assert((None: Option[Int]) == None)
  }

  test("4.01: higher order functions: map"){
    assert(Some(1).map(_ * 5) == Some(5))
    assert((None: Option[Int]).map(_ * 5) == None)
  }

  test("4.01: higher order functions: getOrElse"){
    assert(Some(1).getOrElse(0) == 1)
    assert(None.getOrElse(0) == 0)
  }

  test("4.01: higher order functions: flatMap"){
    val f: Int => Option[Int] =
      x => if (x % 2 == 0) Some(x) else None
    assert(Some(1).flatMap(f) == None)
    assert(Some(2).flatMap(f) == Some(2))
    assert((None: Option[Int]).flatMap(f) == None)
  }

  test("4.01: higher order functions: orElse"){
    assert(Some(1).orElse(Some(2)) == Some(1))
    assert(Some(1).orElse(None) == Some(1))
    assert(None.orElse(Some(2)) == Some(2))
    assert(None.orElse(None) == None)
  }

  test("4.01: higher order functions: filter"){
    val f: Int => Boolean = _ % 2 == 0
    assert(Some(1).filter(f) == None)
    assert(Some(2).filter(f) == Some(2))   
    assert((None: Option[Int]).filter(f) == None)
  }

  test("4.02: variance"){
    assert(Option.variance(Seq[Double](1,2,3,4,5)) == Some(2))
    assert(Option.variance(Seq[Double](0)) == Some(0))
    assert(Option.variance(Seq[Double]()) == None)
  }

  test("4.03: map2"){
    assert(Option.map2(Some(1), Some(2))(_ + _) == Some(3))
    assert(Option.map2(Some(1), None)(_ + _) == None)
    assert(Option.map2(None: Option[Int], Some(2))(_ + _) == None)
    assert(Option.map2(None: Option[Int], None)(_ + _) == None)
  }

  test("4.04: sequence"){
    assert((Option sequence List(Some(1), Some(2), Some(3))) == Some(List(1,2,3)))
    assert((Option sequence List(Some(1), Some(2), None)) == None)
  }

  test("4.05: traverse"){
    def f(x: Int): Option[Int] = if (x >= 0) Some(x) else None
    assert(Option.traverse(List(1,2,3))(f) == Some(List(1,2,3)))
    assert(Option.traverse(List(1,2,-3))(f) == None)
    assert(Option.traverse(List())(f) ==  Some(List()))
  }
}

class TestErrorHandlingSpec extends FunSpec with Matchers{
  describe("4.01"){
    describe("Option.map"){
      describe("with Some value"){
        it("should map it's value with given function"){
          assert(Some(1).map(_ * 5) == Some(5))        
        }
      }
      describe("with None value"){
        it("should return None"){
          assert((None: Option[Int]).map(_ * 5) == None)
        }
      }
    }
    describe("Option.flatMap"){
      val f: Int => Option[Int] =
        x => if (x % 2 == 0) Some(x) else None
      describe("with Some value"){
        it("should map value with given function and faltten it."){
          assert(Some(2).flatMap(f) == Some(2))
        }
        it("should return None if mapped value is None"){
          assert(Some(1).flatMap(f) == None)
        }
      }
      describe("with None value"){
        it("should return None"){
          assert((None: Option[Int]).flatMap(f) == None)
        }
      }
    }
  }
}