package test.fpinscala.errorhandling

import org.scalatest._
import fpinscala.errorhandling._


class TestErrorHandling extends FunSuite with Matchers{
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
}