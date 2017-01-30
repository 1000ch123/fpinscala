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
}