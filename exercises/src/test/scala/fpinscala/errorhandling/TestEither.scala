package test.fpinscala.errorhandling

import org.scalatest._
import fpinscala.errorhandling._


class TestEither extends FunSuite with Matchers{
  test("basic either usage"){
    // 本とmethod名異なるので注意...
    assert(Right(1).get == 1)
    assert(Left("error").get == "error")
  }

  test("4.06: map"){
    assert(Right(1).map(_ * 2) == Right(2))
    assert((Left("error"): Either[String,Int]).map(_ * 2) == Left("error"))
  } 
}