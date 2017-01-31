package test.fpinscala.errorhandling

import org.scalatest._
import fpinscala.errorhandling._


class TestEither extends FunSuite with Matchers{
  test("basic either usage"){
    // 本とmethod名異なるので注意...
    assert(Right(1).get == 1)
    assert(Left("error").get == "error")
  }
}