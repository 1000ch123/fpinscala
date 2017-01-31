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

  test("4.06: flatMap"){
    def f(a: Int): Either[String,Int] = if (a >= 0) Right(a) else Left("negative value")
    assert(Right(1).flatMap(f) == Right(1))
    assert(Right(-1).flatMap(f) == Left("negative value"))
    assert((Left("error"): Either[String,Int]).flatMap(f) == Left("error"))
  }

  test("4.06: orElse"){
    assert(Right(1).orElse(Right(2)) == Right(1))
    assert(Right(1).orElse(Left("error2")) == Right(1))
    assert(Left("error1").orElse(Right(2)) == Right(2))
    assert(Left("error1").orElse(Left("error2")) == Left("error2"))
  }

   test("4.06: map2"){
    def f(x: Int, y:Int): Int = x + y
    assert(Right(1).map2(Right(2))(f) == Right(3))
    assert(Right(1).map2(Left("error2"))(f) == Left("error2"))
    assert(Left("error1").map2(Right(2))(f) == Left("error1"))
    assert(Left("error1").map2(Left("error2"))(f) == Left("error1"))
  }
}