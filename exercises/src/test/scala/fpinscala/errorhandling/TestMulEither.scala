package test.fpinscala.errorhandling

import org.scalatest._
import fpinscala.errorhandling.muleither._


class TestMulEither extends FunSuite with Matchers{
  test("4.08: mkPerson: single erros"){
    // Name / Age が sealed class なので 正常系作れなかったorz
    assert(Person.mkPerson("", 25) == Left(List("Name is empty.")))
    assert(Person.mkPerson("1000ch", -25) == Left(List("Age is out of range.")))
  }
  test("4.08: mkPerson: multi errors"){
    assert(Person.mkPerson("", -25) == Left(List("Name is empty.", "Age is out of range.")))
  }
}