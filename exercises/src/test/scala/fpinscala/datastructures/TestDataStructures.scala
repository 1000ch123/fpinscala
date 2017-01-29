import org.scalatest._
import fpinscala.datastructures.List


class TestDataStructures extends FunSuite with Matchers{
  test("sample: array assetion"){
    // ref: http://www.scalatest.org/user_guide/using_matchers
    Array(1, 2, 3) should equal (Array(1, 2, 3))
    Array(1, 2, 3) should === (Array(1, 2, 3))
    Array(1, 2, 3) should be (Array(1, 2, 3))
    Array(1, 2, 3) shouldEqual Array(1, 2, 3)
    Array(1, 2, 3) shouldBe Array(1, 2, 3) // fastest to compile, no parentheses required
  }
  
  test("3.01: pattern match"){
    //   Array(1,2,3,4,5)
    // が
    //   case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y 
    // に match
    //   1 + 2 = 3
    // となる 
    assert(List.x == 3)
  }

  test("3.02: tail"){
    List.tail(List(1,2,3)) shouldBe List(2,3)
    List.tail(List(1,2,3,4,5)) shouldBe List(2,3,4,5)
    List.tail(List()) shouldBe List()
  }

  test("3.03: setHead"){
    List.setHead(List(1,2,3), 5) shouldBe List(5,2,3)
    List.setHead(List(), 5) shouldBe List()
  }

  test("3.04: drop"){
    List.drop(List(1,2,3), 2) shouldBe List(3)
    List.drop(List(1,2,3), 0) shouldBe List(1,2,3)
    List.drop(List(1,2,3), 3) shouldBe List()
    List.drop(List(1,2,3), 5) shouldBe List()
  }
}