package test.fpinscala.datastructures2

import org.scalatest._
import fpinscala.datastructures2.{List => MyList}


class TestDataStructures2 extends FunSuite with Matchers{
  test("3.24: startsWith"){
    def add: (Int, Int) => Int = _ + _
    MyList.startsWith(List(1,2,3,4), List(1)) shouldBe true
    MyList.startsWith(List(1,2,3,4), List(1,2,3)) shouldBe true
    MyList.startsWith(List(1,2,3,4), List(1,2,3,4)) shouldBe true
    MyList.startsWith(List(1,2,3,4), List(2)) shouldBe false
  }

  test("3.24: hasSubsequence"){
    def add: (Int, Int) => Int = _ + _
    MyList.hasSubsequence(List(1,2,3,4))(List(1)) shouldBe true
    MyList.hasSubsequence(List(1,2,3,4))(List(1,2,3)) shouldBe true
    MyList.hasSubsequence(List(1,2,3,4))(List(2,3)) shouldBe true
    MyList.hasSubsequence(List(1,2,3,4))(List()) shouldBe true
    MyList.hasSubsequence(List(1,2,3,4))(List(1,2,4)) shouldBe false
    MyList.hasSubsequence(List(1,2,3,4))(List(5)) shouldBe true
    MyList.hasSubsequence(List())(List(1)) shouldBe false
  }
}