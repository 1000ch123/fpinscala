package test.fpinscala.datastructures

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

  test("3.05: dropWhile"){
    val isEven: Int => Boolean = _ % 2 == 0
    val isOdd: Int => Boolean = !isEven(_)
    val overFive: Int => Boolean = _ > 5
     
    List.dropWhile(List(2, 4, 1, 2, 3, 4), isEven) shouldBe List(1, 2, 3, 4)
    List.dropWhile(List(1, 3, 5, 2, 4, 6), isOdd) shouldBe List(2, 4, 6)
    List.dropWhile(List(10, 20, 30, 5, 6, 7), overFive) shouldBe List(5, 6, 7)
  }

  test("3.05': dropWhile2"){
    val isEven: Int => Boolean = _ % 2 == 0
    val isOdd: Int => Boolean = !isEven(_)
    val overFive: Int => Boolean = _ > 5
     
    List.dropWhile2(List(2, 4, 1, 2, 3, 4), isEven) shouldBe List(1, 2, 3, 4)
    List.dropWhile2(List(1, 3, 5, 2, 4, 6), isOdd) shouldBe List(2, 4, 6)
    List.dropWhile2(List(10, 20, 30, 5, 6, 7), overFive) shouldBe List(5, 6, 7)
  }

  test("3.05'': dropWhile curried"){
    // curry化されている. 型は下記
    //  def dropWhile3[A](l: List[A])(f: A => Boolean): List[A]
    // 1つめのカッコでAの型がきまるので，fの型アノテーションが必要なくなる
    // dropwhileでもそうしてくれればいいのに...(Haskellとかはそうしてくれるよね)
    // 要するに 型推論がどこまで効くか，って話
    List.dropWhile3(List(2, 4, 1, 2, 3, 4))( _ % 2 == 0) shouldBe List(1, 2, 3, 4)
    List.dropWhile3(List(1, 3, 5, 2, 4, 6))(_ % 2 != 0) shouldBe List(2, 4, 6)
    List.dropWhile3(List(10, 20, 30, 5, 6, 7))(_ > 5) shouldBe List(5, 6, 7)
  }

  test("3.06: init"){
    /**
      len(l) = n のとき
      tail(l) は Cons(x,xs) でxs返すだけ.1回の処理でok
      init(l) は Cons(x,Nil) にぶつかるまでn回checkが必要
    */
    List.init(List(1,2,3,4)) shouldBe List(1,2,3)
    List.init(List(3)) shouldBe List()
    List.init(List()) shouldBe List()
  }


  test("3.09: len"){
    List.length(List(1,2,3,4)) shouldBe 4
    List.length(List(3)) shouldBe 1
    List.length(List()) shouldBe 0
  }
  
  test("3.10: foldLeft"){
    List.foldLeft(List(1,2,3,4), 0)(_ + _) shouldBe 10
  }

  test("3.11: reverse"){
    List.reverse(List(1,2,3,4)) shouldBe List(4,3,2,1)
    List.reverse(List()) shouldBe List()
  }

  test("3.16: succ list"){
    List.succ(List(1,2,3,4)) shouldBe List(2,3,4,5)
    List.succ(List(3)) shouldBe List(4)
    List.succ(List()) shouldBe List()
  }

  test("3.17: double to str"){
    List.doubleToString(List(1,2,3,4)) shouldBe List("1.0", "2.0", "3.0", "4.0")
    List.doubleToString(List(3)) shouldBe List("3.0")
    List.doubleToString(List()) shouldBe List()
  }

  test("3.18: map"){
    def succ: Int => Int = _ + 1
    List.map(List(1,2,3,4))(succ) shouldBe List(2,3,4,5)
    List.map(List(3))(succ) shouldBe List(4)
    List.map(List())(succ) shouldBe List()
  }

  test("3.19: filter"){
    def isEven: Int => Boolean = _ % 2 == 0
    List.filter(List(1,2,3,4))(isEven) shouldBe List(1,3)
    List.filter(List(3))(isEven) shouldBe List(3)
    List.filter(List())(isEven) shouldBe List()
  }

  test("3.20: fmap"){
    def twice: Int => List[Int] = x => List(x, x)
    List.flatMap(List(1,2,3,4))(twice) shouldBe List(1,1,2,2,3,3,4,4)
    List.flatMap(List(3))(twice) shouldBe List(3,3)
    List.flatMap(List())(twice) shouldBe List()
  }

  test("3.20': fmap2"){
    def twice: Int => List[Int] = x => List(x, x)
    List.flatMap2(List(1,2,3,4))(twice) shouldBe List(1,1,2,2,3,3,4,4)
    List.flatMap2(List(3))(twice) shouldBe List(3,3)
    List.flatMap2(List())(twice) shouldBe List()
  }

  test("3.21: filter2"){
    def isEven: Int => Boolean = _ % 2 == 0
    List.filter2(List(1,2,3,4))(isEven) shouldBe List(1,3)
    List.filter2(List(3))(isEven) shouldBe List(3)
    List.filter2(List())(isEven) shouldBe List()
  }

  test("3.22: merge"){
    List.merge(List(1,2,3,4))(List(10,20,30,40)) shouldBe List(11,22,33,44)
    List.merge(List(1))(List(10,20,30,40)) shouldBe List(11)
    List.merge(List())(List(10,20,30,40)) shouldBe List()
  }

  test("3.23: zipWith"){
    def add: (Int, Int) => Int = _ + _
    List.zipWith(add)(List(1,2,3,4))(List(10,20,30,40)) shouldBe List(11,22,33,44)
    List.zipWith(add)(List(1))(List(10,20,30,40)) shouldBe List(11)
    List.zipWith(add)(List())(List(10,20,30,40)) shouldBe List()
  }
}