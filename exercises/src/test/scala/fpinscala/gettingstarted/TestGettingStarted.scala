import org.scalatest.FunSuite
import fpinscala.gettingstarted.MyModule
import fpinscala.gettingstarted.PolymorphicFunctions


class TestGettingStarted extends FunSuite {

  test("2.01: fibonacci") {
    assert(MyModule.fib(0) == 0)
    assert(MyModule.fib(1) == 1)
    assert(MyModule.fib(2) == 1)
    assert(MyModule.fib(3) == 2)
    assert(MyModule.fib(4) == 3)
    assert(MyModule.fib(5) == 5)
    assert(MyModule.fib(6) == 8)
  }

  test("2.02: isSorted") {
    assert(PolymorphicFunctions.isSorted(Array(1,2,3,4), (x: Int, y:Int) => x < y))
  }

  test("2.03: currying"){
    object Module {
      def adder(x: Int, y:Int): Int = x + y
      val addThree = PolymorphicFunctions.curry(adder)(3)
    }
    
    assert((Module addThree 1) == 4)
    assert((Module addThree 3) == 6)
    assert((Module addThree 6) == 9)
  }

  test("2.04: uncurrying"){
    def curriedAdder: Int => Int => Int = x => y => x+y
    val adder = PolymorphicFunctions.uncurry(curriedAdder)
    
    assert(adder(1, 3) == 4)
    assert(adder(3, 5) == 8)
    assert(adder(-3, 6) == 3)
  }

  test("2.05: compose"){
    val mul2: Int => Int = _ * 2
    val plus3: Int => Int = _ + 3
    val mul2plus3 = PolymorphicFunctions.compose(plus3, mul2)
    val plus3mul2 = PolymorphicFunctions.compose(mul2, plus3)
    
    assert(mul2plus3(3) == 9)
    assert(plus3mul2(3) == 12)
  }
}