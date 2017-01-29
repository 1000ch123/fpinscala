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
}