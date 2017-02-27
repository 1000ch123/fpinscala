package test.fpinscala.datastructures.tree

import org.scalatest._
import fpinscala.datastructures._


class TestTree extends FreeSpec with Matchers{

  "3.25" - {
    "size" in {
      Tree.size(Leaf(1)) shouldBe 1
      Tree.size(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) shouldBe 3    
    }
  }
}