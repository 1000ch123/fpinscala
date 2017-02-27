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

  "3.26" - {
    "maximum" in {
      Tree.maximum(Leaf(1)) shouldBe 1
      Tree.maximum(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) shouldBe 3
      Tree.maximum(Branch(Branch(Leaf(3), Leaf(2)), Leaf(1))) shouldBe 3
    }
  }

  "3.27" - {
    "depth" in {
      Tree.depth(Leaf(1), 1) shouldBe 1
      Tree.depth(Leaf(1), 2) shouldBe 0
      Tree.depth(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)), 1) shouldBe 3
      Tree.depth(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)), 3) shouldBe 2
      Tree.depth(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)), 4) shouldBe 0
    }
  }
}