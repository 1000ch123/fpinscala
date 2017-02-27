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

  "3.28" - {
    "map" in {
      Tree.map(Leaf(1))(_+1) shouldBe Leaf(2)
      Tree.map(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)))(_+1) shouldBe Branch(Branch(Leaf(2), Leaf(3)), Leaf(4))
    }
  }

  "3.29" - {
    "fold" - {
      "sum" in {
        Tree.fold(Leaf(1))(0)(_ + _) shouldBe 1
        Tree.fold(Branch(Leaf(1), Leaf(2)))(0)(_ + _) shouldBe 3
        Tree.fold(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)))(0)(_ + _) shouldBe 6
      }
      "size" in {
        Tree.size_(Leaf(1)) shouldBe 1
        Tree.size_(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) shouldBe 3    
      }
      "maximum" in {
        Tree.maximum_(Leaf(1)) shouldBe 1
        Tree.maximum_(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) shouldBe 3
        Tree.maximum_(Branch(Branch(Leaf(3), Leaf(2)), Leaf(1))) shouldBe 3
      }
    }
  }
}