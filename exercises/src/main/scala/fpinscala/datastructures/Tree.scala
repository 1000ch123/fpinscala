package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
    def size[A](t: Tree[A]): Int = t match {
        case Leaf(_) => 1
        case Branch(l ,r) => size(l) + size(r)
    }

    def maximum(t: Tree[Int]): Int = t match {
        case Leaf(v) => v
        case Branch(l ,r) => maximum(l) max maximum(r)
    }

    // 題意がわからん
    // もしかして: R/L のlistを返すべき...?
    def depth[A](t: Tree[A], target: A): Int =  t match {
        case Leaf(v) => if (v == target) 1 else 0
        case Branch(l ,r) => depthStep(depth(l, target), depth(r, target))
    }

    def depthStep(a: Int, b: Int): Int = (a,b) match {
        case (0,0) => 0
        case (x,y) => x.max(y) + 1
    }

    def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
        case Leaf(a) => Leaf(f(a))
        case Branch(l ,r) => Branch(map(l)(f), map(r)(f))
    }

    def fold[A,B](t: Tree[A])(acc: B)(f: (A,B) => B): B = t match {
        case Branch(l ,r) => fold(l)(fold(r)(acc)(f))(f)
        case Leaf(a) => f(a, acc)
    }

    def size_[A](t: Tree[A]): Int = fold(t)(0)((x,y) => y + 1)

    // foldの仕様上 acc として与える値が必要なので注意
    def maximum_(t: Tree[Int]): Int = fold(t)(Int.MinValue)(_ max _) 

    // 呼び出しを意識しないといけないfoldingって面倒だな..
    // branch経由のたびにfuncは呼ばれるからなんとかなるか？
    // それを max取る感じかねぇ
    def depth_[A](t: Tree[A], target: A): Int =  ???

    // empty tree(ListでいうNil)作れない構造だと無理では..?
    def map_[A,B](t: Tree[A])(f: A => B): Tree[B] = ??? //fold(t)()  
}