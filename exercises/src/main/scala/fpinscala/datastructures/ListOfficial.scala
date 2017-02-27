package fpinscala.datastructures2

object List {
  def hasSubsequence[A](sup: List[A])(sub: List[A]): Boolean = ???

  def startsWith[A](as: List[A], ts: List[A]): Boolean = (as, ts) match {
    case (_, Nil) => true
    case (x::xs, y::ys) if (x == y) => startsWith(xs,ys)
    case (_, _) => false
  }
}
