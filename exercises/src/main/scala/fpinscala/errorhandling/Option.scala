package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match{
    case None => None
    case Some(a) => Some(f(a))
  }

  def getOrElse[B>:A](default: => B): B = this match{
    case None => default
    case Some(a) => a
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this.map(f).getOrElse(None)

  def orElse[B>:A](ob: => Option[B]): Option[B] = this.map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] = if (this.map(f).getOrElse(false)) this else None
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = {
    // xs の1つの要素を変換する関数
    def convert(m: Double): (Double => Double) =
      x => math.pow(x - m, 2)

    // xs を mean で convertする
    // xs を closureで使ってるから なんだかなぁって感じはある
    def parts: (Double => Seq[Double]) =
      m => xs.map(convert(m))

    mean(xs).map(parts).flatMap(mean)
  }

  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

  def lift2[A,B,C](f: (A, B) => C): (Option[A], Option[B]) => Option[C] = 
    (a,b) => a flatMap (a => b map (b => f(a,b)))
    // _ flatmap (_ map f.curried)  // Option[B] * Option[B=>C] ができないのでダメ => appricative functorだ

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = 
    a.flatMap( x => b.map( y => f(x, y)))
    //b.map(a.map(f.curried).getOrElse(x => x))  //getOrElseで B => C つくれないから駄目か

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    /**
      foldRightするときに，型annotationが必要になるらしい...
      そうしないと
      > test-only test.fpinscala.errorhandling.TestErrorHandling
        [error]  found   : fpinscala.errorhandling.Option[List[A]]
        [error]  required: fpinscala.errorhandling.Some[List[A]]
        [error]     a.foldRight(Some(Nil: List[A]))(map2(_,_)(cons))
      とひたすら怒られていた
      こんなん知らんわ...
      type annotation で怒られた場合，個別引数でannotaionするのではなく []つかってあげるとscalaコンパイラがよしなにしてくれるのかなぁ
     */
    def cons(x: A, xs: List[A]): List[A] = x :: xs
    def optionCons(x: Option[A], xs: Option[List[A]]): Option[List[A]] = map2(x,xs)(cons)

    //a.foldRight(Some(Nil: List[A]))(optionCons)
    //a.foldRight(Some(Nil: List[A]))(map2(_,_)(cons))
    a.foldRight[Option[List[A]]](Some(Nil))(map2(_,_)(_ :: _))
    
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = 
    /**
    sequenceの応用
    入力が List[A] となるので 変換処理 A=>option[B] を挟んだ状態でmap2すればok
    しかしさすがにこれは読みづらいなぁ
    */
    a.foldRight[Option[List[B]]](Some(Nil))((x,xs) => map2(f(x), xs)(_ :: _))
}