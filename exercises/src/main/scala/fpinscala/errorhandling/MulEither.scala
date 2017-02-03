package fpinscala.errorhandling.muleither


import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E,+A] {
 def map[B](f: A => B): Either[E, B] = this match{
   case Left(e) => Left(e)
   case Right(a) => Right(f(a))
 }

 def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match{
   case Left(e) => Left(e)
   case Right(a) => f(a)
 }
  //this.map(f).map(_.get) だと either.getを呼ぶので駄目っぽい..うーん

 def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match{
   case Left(e) => b
   case Right(a) => Right(a)
 }

 // ex4.08
 // map2にて returnする EitherのLeftをちょいと変えた
 def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[List[EE], C] = (this, b) match {
   case (Left(e1), Left(e2)) => Left(List(e1,e2)) 
   case (Left(e1), _) => Left(List(e1))
   case (_, Left(e2)) => Left(List(e2)) 
   case (Right(a1), Right(a2)) => Right(f(a1, a2))
 }
}

case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

// ex4.08
// mkName / mkAge 両方の失敗を知るにはどうすればよいか？ 
case class Person(name: Name, age: Age)
sealed class Name(val value: String)
sealed class Age(val value: Int)

object Person{
  def mkName(name: String): Either[String, Name] =
    if (name == "" || name == null) Left("Name is empty.")
    else Right(new Name(name))

  def mkAge(age: Int): Either[String, Age] =
    if (age < 0) Left("Age is out of range.")
    else Right(new Age(age))

  def mkPerson(name: String, age: Int): Either[List[String], Person] =
  mkName(name).map2(mkAge(age))(Person(_, _))
}