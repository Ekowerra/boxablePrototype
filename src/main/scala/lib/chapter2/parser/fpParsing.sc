import lib.chapter2.testing._
import Prop._

trait Parsers[ParseError, Parser[+_]] { self =>
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def char(c : Char): Parser[Char] = string(c.toString) map (_.charAt(0))

  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]
  implicit def string(s : String): Parser[String]
  implicit def operators[A](p : Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

  def many[A](p: Parser[A]): Parser[List[A]]

  def flatMap[A,B](a : Parser[A])(f : A => Parser[B]): Parser[B]

  def map[A,B](a : Parser[A])(f : A => B): Parser[B]

  def map2[A,B,C](a: Parser[A], b : Parser[B])(f: (A,B) => C): Parser[C]

  def sequence[A](la: List[Parser[A]]): Parser[List[A]]

  def succeed[A](a:A): Parser[A] =
    string("") map (_ => a)

  def slice[A](p:Parser[A]): Parser[String]

  case class ParserOps[A](p : Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p,p2)
    def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p,p2)
    def many: Parser[List[A]] = self.many(p)
    def map[B](f : A => B): Parser[B] = self.map(p)(f)
    def slice : Parser[String] = self.slice(p)
  }

  val numA: Parser[Int] = char('a').many.map(_.size)

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def maplaw[A](p: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)
  }
}

