package lib.chapter3.monoid

import lib.chapter2.testing.{Gen, Prop}

trait Monoid[A] { self =>
  def op(a1: A, a2: A):A
  def zero: A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  val intAddition = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 + a2
    val zero = 0
  }

  val intMultiplication = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 * a2
    val zero = 1
  }

  val booleanOr = new Monoid[Boolean] {
    def op(a1 : Boolean, a2: Boolean) = a1 || a2
    val zero = false
  }

  val booleanAnd = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 && a2
    val zero = true
  }


  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(a1 : A => A, a2: A => A) = a1 compose a2
    val zero: A => A = a => a
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(x: Option[A], y: Option[A]) = x orElse y
    val zero = None
  }

  // We can get the dual of any monoid just by flipping the `op`.
  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)
    val zero = m.zero
  }

  // Now we can have both monoids on hand:
  def firstOptionMonoid[A]: Monoid[Option[A]] = optionMonoid[A]
  def lastOptionMonoid[A]: Monoid[Option[A]] = dual(firstOptionMonoid)

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
  // Associativity
    Prop.forAll(for {
      x <- gen
      y <- gen
      z <- gen
    } yield (x, y, z))(p =>
      m.op(p._1, m.op(p._2, p._3)) == m.op(m.op(p._1, p._2), p._3)) &&
      // Identity
      Prop.forAll(gen)((a: A) =>
        m.op(a, m.zero) == a && m.op(m.zero, a) == a)


  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  def foldMap[A,B](as: List[A], m: Monoid[B])(f:A => B): B =
    concatenate(as map f, m)
    /*as.foldRight(m.zero)((a, acc) => m.op(f(a),acc))
    as.foldLeft(m.zero)((acc, a) => m.op(f(a),acc))
    */

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(f.curried)(z)

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, dual(endoMonoid[B]))(a => b => f(b, a))(z)

  def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    v match {
      case IndexedSeq() => m.zero
      case IndexedSeq(h) => f(h)
      case _ =>
        val (l,r) = v.splitAt(v.length/2)
        m.op(foldMapV(l, m)(f),foldMapV(r, m)(f))
    }

  def isOrdered(as : IndexedSeq[Int]): Boolean =
    foldMapV(as, new Monoid[(Int,Boolean)] {
      def op(a1 : (Int, Boolean), a2: (Int,Boolean)) =
        if(a1._1 <= a2._1) (a2._1, a1._2 && a2._2) else (a2._1, false)
      val zero = (0, true)
    })(x => (x,true))._2


  val wcMonoid:Monoid[WC] = new Monoid[WC] {
    override def op(a1: WC, a2: WC): WC = (a1,a2) match {
        // attention au nombre  de mots ! => + 1 n'est pas vraiment correct
      case (Part(a1l, a1w, a1r), Part(a2l, a2w, a2r)) => Part(a1l, a1w + a2w + (if ((a1r + a2l).isEmpty) 0 else 1), a2r)
      case (Stub(s), Part(l,w,r)) => Part(s+l, w,r)
      case (Part(l,w,r), Stub(s)) => Part(l, w,r+s)
      case (Stub(s1),Stub(s2)) => Stub(s1+s2)
    }

    override def zero: WC = Stub("")
  }

  def countWords(text : String): Int = {
    def wc(c: Char): WC =
      if (c.isWhitespace)
        Part("", 0, "")
      else
        Stub(c.toString)
    def unstub(s: String) = s.length min 1
    foldMapV(text.toIndexedSeq, wcMonoid)(wc) match {
      case Stub(s) => unstub(s)
      case Part(l, w, r) => unstub(l) + w + unstub(r)
    }
  }

  def productMonoid[A,B](a: Monoid[A], b:Monoid[B]): Monoid[(A,B)] =
    new Monoid[(A, B)] {
      override def op(a1: (A, B), a2: (A, B)): (A, B) = (a.op(a1._1, a2._1), b.op(a1._2, a2._2))

      override def zero: (A, B) = (a.zero, b.zero)
    }


  def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {
      def zero = Map[K,V]()
      def op(a: Map[K, V], b: Map[K, V]) =
        (a.keySet ++ b.keySet).foldLeft(zero) { (acc,k) =>
          acc.updated(k, V.op(a.getOrElse(k, V.zero),
            b.getOrElse(k, V.zero)))
        }
    }

  def functionMonoid[A,B](b: Monoid[B]): Monoid[A => B] =
    new Monoid[(A) => B] {
      override def op(a1: (A) => B, a2: (A) => B): (A) => B = a => b.op(a1(a),a2(a))

      override def zero: (A) => B = _ => b.zero
    }

  def bag[A](as : IndexedSeq[A]): Map[A,Int] =
    foldMapV(as,mapMergeMonoid[A,Int](intAddition))(x => Map(x -> 1))
}
