package lib.newTry

import lib.newTry.iomonad.IO2b
import lib.newTry.iomonad.IO2b.{Return, Suspend, TailRec}
import lib.newTry.table._

object FactorialM {

  /*

Larger example using various monadic combinators. Sample run:

The Amazing Factorial REPL, v2.0
q - quit
<number> - compute the factorial of the given number
<anything else> - bomb with horrible error
3
factorial: 6
7
factorial: 5040
q

*/

  val helpstring = """
                     | The Amazing Factorial REPL, v2.0
                     | q - quit
                     | <number> - compute the factorial of the given number
                     | <anything else> - bomb with horrible error
                   """.trim.stripMargin

  def printLine(s: String): TailRec[Unit] =
    Suspend(() => Return(println(s)))

  def rdLine: TailRec[String] =
    Suspend(() => readLine)

  def factorial(n: Int): TailRec[Int] = for {
    start <- Suspend(() => 1)
    result <- TailRec.foldM(1 to n toStream)(start)((acc,i) => Return(acc* i))
  } yield result

  val factorialREPL: TailRec[Unit] = TailRec.sequence_(
    printLine(helpstring),
    TailRec.doWhile { rdLine } { line =>
      val ok = line != "q"
      TailRec.when (ok) { for {
        n <- factorial(line.toInt)
        _ <- printLine("factorial: " + n)
      } yield () }
    }
  )
}

object Main extends App {



  val tableConsole: Table = new Table {

    override def draw[R <: Row](rows: Traversable[R]): TailRec[Unit] = {

      def getCells(rowWithIndex : (R,Int)): List[(Cell,Int,Int)] = {
        rowWithIndex._1.cells.toList.zipWithIndex.map(c => (c._1, rowWithIndex._2, c._2))
      }

      def printCell(c : (Cell,Int,Int)): TailRec[Unit] = {
        Return(print("|"+c.toString))
      }

      def printRow(r: List[(Cell,Int,Int)]): TailRec[Unit] = {
        Suspend(() => r.foreach(c => printCell(c))).flatMap(_ => Return(print("|\n")))
      }


      val cells = rows.toList.zipWithIndex map getCells

      TailRec.foldM(cells.toStream)(())((_,x) => printRow(x))

    }
  }

  val tab1 = tableConsole.draw(
    List(
      HeaderRow(List(Cell("HeaderCell1"), Cell("HeaderCell2"))),
      ContentRow(List(Cell(), Cell(), Cell(), Cell(), Cell())),
      ContentRow(List(Cell(), Cell(), Cell(), Cell())),
      ContentRow(List(Cell(), Cell(), Cell())),
      ContentRow(List(Cell(), Cell())),
      ContentRow(List(Cell()))
    )
  )

  println("/**************************************************************************/")
  IO2b.run(tab1)

}
