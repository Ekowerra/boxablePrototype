package lib.newTry

import lib.newTry.iomonad.IO2b
import lib.newTry.iomonad.IO2b.{Return, Suspend, TailRec}
import lib.newTry.table._


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

      println("coucou")
      TailRec.foldM(cells.toStream)(())((_,x) => printRow(x))

    }
  }

  val tab1 = tableConsole.draw(
    List(
      HeaderRow(List(Cell(), Cell(), Cell())),
      ContentRow(List(Cell(), Cell(), Cell())),
      ContentRow(List(Cell(), Cell(), Cell())),
      ContentRow(List(Cell(), Cell(), Cell())),
      ContentRow(List(Cell(), Cell(), Cell()))
    )
  )

  println("/**************************************************************************/")
  IO2b.run(tab1)
}
