package lib.newTry.table

import lib.newTry.iomonad.IO2b.TailRec

trait Table {
  def draw[R<:Row](rows : Traversable[R]) : TailRec[Unit]
}

/*
object Table {
  val table: Table =     /*override def draw[R <: Row](rows: Traversable[R]): Unit = {
      var table = ""
      var size = 0
      def go(c: Traversable[(Cell, Int)]) = {
        val listCells = c.map { cell => {
          c.filter(elem => elem._2 == cell._2).toList.zipWithIndex
        }
        }.foldLeft(List[List[((Cell, Int), Int)]]()) { (acc, elem) =>
          if (acc.contains(elem)) acc else acc :+ elem
        }.map {
          x => x.map(elem => (elem._1._1, elem._1._2, elem._2))
        }
        size = listCells.map(x => x.toString.length).max
        listCells.foreach { listTuple =>
          table+= "|"
          val space = Math.round((size - listTuple.toString.length)/ listTuple.length.toFloat /2F)
          listTuple.foreach(tuple => table+=(" "*space + tuple + " "*space + "|"))
          table+= "\n"
        }
      }

      go(rows.toList.zipWithIndex.flatMap(rowWithIndex => rowWithIndex._1.cells.map(cell => (cell, rowWithIndex._2))))
      rows foreach {
        case HeaderRow(_) => println("|" + " " * ((size -6)/ 2) + "Header" + " " * ((size-6) / 2) + "|")
        case _ => ()
      }
      println(table)
    }*/
}
*/
