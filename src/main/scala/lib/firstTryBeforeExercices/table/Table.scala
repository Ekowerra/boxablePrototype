package lib.firstTryBeforeExercices.table

import lib.firstTryBeforeExercices.provider.{Margin, PDPageProvider}

case class Table(pageProvider: PDPageProvider,
                 margin : Margin,
                 yStart : Float)(fg :(Traversable[Row]) => (Traversable[(Cell, Int)]) => (Cell, Int, Int)) {

  def draw(rows : Traversable[Row]): (Cell, Int, Int) = {
    fg(rows)(rows.toList.zipWithIndex.flatMap(rowWithIndex => rowWithIndex._1.cells.map(cell => (cell, rowWithIndex._2))))
  }

}

object Table {
  def apply(pageProvider: PDPageProvider): Table = {

    def f(r : Traversable[Row]) = {
     // r foreach println
      g _
    }

    def g(c : Traversable[(Cell, Int)] = List[(Cell, Int)]((Cell(),0))) = {
      c.map {cell => {
        c.filter(elem => elem._2 == cell._2).toList.zipWithIndex
      } }.foldLeft(List[List[((Cell,Int),Int)]]()) { (acc, elem) =>
        if (acc.contains(elem)) acc else acc :+ elem
      }.map {
        x => x.map(elem => (elem._1._1, elem._1._2, elem._2))
      }.foreach{listTuple =>
        print("|")
        listTuple.foreach(tuple => print(" " + tuple + " |"))
        println("")
      }
      (c.head._1, c.head._2, 0)
    }

    new Table(pageProvider,Margin(0,0,0,0),0)(f)

  }
}
