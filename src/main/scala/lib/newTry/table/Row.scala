package lib.newTry.table

trait Row {
  val cells : Traversable[Cell]
}
case class HeaderRow(cells : Traversable[Cell] = Seq[Cell]()) extends Row
case class ContentRow(cells : Traversable[Cell] = Seq[Cell]()) extends Row
