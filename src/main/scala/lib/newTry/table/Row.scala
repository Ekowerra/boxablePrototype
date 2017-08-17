package lib.newTry.table

trait Row {
  val cells : Traversable[Cell]
  val height : Int
}
case class HeaderRow(cells : Traversable[Cell] = Seq[Cell](), height: Int = 1) extends Row
case class ContentRow(cells : Traversable[Cell] = Seq[Cell](), height: Int = 1) extends Row
