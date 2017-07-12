import lib.provider.{Margin, PDPageProvider}
import lib.table.{Cell, Row, Table}
import org.apache.pdfbox.pdmodel.common.PDRectangle
import org.apache.pdfbox.pdmodel.{PDDocument, PDPage}

object Main extends App {

  val doc = new PDDocument()
  val page = new PDPage(PDRectangle.A4)
  doc.addPage(page)
  val pageProvider = PDPageProvider(doc, page)
  val rows : Traversable[Row] = List(Row(List(Cell(),Cell(),Cell(),Cell())),
                                     Row(List(Cell(),Cell(),Cell(),Cell())),
                                     Row(List(Cell(),Cell())))
  Table(pageProvider).draw(rows)
}
