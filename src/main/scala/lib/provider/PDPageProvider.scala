package lib.provider

import org.apache.pdfbox.pdmodel.{PDDocument, PDPage}

case class PDPageProvider(document : PDDocument, currentPage : PDPage) {

}
