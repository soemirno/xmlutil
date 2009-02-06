package net.soemirno.xmlUtil

import _root_.scala.xml._
import _root_.scala.xml.transform.{RuleTransformer, RewriteRule}
import java.io.File
import org.specs.runner.JUnit4
import org.specs.Specification
import org.slf4j.{Logger,LoggerFactory}
import scala.xml.parsing.ConstructingParser

class fixWiringManualSpecTest extends JUnit4(fixWiringManualSpec)

object fixWiringManualSpec extends Specification {
    val LOGGER = LoggerFactory.getLogger(this.getClass)

//    "should fix returned elements" in {

//        val pub_folder = "/Users/soemirno/Desktop/publication/"
//        val aircraft = "100"
//        val revision =  "20090301"
//        val previous_revision =  "20081201"
//        val deletedKeys = findDeletedKeys(pub_folder + aircraft + "/wm/_shadowfolder/" + previous_revision + "-finalprint-ata2100.xml")
//        val deletedSheetKeys = findDeletedSheetKeys(pub_folder + aircraft + "/wm/_shadowfolder/" + previous_revision + "-finalprint-ata2100.xml")
//        val returnedElements = findReturnedElements(pub_folder + aircraft +  "/wm/_unrevised.xml", deletedKeys)
//        val returnedSheetElements = findReturnedSheetElements(pub_folder + aircraft +  "/wm/_unrevised.xml", deletedSheetKeys)
//        val sourceFile = pub_folder + aircraft + "/wm/" + revision + ".xml"
//        val tempFile = pub_folder + "/withAddedBreaks.xml"
//        val outputFile = pub_folder + "/fixed.xml"
//
//        val fixedReturnedSheetElements = fixReturnedSheetElements(sourceFile, returnedSheetElements )
//
//        LOGGER.info("Starting adding breaks: " + sourceFile)
//        scala.xml.XML.saveFull(tempFile, AtaDocument.addLineBreakAfterGraphic(xml.XML.loadFile( sourceFile)), "UTF-8", true, null)
//        LOGGER.info("Restoring graphics: " + tempFile)
//        scala.xml.XML.saveFull(outputFile, AtaDocument.restoreRemovedGraphics(xml.XML.loadFile( tempFile),returnedElements, fixedReturnedSheetElements), "UTF-8", true, null)
//        LOGGER.info("Finished converting to: " + outputFile)

//    }

  val previousRevision = <root><graphic key="W0428653" chg="D" revdate="20081201" chapnbr="22" gtype="SS" pagenbr="115" schemnbr="01" sectnbr="11" subjnbr="27"><revst/><deleted/><revend/></graphic><graphic key="W0290526" chg="R" revdate="20081201" chapnbr="22" gtype="SS" pagenbr="117" sectnbr="11" subjnbr="11"><effect effrg="563563 571571 573573 575575"/><title>FLIGHT CONT CMPTR SYSTEM DATA BUS I/O SYS 1</title><sheet key="W0290527" chg="R" revdate="20081201" chapnbr="22" gnbr="21221111_005_-" gtype="SS" pagenbr="117" sectnbr="11" sheetnbr="01" subjnbr="11"><effect><sbeff effrg="563563 571571 573573 575575" sbcond="POST" sbnbr="SBF100-34-042"/></effect><title>21221111/005/-</title></sheet></graphic></root>

  val unrevised = <root><graphic chapnbr="22" chg="x" gtype="SS" key="W0291653" pagenbr="115" revdate="TODOTODO" sectnbr="11" subjnbr="27"><effect effrg="540541 543543 556556 558558" />
<title>FLIGHT CONT CMPTR SYSTEM DISCRETE I/O SYS 1</title><sheet chapnbr="22" chg="x" gnbr="d40501_175_ax" gtype="SS" key="W0291654" pagenbr="115" revdate="TODOTODO" sectnbr="11" sheetnbr="01" subjnbr="27"><effect><sbeff effrg="540541 543543 556556 558558" sbcond="POST" sbnbr="SBF100-34-042" /></effect>
<title>D40501/175/AX</title></sheet></graphic><graphic chapnbr="22" chg="x" gtype="SS" key="W0428653" pagenbr="115" revdate="TODOTODO" schemnbr="01" sectnbr="11" subjnbr="27"><effect effrg="540541 543543 556556 558558" />
<title>FLIGHT CONT CMPTR SYSTEM DISCRETE I/O SYS 1</title><sheet chapnbr="22" chg="x" gnbr="d40501_144_al" gtype="SS" key="W0428654" pagenbr="115" revdate="TODOTODO" schemnbr="01" sectnbr="11" sheetnbr="01" subjnbr="27"><effect><sbeff effrg="540541 543543 556556 558558" sbcond="POST" sbnbr="SBF100-22-051" /></effect>
<title>D40501/144/AL</title></sheet></graphic><graphic chapnbr="22" chg="x" gtype="SS" key="W0291655" pagenbr="116" revdate="TODOTODO" sectnbr="11" subjnbr="27"><effect effrg="563563 570570 574574" />
<title>FLIGHT CONT CMPTR SYSTEM DISCRETE I/O SYS 1</title><sheet chapnbr="22" chg="x" gnbr="21221127_003_a" gtype="SS" key="W0291656" pagenbr="116" revdate="TODOTODO" sectnbr="11" sheetnbr="01" subjnbr="27"><effect><sbeff effrg="570570 574574" sbcond="POST" sbnbr="FAS-N311" /><sbeff effrg="563563" sbcond="POST" sbnbr="FS-N558B" /></effect>
<title>21221127/003/A</title></sheet></graphic></root>

  val revised = <root><graphic key="W0291653" chg="R" revdate="20090301" chapnbr="22" gtype="SS" pagenbr="115" sectnbr="11" subjnbr="27"><effect effrg="540541 543543 556556 558558" />
<title>FLIGHT CONT CMPTR SYSTEM DISCRETE I/O SYS 1</title><sheet key="W0291654" chg="R" revdate="20090301" chapnbr="22" gnbr="d40501_175_ax" gtype="SS" pagenbr="115" sectnbr="11" sheetnbr="01" subjnbr="27"><effect><sbeff effrg="540541 543543 556556 558558" sbcond="POST" sbnbr="SBF100-34-042" /></effect>
<title>D40501/175/AX</title></sheet></graphic><effect effrg="540541 543543 556556 558558">
<title>FLIGHT CONT CMPTR SYSTEM DISCRETE I/O SYS 1</title><sheet key="W0428654" chg="N" revdate="20090301" chapnbr="22" gnbr="d40501_144_al" gtype="SS" pagenbr="115" schemnbr="01" sectnbr="11" sheetnbr="01" subjnbr="27"><effect><sbeff effrg="540541 543543 556556 558558" sbcond="POST" sbnbr="SBF100-22-051" /></effect>
<title>D40501/144/AL</title></sheet></effect><graphic key="W0291655" chg="R" revdate="20090301" chapnbr="22" gtype="SS" pagenbr="116" sectnbr="11" subjnbr="27"><effect effrg="563563 570570 574574" />
<title>FLIGHT CONT CMPTR SYSTEM DISCRETE I/O SYS 1</title><sheet key="W0291656" chg="R" revdate="20090301" chapnbr="22" gnbr="21221127_003_a" gtype="SS" pagenbr="116" sectnbr="11" sheetnbr="01" subjnbr="27"><effect><sbeff effrg="570570 574574" sbcond="POST" sbnbr="FAS-N311" /><sbeff effrg="563563" sbcond="POST" sbnbr="FS-N558B" /></effect>
<title>21221127/003/A</title></sheet></graphic></root>
  
  "should find deleted keys" in {
    val keys = AtaDocument.findDeletedKeys(previousRevision)
    keys.length must_== 1
    keys(0) must_== "W0428653"
  }

  "should find returned graphics" in {
    val keys = AtaDocument.findDeletedKeys(previousRevision)
    val returnedGraphics = AtaDocument.findReturnedElements(unrevised, keys)
    returnedGraphics.length must_== 1
    (returnedGraphics(0) \ "@key").text must_== "W0428653"
  }

  "should fix returned graphics" in {
    val keys = AtaDocument.findDeletedKeys(previousRevision)
    val returnedGraphics = AtaDocument.findReturnedElements(unrevised, keys)
    val fixedGraphics =AtaDocument.restoreRemovedGraphics(revised, returnedGraphics, scala.collection.mutable.Map [String, Node]())
    returnedGraphics.length must_== 1
    (returnedGraphics(0) \ "@key").text must_== "W0428653"
    Console.println(fixedGraphics)
  }

    def fixReturnedSheetElements( revised: String, returnedSheetElements : NodeSeq): scala.collection.mutable.Map [String, Node] = {


      LOGGER.info("fixing returned sheets")
      val graphicKeys = for ( graphic <- returnedSheetElements) yield (graphic \ "@key").text
      val revisedGraphics = scala.collection.mutable.Map [String, Node]()
      for (  graphic <-  xml.XML.loadFile(revised) \\ "graphic" if (graphicKeys.contains( (graphic \ "@key").text)) ) {
        revisedGraphics += ((graphic \ "@key").text -> graphic)              
      }

      val fixed = scala.collection.mutable.Map [String, Node]()
      for (graphic <- returnedSheetElements){
          fixed += ((graphic \ "@key").text -> fillRevisionData(graphic, revisedGraphics((graphic \ "@key").text)))
      }
      return fixed
    }

    def fillRevisionData(graphic :Node, revisedGraphic : Node): Node = {


      val fillRevisionDataRule = new RewriteRule {
        override def transform(n: Node) = n match {
          case e: Elem if (e.label == "sheet") =>  {
            val key =  (e \ "@key").text
            LOGGER.info("looking for key " + key)

            val lookupSheet = try {
              {for (sheet <- revisedGraphic \ "sheet" if (sheet \ "@key").text == key) yield sheet}.first
            }
             catch {
              case e: Exception =>null
            }

            if (lookupSheet != null){
              LOGGER.info("found key " + key)
              val change = new UnprefixedAttribute("chg",lookupSheet \"@chg", xml.Null)
              val revdate = new UnprefixedAttribute("revdate",lookupSheet \"@revdate", xml.Null)
              val attributes = e.attributes.
                                      remove("chg").
                                      remove("revdate").
                                      append(change).
                                      append(revdate)

              Elem(e.prefix, e.label, attributes, e.scope, e.child: _*)
            }
            else{
              val change = new UnprefixedAttribute("chg", "N", xml.Null)
              val revdate = new UnprefixedAttribute("revdate", "20090301", xml.Null)
              val attributes = e.attributes.
                                      remove("chg").
                                      remove("revdate").
                                      append(change).
                                      append(revdate)

              Elem(e.prefix, e.label, attributes, e.scope, e.child: _*)
              }
          }
          case _ => n

        }
      }  

      val sheets: NodeSeq = for (sheet: Node <- graphic \ "sheet")
            yield (new RuleTransformer(fillRevisionDataRule).transform(sheet)).first


      val change = new UnprefixedAttribute("chg",revisedGraphic \"@chg", xml.Null)
      val revdate = new UnprefixedAttribute("revdate",revisedGraphic \"@revdate", xml.Null)
      val attributes = revisedGraphic.attributes.
                              remove("chg").
                              remove("revdate").
                              append(change).
                              append(revdate)
      val children = (Seq( (graphic \ "effect").first, (graphic \ "title").first))++sheets
      Elem(graphic.prefix, graphic.label, attributes, graphic.scope, children : _*)
        

   }

    def findDeletedKeys(previousFinalPrint :String): Seq[String] = {
        LOGGER.info("retrieving previous deleted graphics")
        for( val graphic <- xml.XML.loadFile(previousFinalPrint) \\ "graphic" if graphic \ "@chg" == "D" )
            yield (graphic \ "@key").text

    }


    def findReturnedElements(unrevised :String, deletedKeys :Seq[String]): NodeSeq ={
        LOGGER.info("finding returned element")
        for (val returned <- xml.XML.loadFile(unrevised) \\ "graphic" if (deletedKeys.contains((returned \"@key").text)))
            yield (returned)

    }

    def findDeletedSheetKeys(previousFinalPrint :String): Seq[String] = {
        LOGGER.info("retrieving previous deleted sheets")
        for( val sheet <- xml.XML.loadFile(previousFinalPrint) \\ "sheet" if sheet \ "@chg" == "D" )
            yield (sheet \ "@key").text

    }

    def findReturnedSheetElements(unrevised :String, deletedKeys :Seq[String]): NodeSeq ={
        LOGGER.info("finding returned sheet element")
        for (val returned <- xml.XML.loadFile(unrevised) \\ "graphic"
             if (deletedKeys.contains((returned \"sheet" \"@key").text)
                     || (deletedKeys.contains(((returned \"sheet").first \ "@key").text))
                     || ((returned \"sheet").length == 2 && deletedKeys.contains(((returned \"sheet")(1) \ "@key").text ))))
        yield (returned)

    }
}

