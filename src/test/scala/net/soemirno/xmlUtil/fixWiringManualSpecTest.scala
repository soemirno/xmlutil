package net.soemirno.xmlUtil


import _root_.scala.xml.transform.{RuleTransformer, RewriteRule}
import java.util.regex.Pattern.Node
import _root_.scala.xml.{Elem, NodeSeq}
import org.specs.runner.{Runner, JUnit}
import org.specs.Specification
import org.slf4j.{Logger,LoggerFactory}

class fixWiringManualSpecTest extends Runner(fixWiringManualSpec) with JUnit

object fixWiringManualSpec extends Specification {
    val LOGGER = LoggerFactory.getLogger(this.getClass)

    "should read unrevised" in {
//        val deletedKeys = findDeletedKeys("/Users/soemirno/Desktop/publication/100/wm/_shadowfolder/20080901-finalprint-ata2100.xml")
//        val returnedElements = findReturnedElements("/Users/soemirno/Desktop/publication/100/wm/_unrevised.xml", deletedKeys)

        val filename = "/Users/soemirno/Desktop/publication/100/wm/20081201.xml"
        LOGGER.info("Starting fixing: " + filename)
        val wmDoc = xml.XML.loadFile( filename)

//val wmDoc =
//<subject>
//<graphic key="W0438532" chg="R" revdate="20081201" chapnbr="22" gtype="SS" pagenbr="159" sectnbr="11" subjnbr="23">
//<effect effrg="549549 559559" />
//<title>FLIGHT CONT CMPTR SYSTEM DISCRETE I/O SYS 1</title>
//<sheet key="W0438533" chg="R" revdate="20081201" chapnbr="22" gnbr="d40501_317_ce" gtype="SS" pagenbr="159" sectnbr="11" sheetnbr="01" subjnbr="23">
//<effect effrg="549549 559559" />
//<title>D40501/317/CE</title></sheet></graphic>
//<effect effrg="549549 559559">
//    <title>FLIGHT CONT CMPTR SYSTEM DISCRETE I/O SYS 1</title>
//    <sheet key="W0451848" chg="N" revdate="20081201" chapnbr="22" gnbr="21221123_041_b" gtype="SS" pagenbr="159" schemnbr="01" sectnbr="11" sheetnbr="01" subjnbr="23">
//    <effect>
//    <sbeff effrg="549549 559559" sbcond="POST" sbnbr="SBF100-22-050" />
//    </effect>
//<title>21221123/041/B</title></sheet>
//</effect>
//</subject>
        scala.xml.XML.saveFull( "/Users/soemirno/Desktop/fixed.xml",
            AtaDocument.restoreRemovedGraphics(wmDoc), "UTF-8", true, null)

//        Console.println((wmDoc \\ "effect"))


//        val distinctSbRule =  new RewriteRule {
//           override def transform(n: Node) = n match {
//              case e: Elem if (e \ "@chg" == "D") => {
//                  Console.println(e \ "@key")
//                  e
//                }
//              case n => n
//          }
//        }
//        val n = new RuleTransformer(distinctSbRule).transform(LOGGER.info("Finished converting: " + filename))


        LOGGER.info("Finished converting: " + filename)

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
}

