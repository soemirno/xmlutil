package net.soemirno.xmlUtil

import _root_.scala.xml.transform.{RuleTransformer, RewriteRule}
import _root_.scala.xml.{Elem, NodeBuffer, NodeSeq}
import java.util.regex.Pattern.Node
import java.io.File
import org.specs.runner.{Runner, JUnit}
import org.specs.Specification
import org.slf4j.{Logger,LoggerFactory}
import scala.xml.parsing.ConstructingParser

class fixWiringManualSpecTest extends Runner(fixWiringManualSpec) with JUnit

object fixWiringManualSpec extends Specification {
    val LOGGER = LoggerFactory.getLogger(this.getClass)

    "should fix returned elements" in {
        val deletedKeys = findDeletedKeys("/Users/soemirno/Desktop/publication/50/wm/_shadowfolder/20081201-finalprint-ata2100.xml")
        val returnedElements = findReturnedElements("/Users/soemirno/Desktop/publication/50/wm/_unrevised.xml", deletedKeys)
        val sourceFile = "/Users/soemirno/Desktop/publication/50/wm/20090301.xml"
        val tempFile = "/Users/soemirno/Desktop/withAddedBreaks.xml"
        val outputFile = "/Users/soemirno/Desktop/fixed.xml" 

        LOGGER.info("Starting adding breaks: " + sourceFile)
        scala.xml.XML.saveFull(tempFile, AtaDocument.addLineBreakAfterGraphic(xml.XML.loadFile( sourceFile)), "UTF-8", true, null)
        LOGGER.info("Restoring graphics: " + tempFile)
        scala.xml.XML.saveFull(outputFile, AtaDocument.restoreRemovedGraphics(xml.XML.loadFile( tempFile),returnedElements), "UTF-8", true, null)
        LOGGER.info("Finished converting to: " + outputFile)

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

