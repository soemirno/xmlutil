package net.soemirno.xmlUtil

import xml.Utility.trim
import _root_.scala.xml._
import _root_.scala.xml.transform.{RuleTransformer, RewriteRule}
import scala.io.Source 

import org.specs.runner.{Runner, JUnit}
import org.specs.Specification
import org.slf4j.{Logger,LoggerFactory}
class mergeDuplicatesSpecTest extends Runner(mergeDuplicatesSpec) with JUnit

object mergeDuplicatesSpec extends Specification {
  val LOGGER = LoggerFactory.getLogger(this.getClass)

    
  val ipcItem: Elem =
  <item>
    <effect effrg=" "/>
    <pnrmfr>
      <pnr>A42513-701</pnr>
      <mfr>H0643</mfr>
    </pnrmfr>
    <upa>01</upa>
    <tqa>0001</tqa>
    <sbcdata>
      <effect effrg="097097 099099"/>
      <sbc chgcond="PO" chgnbr="SBF28-24-034" chgtype="sb"/>
    </sbcdata>
    <sbcdata>
      <effect effrg="099099"/>
      <sbc chgcond="PO" chgnbr="SBF28-24-034" chgtype="sb"/>
    </sbcdata>
  </item>

  "sb list has duplicate sbcdata removed" in {
    val normalizedItem = AtaDocument.listWithUniqueSb(ipcItem.child.toList)
    normalizedItem.size must_== 11
  }

  "sb is replaced in item" in {
    (AtaDocument.removeDuplicateSbFromFigure(ipcItem) \ "sbcdata").length must_== 1
  }

  "sb has effectivity of '097097 099099' " in {
    (AtaDocument.removeDuplicateSbFromFigure(ipcItem) \\ "sbcdata" \\ "@effrg").text must_== "097097 099099"
  }

  def convertFile(filename :String) = {
      LOGGER.info("Starting converting: " + filename)
      if (new java.io.File("/tmp/new/" + filename).exists) {

          val loadnode = xml.XML.loadFile("/tmp/new/" + filename)
          scala.xml.XML.saveFull("/tmp/temp/" + filename, loadnode, "UTF-8", true, null)

          val loadReversNode = xml.XML.loadFile("/tmp/temp/" + filename)

          scala.xml.XML.saveFull("/tmp/converted/" + filename, AtaDocument.removeDuplicateSbFromFigure(loadReversNode), "UTF-8", true, null)

          LOGGER.info("Finished converting: " + filename)
      } else
          LOGGER.error(filename + " does not exists")
  }

}
