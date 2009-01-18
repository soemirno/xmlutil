package net.soemirno.xmlUtil

import xml.Utility.trim
import _root_.scala.xml._
import _root_.scala.xml.transform.{RuleTransformer, RewriteRule}

import org.specs.runner.{Runner, JUnit}
import org.specs.Specification

class mergeDuplicatesSpecTest extends Runner(mergeDuplicatesSpec) with JUnit

object mergeDuplicatesSpec extends Specification {

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
    val normalizedItem = Atadocument.listWithUniqueSb(ipcItem.child.toList)
    normalizedItem.size must_== 11
  }

  "sb is replaced in item" in {
    (AtaDocument.removeDuplicateSbFromFigure(ipcItem) \ "sbcdata").length must_== 1
  }

  "sb has effectivity of '097097 099099' " in {
    (AtaDocument.removeDuplicateSbFromFigure(ipcItem) \\ "sbcdata" \\ "@effrg").text must_== "097097 099099"
  }

  val listWithDuplicates = List(1, 1, 2, 2, 3, 4, 4)

  def normalizeList(list: List[int]): List[int] =
    {
      if (list.length == 1 || list.isEmpty)
        list
      else {
        if (list.head == list.tail.head) {
          list.head :: normalizeList(list.tail.tail)
        } else {
          list.head :: normalizeList(list.tail)
        }
      }
    }

  "normalized list contains 4 items" in {
    (normalizeList(listWithDuplicates)).size must_== 4
  }

  "normalized list has '3' as third element" in {
    (normalizeList(listWithDuplicates))(2) must_== 3
  }
}
