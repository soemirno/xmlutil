package net.soemirno.xmlUtil

import xml.Utility.trim
import _root_.scala.xml._
import _root_.scala.xml.transform.{RuleTransformer, RewriteRule}

import org.specs.runner.{Runner, JUnit}
import org.specs.Specification

class mergeDuplicatesSpecTest extends Runner(mergeDuplicatesSpec) with JUnit

object mergeDuplicatesSpec extends Specification {

  def updateFigure(ipcFigure: Elem): NodeSeq = {
    new RuleTransformer(distinctSbRule).transform(ipcFigure)
  }

  val distinctSbRule = new RewriteRule {
    override def transform(n: Node): NodeSeq = n match {
      case e :Elem if (e.label == "item") => removeDuplicateSbFrom(e)
      case n => n
    }
  }
  
  def removeDuplicateSbFrom(elem: Elem) = {
    val childs = listWithUniqueSb(elem.child.toList)
    Elem(elem.prefix, elem.label, elem.attributes, elem.scope, childs : _*)
  }

  def listWithUniqueSb(list: List[Node]): List[Node] = {
    if (list.length < 2)
      list
    else {
      list.head :: listWithUniqueSb(trimmedSbTail(list))
    }  
  }

  def trimmedSbTail(list: List[Node]) = {
    val trimmedTail = trimmedPcDataTail(list)
    if ((list.head.label == "sbcdata") && (list.head \ "sbc") == (trimmedTail.head \ "sbc"))
      trimmedTail.tail
    else
      list.tail
  }

  def trimmedPcDataTail(list: List[Node]) = {
    if (list.tail.head.label.equals("#PCDATA"))
      list.tail.tail
    else
      list.tail
  }

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
    val normalizedItem = listWithUniqueSb(ipcItem.child.toList)
    normalizedItem.size must_== 11
  }

  "sb is replaced in item" in {
    (updateFigure(ipcItem) \ "sbcdata").length must_== 1
  }

  "sb has effectivity of '097097 099099' " in {
    (updateFigure(ipcItem) \\ "sbcdata" \\ "@effrg").text must_== "097097 099099"
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
