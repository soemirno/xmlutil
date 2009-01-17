package net.soemirno.xmlUtil

import xml.Utility.{trim, trimProper}
import _root_.scala.xml._
import _root_.scala.xml.transform.{RuleTransformer, RewriteRule}

import org.specs.runner.{Runner, JUnit}
import org.specs.Specification

class mergeDuplicatesSpecTest extends Runner(mergeDuplicatesSpec) with JUnit

object mergeDuplicatesSpec extends Specification {

  def updateFigure(ipcFigure: Elem): NodeSeq = {

    val updateServiceBulletins = new RewriteRule {

      override def transform(n: Node): NodeSeq = n match {
        case e: Elem if (e).label == "item" =>
          new Elem(e.prefix, e.label,
            e.attributes,
            e.scope,
            normalizeServiceBulletins((trim(e) \ "_").toList) : _*)       
        case n => n
      }
    }

    val transformer = new RuleTransformer(updateServiceBulletins)
    transformer.transform(ipcFigure)
  }

  def normalizeServiceBulletins(list: List[Node]): List[Node] =
    {
      if (list.length == 1 || list.isEmpty) {
        list
      } else {
        if ((list.head.label == "sbcdata") && (list.head \ "sbc") == (list.tail.head \ "sbc")) {
          list.head :: normalizeServiceBulletins(list.tail.tail)
        } else {
          list.head :: normalizeServiceBulletins(list.tail)
        }
      }
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

  "normalized sb list has one sbcdata removed" in {
    val normalizedItem = normalizeServiceBulletins((trim(ipcItem) \ "_").toList)
    normalizedItem.size must_== 5
  }

  "sb is replaced in item" in {
    Console.println(new PrettyPrinter(80, 2).format(<root> {updateFigure(ipcItem)} </root>))
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