package net.soemirno.xmlUtil


import _root_.scala.xml.transform.{RuleTransformer, RewriteRule}
import _root_.scala.xml.{Elem, NodeSeq, Node}
import xml.Utility.trim

class AtaDocument (source: Elem) extends Elem(source.prefix, source.label,
  source.attributes, source.scope, source.child : _*) {
}

object AtaDocument {
  def updateFigure(figure: Elem): Elem = {
    val n = new RuleTransformer(distinctSbRule).transform(figure)
    Elem(figure.prefix, figure.label, figure.attributes, figure.scope, n.first.child.toList : _*)

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
    if ((list.head.label == "sbcdata")&& (!trimmedTail.isEmpty) && (list.head \ "sbc") == (trimmedTail.head \ "sbc"))
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

}
