package net.soemirno.xmlUtil


import _root_.scala.xml.transform.{RuleTransformer, RewriteRule}
import _root_.scala.xml.{Elem, NodeSeq, Node}
import xml.Utility.trim

class AtaDocument (source: Elem) extends Elem(source.prefix, source.label,
  source.attributes, source.scope, source.child : _*) {

  def normalized(): NodeSeq = {
    new RuleTransformer(removeDuplicateSbRule).transform(this)
  }

  val removeDuplicateSbRule = new RewriteRule {
    override def transform(n: Node): NodeSeq = n match {
      case e :Elem if (e.label == "item") => removeDuplicateSbFrom(e)
      case n => n
    }
  }

  def removeDuplicateSbFrom(elem: Elem) = {
    val childs = (trim(elem) \ "_").toList
    Elem(elem.prefix, elem.label, elem.attributes, elem.scope, duplicatesSbRemoved(childs) : _*)
  }

  def duplicatesSbRemoved(list: List[Node]): List[Node] = {
    if (list.isEmpty || list.tail.isEmpty)
      list
    else {
      val normalizedList = removeAnyDoubleSbHead(list);
      normalizedList.head :: duplicatesSbRemoved(normalizedList.tail)
    }
  }

  def removeAnyDoubleSbHead(list: List[Node]) = {
    val hasDoubleHead = (list.head.label == "sbcdata") && (list.head \ "sbc") == (list.tail.head \ "sbc")
    if (hasDoubleHead)
      list.tail
    else
      list
  }

}
