package net.soemirno.xmlUtil

import _root_.scala.xml.transform.{RuleTransformer, RewriteRule}
import _root_.scala.xml.{Elem, NodeSeq, Node}
import xml.Utility.trim

/**
 * Ata specific scripts
 */
object AtaDocument {
    /**
     * Convert figures with duplicate sb      
     */
    def removeDuplicateSbFromFigure(figure: Elem): Elem = {

        val n = new RuleTransformer(distinctSbRule).transform(figure)
        Elem(figure.prefix, figure.label, figure.attributes, figure.scope, n.first.child.toList: _*)

    }

    /**
     *  rewrite rule to ensure unique sb in element
     */
    val distinctSbRule = new RewriteRule {
        override def transform(n: Node) = n match {
            case e: Elem if (e.label == "item") =>
                Elem(e.prefix, e.label, e.attributes, e.scope, listWithUniqueSb(e.child.toList): _*)            
            case n => n
        }
    }

    /**
     * make sure no duplicate sb exists in list
     */
    def listWithUniqueSb(list: List[Node]): List[Node] = {
        if (list.length < 2)
            list
        else
            list.head :: listWithUniqueSb(trimmedSbTail(list))
    }

    /**
     * ignore any double sb on tail head
     */
    def trimmedSbTail(list: List[Node]) = {
        val trimmedTail = trimmedPcDataTail(list)

        val tailStartsWithSameSb = 
            list.head.label == "sbcdata" && !trimmedTail.isEmpty && (list.head \ "sbc") == (trimmedTail.head \ "sbc")

        if (tailStartsWithSameSb)
            trimmedTail.tail
        else
            list.tail
    }

    /**
     * ignore whitespace between elements
     */
    def trimmedPcDataTail(list: List[Node]) = {
        if (list.tail.head.label.equals("#PCDATA"))
            list.tail.tail
        else
            list.tail
    }

}
