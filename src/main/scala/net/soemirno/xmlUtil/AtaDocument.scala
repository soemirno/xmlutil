package net.soemirno.xmlUtil

import _root_.scala.xml.transform.{RuleTransformer, RewriteRule}
import _root_.scala.xml.{Elem, NodeSeq, Node, PrettyPrinter}
import org.slf4j.{Logger, LoggerFactory}
import xml.Utility.trim

/**
 * Ata specific scripts
 */
object AtaDocument {
    val logger = LoggerFactory.getLogger(this.getClass)

    def restoreRemovedGraphics(wm: Elem): Elem = {

        val n = new RuleTransformer(restoreRemovedRule).transform(wm)
        Elem(wm.prefix, wm.label, wm.attributes, wm.scope, n.first.child.toList: _*)

    }

    /**
     *  rewrite rule to ensure unique sb in element
     */
    val restoreRemovedRule = new RewriteRule {
        override def transform(n: Node) = n match {
            case e: Elem if (e.label == "effect" && e.child.length>1 && e.child(1).label == "title") => {
                val effect = Elem(e.prefix, e.label, e.attributes, e.scope, null : _*)
                val child = e.child.toList.tail
                Elem(null, "graphic", null, e.scope, child : _*)
            }
            case n => n
        }
    }

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
            case e: Elem if (e.label == "item") => {
                val filteredList = listWithUniqueSb(e.child.toList)
                if (filteredList.length != e.child.toList.length)
                    logger.warn("modified item with key: " + (e \\ "@key").text)
                Elem(e.prefix, e.label, e.attributes, e.scope, filteredList: _*)
            }
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
        list.head.label == "sbcdata" && !trimmedTail.isEmpty &&
                (list.head \ "sbc" \ "@chgnbr") == (trimmedTail.head \ "sbc" \ "@chgnbr")

        if (tailStartsWithSameSb) {
            logChanges(list.head, trimmedTail.head)
            trimmedTail.tail
        }
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

    def logChanges(nodeKept: Node, nodeDropped: Node) = {
        val effect1 = (nodeKept \ "effect" \ "@effrg").text
        val effect2 = (nodeDropped \ "effect" \ "@effrg").text
        if (!effect1.equals(effect2)) {
            logger.warn("different effectivity")
            logger.warn("keeping:")
            logger.warn((nodeKept \ "sbc" \ "@chgnbr").text)
            logger.warn(effect1)
            logger.warn("removing:")
            logger.warn((nodeDropped \ "sbc" \ "@chgnbr").text)
            logger.warn(effect2)
        } else {
            logger.info("keeping:")
            logger.info((nodeKept \ "sbc" \ "@chgnbr").text)
            logger.info(effect1)
            logger.info("removing:")
            logger.info((nodeDropped \ "sbc" \ "@chgnbr").text)
            logger.info(effect2)
        }
    }

}
