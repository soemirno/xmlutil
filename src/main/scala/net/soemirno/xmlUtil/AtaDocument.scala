package net.soemirno.xmlUtil

import _root_.scala.xml._
import _root_.scala.xml.PCData
import _root_.scala.xml.transform.{RuleTransformer, RewriteRule}
import org.slf4j.{Logger, LoggerFactory}
import xml.Utility.trim

/**
 * Ata specific scripts
 */
object AtaDocument {
    val logger = LoggerFactory.getLogger(this.getClass)

    def restoreRemovedGraphics(wm: Node, returnedElements :NodeSeq): Elem = {

        val restoreRemovedRule = new RewriteRule {
            override def transform(n: Node) = n match {
               case e: Elem if (e.label == "effect" && e.child.length>1 && e.child(1).label == "title") =>{
                   logger.info("fix effect element")
                   val effect :Elem = Elem(e.prefix, e.label, e.attributes, e.scope, NodeSeq.Empty : _*)
                   val child :NodeSeq =  effect :: e.child.toList.tail

                   val sheet = e \ "sheet"
                   logger.info("looking for parent of sheet " + (sheet \"@key").text)                   
                   val parent = findGraphic(returnedElements.toList, sheet ).first
                   logger.info("found parent " + (parent \"@key").text)

                   val change = new UnprefixedAttribute("chg",sheet \"@chg", xml.Null)
                   val revdate = new UnprefixedAttribute("revdate",sheet \"@revdate", xml.Null)
                   val attributes = parent.attributes.
                            remove("chg").
                            remove("revdate").
                            append(change).
                            append(revdate)

                   Elem(parent.prefix, parent.label, attributes, e.scope, child : _*)
                }
               case _ => n
            }
        }

        val n = new RuleTransformer(restoreRemovedRule).transform(wm)
        Elem(wm.prefix, wm.label, wm.attributes, wm.scope, n.first.child.toList: _*)

    }

    def findGraphic(list :List[Node], sheet :NodeSeq ):List[Node] ={
        val head = list.head
        if ((head \\ "sheet" \ "@schemnbr").text == (sheet \"@schemnbr").text
                && (head \\ "sheet" \ "@pagenbr").text == (sheet \"@pagenbr").text
                && (head \\ "sheet" \ "@chapnbr").text == (sheet \"@chapnbr").text
                && (head \\ "sheet" \ "@sectnbr").text == (sheet \"@sectnbr").text
                && (head \\ "sheet" \ "@subjnbr").text == (sheet \"@subjnbr").text
        )
            List(head)
        else
            findGraphic(list.tail, sheet)        
    }

    def addLineBreakAfterGraphic(wm: Node): Elem = {

        val addLineBreakAfterGraphicRule = new RewriteRule {
        val break :Node =
(<root>
<test></test>
<test></test>
</root>).child.toList.first
            override def transform(n: Node) = n match {                
                case e: Elem if (e.label == "graphic") => {
                    if ((e \ "@chapnbr").text > "20")
                        List(e, break)
                    else
                        e
                }
                case _ => n
            }
        }

        val n = new RuleTransformer(addLineBreakAfterGraphicRule).transform(wm)
        Elem(wm.prefix, wm.label, wm.attributes, wm.scope, n.first.child.toList: _*)
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
