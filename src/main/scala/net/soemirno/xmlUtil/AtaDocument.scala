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

    def findDeletedKeys(previousRevision :Elem): Seq[String] = {
        logger.info("retrieving previous deleted graphics")
        for( val graphic <- previousRevision \\ "graphic" if graphic \ "@chg" == "D" )
            yield (graphic \ "@key").text
    }

  def findReturnedElements(unrevised :Elem, deletedKeys :Seq[String]): NodeSeq ={
      logger.info("finding returned element")
      for (val returned <- unrevised \\ "graphic" if (deletedKeys.contains((returned \"@key").text)))
          yield (returned)
  }

    def restoreRemovedGraphics(wm: Node, returnedElements :NodeSeq, returnedSheetElements :scala.collection.mutable.Map [String, Node]): Elem = {

        val restoreRemovedRule = new RewriteRule {
            override def transform(n: Node) = n match {
               case e: Elem if (e.label == "graphic" && returnedSheetElements.keySet.contains((e \ "@key").text)) =>{
                  returnedSheetElements((e \ "@key").text)
               }
               case e: Elem if (e.label == "effect" && (e \ "sheet").length > 0) =>{
                   logger.info("fix effect element for graphic")
                   val effect :Elem = Elem(e.prefix, e.label, e.attributes, e.scope, NodeSeq.Empty : _*)
                   val child :NodeSeq =  effect :: e.child.toList.tail
                   val sheet = {
                        if ((e \ "sheet").length == 1 )
                            (e \"sheet")
                        else
                            (e \"sheet").first
                   }
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

    def findSheet(list :List[Node], effect :NodeSeq ):List[Node] ={
        val head =
        try {
              list.head
        } catch {
             case e: Exception =>{
                Console.println(list)
                Console.println(effect)
                throw e
             }
        }

        if ((head \ "title").text == (effect \  "title").text && (head \ "effect" \ "@effrg").text == (effect \  "@effrg").text){
            if ((head \"effect" \ "sbeff").length == 1 ){
                if ( (effect \ "sbeff" \ "@effrg").text != (head \"effect" \ "sbeff" \ "@effrg").text){
                    return findSheet(list.tail, effect)
                } else {
                    return List(head)                    
                }
            }
            if ((head \"effect" \ "sbeff").length > 1 ){
                val sbnbr = ((head \"effect" \ "sbeff").first \ "@sbnbr").text
                if ( !(effect \ "sbeff" \"@sbnbr" ).text.equals(sbnbr)){
                    return findSheet(list.tail, effect)
                } else {
                    return List(head)
                }
            }

            return List(head)
        }
        else
            return findSheet(list.tail, effect)        
    }
    def findGraphic(list :List[Node], sheet :NodeSeq ):List[Node] ={
        val head =
        try {
              list.head
        } catch {
             case e: Exception =>{
                Console.println(list)
                Console.println(sheet)
                throw e
             }
        }
        val compareSheet = {
            if ((head \ "sheet" ). length == 1) 
                (head \ "sheet" )
            else
                (head \ "sheet" ).first
        }
        if ((compareSheet \ "@schemnbr").text == (sheet \"@schemnbr").text
                && (compareSheet \ "@pagenbr").text == (sheet \"@pagenbr").text
                && (compareSheet \ "@chapnbr").text == (sheet \"@chapnbr").text
                && (compareSheet \ "@sectnbr").text == (sheet \"@sectnbr").text
                && (compareSheet \ "@subjnbr").text == (sheet \"@subjnbr").text
        )
            List(head)
        else {
            findGraphic(list.tail, sheet)
        }
    }

    def addLineBreakAfterGraphic(wm: Node): Elem = {
        val break :Node = Text("\n")
        val addLineBreakAfterGraphicRule = new RewriteRule {

            val space :Node =
    (<root> <test></test> <test></test></root>).child.toList.first

            override def transform(n: Node) = n match {
                case e: Elem if (e.label == "title") => {
                    Elem(e.prefix, e.label, e.attributes, e.scope, e.child: _*)
                }

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
