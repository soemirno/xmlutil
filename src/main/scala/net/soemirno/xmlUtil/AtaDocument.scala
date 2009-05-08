package net.soemirno.xmlUtil

import _root_.scala.xml._
import _root_.scala.xml.PCData
import _root_.scala.xml.transform.{RuleTransformer, BasicTransformer, RewriteRule}
import org.slf4j.{Logger, LoggerFactory}
import xml.Utility.trim
import scala.collection.mutable.Map

class CustomRuleTransformer(rules: RewriteRule*) extends CustomBasicTransformer {
  override def transform(n: Node): Seq[Node] = {
    var m: Seq[Node] = super.transform(n)
    val it = rules.elements; while (it.hasNext) {
      val rule = it.next
      val m2 = rule.transform(m)
      m = m2
    }
    m
  }
}

object WmFixer {
    val logger = LoggerFactory.getLogger(this.getClass)
    def main(args: Array[String]) {
        if (args.size != 4) {
          Console.println("Invalid number of arguments.")
          Console.println("java -Xmx2048m -jar xmlUtil-1.0.0-jar-with-dependencies.jar /pub/folder 50 20090601 20090301")
          return
        }
        val pub_folder = args(0)
        val aircraft = args(1)
        val revision =  args(2)
        val previous_revision = args(3)

        logger.info("Start fixing wm document for F" + aircraft)
        logger.info("Publication folder is: " + pub_folder)
        logger.info("Revision is: " + revision)
        logger.info("Previous revison is: " + previous_revision)

        val deletedKeys = AtaDocument.findDeletedKeys(xml.XML.loadFile(pub_folder + aircraft + "/wm/_shadowfolder/" + previous_revision + "-finalprint-ata2100.xml"), "graphic")
        val deletedSheetKeys = AtaDocument.findDeletedKeys(xml.XML.loadFile(pub_folder + aircraft + "/wm/_shadowfolder/" + previous_revision + "-finalprint-ata2100.xml"), "sheet")

        val returnedElements = AtaDocument.findReturnedElements(xml.XML.loadFile(pub_folder + aircraft +  "/wm/_unrevised.xml"), deletedKeys)
        val returnedSheetElements = AtaDocument.findReturnedSheetElements(xml.XML.loadFile(pub_folder + aircraft +  "/wm/_unrevised.xml"), deletedSheetKeys)

        val sourceFile = pub_folder + aircraft + "/wm/" + revision + ".xml"
        val outputFile = pub_folder + "/fixed.xml"

        val fixedReturnedSheetElements = AtaDocument.fixReturnedSheetElements(xml.XML.loadFile(sourceFile), returnedSheetElements )

        logger.info("Restoring graphics: " + sourceFile)
        scala.xml.XML.saveFull(outputFile, AtaDocument.restoreRemovedGraphics(xml.XML.loadFile( sourceFile),returnedElements, fixedReturnedSheetElements), "UTF-8", true, null)
        logger.info("Finished converting to: " + outputFile)    }
  }
/**
 * Ata specific scripts
 */
object AtaDocument {
  val logger = LoggerFactory.getLogger(this.getClass)

  def findDeletedKeys(previousRevision :Elem, label: String): Seq[String] = {
    logger.info("retrieving previous deleted " + label + "s")
    for {
      graphic <- previousRevision\\label
      if graphic\"@chg" == "D"
    } yield (graphic \ "@key").text
  }

  def findReturnedElements(unrevised :Elem, deletedKeys :Seq[String]): NodeSeq = {
    logger.info("finding returned element")
    for {
      returned <- unrevised\\"graphic"

      matchedKeys = for {
        key <- returned\\"@key"
        if deletedKeys.contains(key.text)
      } yield key

      if (matchedKeys.length > 0)
    } yield returned
  }

  def findReturnedSheetElements(unrevised :Elem, deletedKeys :Seq[String]): NodeSeq ={
        logger.info("finding returned sheet element")
        for (val returned <- unrevised \\ "graphic"
             if (deletedKeys.contains((returned \"sheet" \"@key").text)
                     || (deletedKeys.contains(((returned \"sheet").first \ "@key").text))
                     || ((returned \"sheet").length == 2 && deletedKeys.contains(((returned \"sheet")(1) \ "@key").text ))))
        yield (returned)

    }

  def fixReturnedSheetElements( revised: Elem, returnedSheetElements : NodeSeq): Map [String, Node] = {
    logger.info("fixing returned sheets")
    val graphicKeys = for ( graphic <- returnedSheetElements) yield (graphic \ "@key").text
    val revisedGraphics = scala.collection.mutable.Map [String, Node]()

    for {
      graphic <-  revised \\ "graphic"
      key = (graphic \ "@key").text
      if (graphicKeys.contains(key))
    } revisedGraphics += (key -> graphic)

    val fixed = scala.collection.mutable.Map [String, Node]()
    for (graphic <- returnedSheetElements){
      val key = (graphic\"@key").text
      logger.info("fixing graphic " + key)
      fixed += (key -> fillRevisionData(graphic, revisedGraphics(key)))
    }    
    fixed
  }

  def fillRevisionData(graphic :Node, revisedGraphic : Node): Node = {

    val fillRevisionDataRule = new RewriteRule {
      override def transform(n: Node) = n match {
        case e: Elem if (e.label == "sheet") =>  {
          val key = (e\"@key").text

          val lookupSheet = try {
            (for (sheet <- revisedGraphic\"sheet" if (sheet\"@key").text == key) yield sheet).first
          } catch {
            case e: Exception => {
              logger.info("sheet was deleted in previous revision. key " + key)
              null
            }
          }
          val attributes = if (lookupSheet != null){
           e.attributes.remove("chg"). remove("revdate").
              append(new UnprefixedAttribute("chg", lookupSheet \"@chg", xml.Null)).
              append(new UnprefixedAttribute("revdate",lookupSheet \"@revdate", xml.Null))
          } else {
            logger.info("found sheet with key " + key)
            e.attributes.remove("chg").remove("revdate").
              append(new UnprefixedAttribute("chg", "N", xml.Null)).
              append(new UnprefixedAttribute("revdate", "20090301", xml.Null))
          }
          Elem(e.prefix, e.label, attributes, e.scope, e.child: _*)
        }
        case _ => n
      }
    }
    val sheets: NodeSeq = for {
      sheet: Node <- graphic\"sheet"
    } yield (new RuleTransformer(fillRevisionDataRule).transform(sheet)).first

    val attributes = revisedGraphic.attributes.remove("chg").remove("revdate").
      append(new UnprefixedAttribute("chg",revisedGraphic \"@chg", xml.Null)).
      append(new UnprefixedAttribute("revdate",revisedGraphic \"@revdate", xml.Null))

    val children :NodeSeq = (Seq((graphic\"effect").first, (graphic\"title").first)) ++ sheets
    Elem(graphic.prefix, graphic.label, attributes, graphic.scope, children : _*)
  }

  def restoreRemovedGraphics(wm: Node, returnedElements :NodeSeq, returnedSheetElements :scala.collection.mutable.Map [String, Node]): Elem = {

    val restoreRemovedRule = new RewriteRule {
      override def transform(n: Node) = n match {
        case e: Elem if (e.label == "graphic" && returnedSheetElements.keySet.contains((e\"@key").text)) =>
          returnedSheetElements((e\"@key").text)

        case e: Elem if (e.label == "effect" && (e\"sheet").length > 0) => {
          logger.info("fix effect element for graphic")
          val effect :Elem = Elem(e.prefix, e.label, e.attributes, e.scope, NodeSeq.Empty : _*)
          val child :NodeSeq =  effect :: e.child.toList.tail
          val sheet = {
            if ((e\"sheet").length == 1)
              e\"sheet"
            else
              (e\"sheet").first
          }

          logger.info("looking for parent of sheet " + (sheet \"@key").text)
          val parent = findGraphic(returnedElements.toList, sheet).first
          logger.info("found parent " + (parent \"@key").text)

          val attributes = parent.attributes.remove("chg").remove("revdate").
            append(new UnprefixedAttribute("chg",sheet \"@chg", xml.Null)).
            append(new UnprefixedAttribute("revdate",sheet \"@revdate", xml.Null))
          Elem(parent.prefix, parent.label, attributes, e.scope, child : _*)
        }

        case _ => n
       }
    }

    val n = new CustomRuleTransformer(restoreRemovedRule).transform(wm)
    Elem(wm.prefix, wm.label, wm.attributes, wm.scope, n.first.child.toList: _*)

  }

  def findGraphic(list :List[Node], sheet :NodeSeq ):List[Node] = {
    val head = try {
      list.head
    } catch {
         case e: Exception => {
            Console.println(list)
            Console.println(sheet)
            throw e
      }
    }
    val sheetSeq = (head\"sheet")
    val compareSheet = {
      if (sheetSeq.length == 1)
        sheetSeq
      else
        sheetSeq.first
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
        if (list.length <= 1) return list

        list.head :: listWithUniqueSb(trimmedSbTail(list))
    }

    /**
     * ignore any double sb on tail head
     */
    def trimmedSbTail(list: List[Node]) = {
        val trimmedTail = trimmedPcDataTail(list)

        if (startsWithSameSb(list, trimmedTail)) {
            logChanges(list.head, trimmedTail.head)
            trimmedTail.tail
        }
        else
            list.tail
    }

    def startsWithSameSb(firstList: List[Node], secondList: List[Node]) = {
      firstList.head.label == "sbcdata" && !secondList.isEmpty &&
              (firstList.head \ "sbc" \ "@chgnbr") == (secondList.head \ "sbc" \ "@chgnbr")
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
