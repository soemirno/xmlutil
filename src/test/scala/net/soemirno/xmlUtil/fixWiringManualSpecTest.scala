package net.soemirno.xmlUtil

import _root_.scala.xml._
import _root_.scala.xml.transform.{RuleTransformer, RewriteRule}
import java.io.File
import org.specs.runner.JUnit4
import org.specs.Specification
import org.slf4j.{Logger,LoggerFactory}
import scala.xml.parsing.ConstructingParser

class fixWiringManualSpecTest extends JUnit4(fixWiringManualSpec)

object fixWiringManualSpec extends Specification {
    val LOGGER = LoggerFactory.getLogger(this.getClass)

  val previousRevision = <root><graphic key="A5428653" chg="D" revdate="20081201" chapnbr="5" gtype="SS" pagenbr="115" schemnbr="01" sectnbr="3" subjnbr="27"><revst/><deleted/><revend/></graphic><graphic key="A5290526" chg="R" revdate="20081201" chapnbr="5" gtype="SS" pagenbr="117" sectnbr="3" subjnbr="11"><effect effrg="563563 571571 573573 575575"/><title>ANOTHER RANDOM TITLE</title><sheet key="A5290527" chg="R" revdate="20081201" chapnbr="5" gnbr="21221111_005_-" gtype="SS" pagenbr="117" sectnbr="3" sheetnbr="01" subjnbr="11"><effect><sbeff effrg="563563 571571 573573 575575" sbcond="POST" sbnbr="87654321"/></effect><title>2222222222</title></sheet></graphic></root>

  val unrevised = <root><dummy/><graphic chapnbr="5" chg="x" gtype="SS" key="A5291653" pagenbr="115" revdate="" sectnbr="3" subjnbr="7"><effect effrg="540541 543543 556556 558558" />
<title>SOME RANDOM TITLE</title><sheet chapnbr="5" chg="x" gnbr="34738_x" gtype="SS" key="A5291654" pagenbr="115" revdate="" sectnbr="3" sheetnbr="01" subjnbr="7"><effect><sbeff effrg="540541 543543 556556 558558" sbcond="POST" sbnbr="87654321" /></effect>
<title>123456</title></sheet></graphic><graphic chapnbr="5" chg="x" gtype="SS" key="A5428653" pagenbr="115" revdate="" schemnbr="01" sectnbr="3" subjnbr="7"><effect effrg="540541 543543 556556 558558" />
<title>SOME RANDOM TITLE</title><sheet chapnbr="5" chg="x" gnbr="8473942_l" gtype="SS" key="A5428654" pagenbr="115" revdate="" schemnbr="01" sectnbr="3" sheetnbr="01" subjnbr="7"><effect><sbeff effrg="540541 543543 556556 558558" sbcond="POST" sbnbr="8765432" /></effect>
<title>654321</title></sheet></graphic><graphic chapnbr="5" chg="x" gtype="SS" key="A5291655" pagenbr="116" revdate="" sectnbr="3" subjnbr="7"><effect effrg="563563 570570 574574" />
<title>SOME RANDOM TITLE</title><sheet chapnbr="5" chg="x" gnbr="543637_a" gtype="SS" key="A5291656" pagenbr="116" revdate="" sectnbr="3" sheetnbr="01" subjnbr="7"><effect><sbeff effrg="570570 574574" sbcond="POST" sbnbr="FAS-N311" /><sbeff effrg="563563" sbcond="POST" sbnbr="543637" /></effect>
<title>21221127/003/A</title></sheet></graphic></root>

  val revised = <root><dummy/><graphic key="A5291653" chg="R" revdate="20090301" chapnbr="5" gtype="SS" pagenbr="115" sectnbr="3" subjnbr="7"><effect effrg="540541 543543 556556 558558" />
<title>SOME RANDOM TITLE</title><sheet key="A5291654" chg="R" revdate="20090301" chapnbr="5" gnbr="34738_x" gtype="SS" pagenbr="115" sectnbr="3" sheetnbr="01" subjnbr="7"><effect><sbeff effrg="540541 543543 556556 558558" sbcond="POST" sbnbr="87654321" /></effect>
<title>123456</title></sheet></graphic>
<effect effrg="540541 543543 556556 558558">
<title>SOME RANDOM TITLE</title><sheet key="A5428654" chg="N" revdate="20090301" chapnbr="5" gnbr="8473942_l" gtype="SS" pagenbr="115" schemnbr="01" sectnbr="3" sheetnbr="01" subjnbr="7"><effect><sbeff effrg="540541 543543 556556 558558" sbcond="POST" sbnbr="8765432" /></effect>
<title>654321</title></sheet></effect><graphic key="A5291655" chg="R" revdate="20090301" chapnbr="5" gtype="SS" pagenbr="116" sectnbr="3" subjnbr="7"><effect effrg="563563 570570 574574" />
<title>SOME RANDOM TITLE</title><sheet key="A5291656" chg="R" revdate="20090301" chapnbr="5" gnbr="543637_a" gtype="SS" pagenbr="116" sectnbr="3" sheetnbr="01" subjnbr="7"><effect><sbeff effrg="570570 574574" sbcond="POST" sbnbr="854321" /><sbeff effrg="563563" sbcond="POST" sbnbr="12345" /></effect>
<title>21221127/003/A</title></sheet></graphic></root>

  val revised_doublesheet = <root><graphic key="A5308140" chg="R" revdate="20090301" chapnbr="31" gtype="SS" pagenbr="111" sectnbr="51" subjnbr="25"><effect effrg="269271 273273" />
<title>SOME OTHERRANDOM TITLE</title><sheet key="A5308141" chg="R" revdate="20090301" chapnbr="31" gnbr="21315125_059_c" gtype="SS" pagenbr="111" sectnbr="51" sheetnbr="01" subjnbr="25"><effect><sbeff effrg="269271 273273" sbcond="POST" sbnbr="123456" /></effect>
<title>123456789</title></sheet><effect effrg="269271 273273">
<title>123456789/A</title></effect><sheet key="A5442182" chg="R" revdate="20090301" chapnbr="31" gnbr="28315125_025_-" gtype="SS" pagenbr="111" sectnbr="51" sheetnbr="03" subjnbr="25"><effect effrg="269271 273273" />
<title>123456789/Z</title></sheet></graphic></root>

  val unrevised_doublesheet = <root><graphic key="W11111"/><graphic chapnbr="31" chg="x" gtype="SS" key="A5308140" pagenbr="111" revdate="" sectnbr="51" subjnbr="25"><effect effrg="269271 273273" />
<title>SOME OTHERRANDOM TITLE</title><sheet chapnbr="31" chg="x" gnbr="21315125_059_c" gtype="SS" key="A5308141" pagenbr="111" revdate="" sectnbr="51" sheetnbr="01" subjnbr="25"><effect><sbeff effrg="269271 273273" sbcond="POST" sbnbr="123456" /></effect>
<title>123456789</title></sheet><sheet chapnbr="31" chg="x" gnbr="28315125_010_e" gtype="SS" key="A5308142" pagenbr="111" revdate="" sectnbr="51" sheetnbr="02" subjnbr="25"><effect effrg="269271 273273" />
<title>123456789/A</title></sheet><sheet chapnbr="31" chg="x" gnbr="28315125_025_-" gtype="SS" key="A5442182" pagenbr="111" revdate="" sectnbr="51" sheetnbr="03" subjnbr="25"><effect effrg="269271 273273" />
<title>123456789/Z</title></sheet></graphic></root>

  val previous_doublesheet = <root><graphic key="A5308140" chg="R" revdate="20081201" chapnbr="31" gtype="SS" pagenbr="111" sectnbr="51" subjnbr="25">
<effect effrg="268268 272272"/>
<title>SOME OTHERRANDOM TITLE</title>
<sheet key="A5308141" chg="R" revdate="20081201" chapnbr="31" gnbr="21315125_002_b" gtype="SS" pagenbr="111" sectnbr="51" sheetnbr="01" subjnbr="25">
<effect><sbeff effrg="268268 272272" sbcond="POST" sbnbr="12345678"/></effect>
<title>21315125/002/B</title></sheet>
<sheet key="A5308142" chg="D" revdate="20081201" chapnbr="31" gnbr="28315125_010_d" gtype="SS" pagenbr="111" sectnbr="51" sheetnbr="02" subjnbr="25"><revst/><deleted/><revend/></sheet>
<sheet key="A5442182" chg="R" revdate="20081201" chapnbr="31" gnbr="28315125_006_b" gtype="SS" pagenbr="111" sectnbr="51" sheetnbr="03" subjnbr="25">
<effect effrg="268268 272272"/>
<title>28315125/006/B</title></sheet></graphic></root>


  val revised_singlesheet = <root><graphic key="A5308140" chg="R" revdate="20090301" chapnbr="31" gtype="SS" pagenbr="111" sectnbr="51" subjnbr="25"><effect effrg="269271 273273" />
<title>SOME TITLE</title><effect effrg="269271 273273">
<title>123456789/A</title></effect></graphic></root>

  val unrevised_singlesheet = <root><graphic chapnbr="31" chg="x" gtype="SS" key="A5308140" pagenbr="111" revdate="" sectnbr="51" subjnbr="25"><effect effrg="269271 273273" />
<title>SOME TITLE</title><sheet chapnbr="31" chg="x" gnbr="123456789_e" gtype="SS" key="A5308142" pagenbr="111" revdate="" sectnbr="51" sheetnbr="02" subjnbr="25"><effect effrg="269271 273273" />
<title>123456789/A</title></sheet></graphic></root>

  val previous_singlesheet = <root><graphic key="A5308140" chg="R" revdate="20081201" chapnbr="31" gtype="SS" pagenbr="111" sectnbr="51" subjnbr="25">
<effect effrg="268268 272272"/>
<title>SOME TITLE</title>
<sheet key="A5308141" chg="R" revdate="20081201" chapnbr="31" gnbr="123456789_b" gtype="SS" pagenbr="111" sectnbr="51" sheetnbr="01" subjnbr="25">
<effect><sbeff effrg="268268 272272" sbcond="POST" sbnbr="12345678"/></effect>
<title>21315125/002/B</title></sheet>
<sheet key="A5308142" chg="D" revdate="20081201" chapnbr="31" gnbr="123456789_d" gtype="SS" pagenbr="111" sectnbr="51" sheetnbr="02" subjnbr="25"><revst/><deleted/><revend/></sheet>
<effect effrg="268268 272272"/>
</graphic></root>

  "should find deleted keys" in {
    val keys = AtaDocument.findDeletedKeys(previousRevision, "graphic")
    keys.length must_== 1
    keys(0) must_== "A5428653"
  }

  "should find deleted keys in single sheet graphic" in {
    val keys = AtaDocument.findDeletedKeys(previous_singlesheet, "sheet")
    keys.length must_== 1
    keys(0) must_== "A5308142"
  }

  "should find deleted keys in double sheet graphic" in {
    val keys = AtaDocument.findDeletedKeys(previous_doublesheet, "sheet")
    keys.length must_== 1
    keys(0) must_== "A5308142"
  }

  "should find returned graphics" in {
    val keys = AtaDocument.findDeletedKeys(previousRevision, "graphic")
    val returnedGraphics = AtaDocument.findReturnedElements(unrevised, keys)
    returnedGraphics.length must_== 1
    (returnedGraphics(0) \ "@key").text must_== "A5428653"
  }

  "should find returned sheets in singlesheet" in {
    val keys = AtaDocument.findDeletedKeys(previous_singlesheet, "sheet")
    val returnedSheets = AtaDocument.findReturnedElements(unrevised_singlesheet, keys)
    returnedSheets.length must_== 1
    (returnedSheets(0) \ "@key").text must_== "A5308140"
  }

  "should find returned sheets in doublesheet" in {
    val keys = AtaDocument.findDeletedKeys(previous_doublesheet, "sheet")
    val returnedSheets = AtaDocument.findReturnedElements(unrevised_doublesheet, keys)
    returnedSheets.length must_== 1
    (returnedSheets(0) \ "@key").text must_== "A5308140"
  }

  "should fix returned single sheets" in {
    val keys = AtaDocument.findDeletedKeys(previous_singlesheet, "sheet")
    val returnedSheets = AtaDocument.findReturnedElements(unrevised_singlesheet, keys)
    val fixedSheets =AtaDocument.fixReturnedSheetElements(revised_singlesheet, returnedSheets)
    fixedSheets.keys.toList.length must_== 1
  }

  "should fix returned double sheets" in {
    val keys = AtaDocument.findDeletedKeys(previous_doublesheet, "sheet")
    val returnedSheets = AtaDocument.findReturnedElements(unrevised_doublesheet, keys)
    val fixedSheets =AtaDocument.fixReturnedSheetElements(revised_doublesheet, returnedSheets)
    fixedSheets.keys.toList.length must_== 1

    (fixedSheets("A5308140")\"@key").text must_=="A5308140"
    (fixedSheets("A5308140")\"@chg").text must_=="R"
    ((fixedSheets("A5308140")\"sheet")(0)\"@chg").text must_=="R"
    ((fixedSheets("A5308140")\"sheet")(0)\"@revdate").text must_=="20090301"
    ((fixedSheets("A5308140")\"sheet")(1)\"@chg").text must_=="N"
    ((fixedSheets("A5308140")\"sheet")(1)\"@revdate").text must_=="20090301"

  }

  "should fix returned graphics" in {
    val keys = AtaDocument.findDeletedKeys(previousRevision, "graphic")
    val returnedGraphics = AtaDocument.findReturnedElements(unrevised, keys)
    val fixedGraphics =AtaDocument.restoreRemovedGraphics(revised, returnedGraphics, scala.collection.mutable.Map [String, Node]())
    ((fixedGraphics \ "graphic").first \"@key").text must_== "A5291653"
  }
  
}

