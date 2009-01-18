package net.soemirno.xmlUtil

import _root_.org.specs.runner.{Runner, JUnit}
import _root_.org.specs.Specification

class loadAtaDocumentSpecTest extends Runner(loadAtaDocumentSpec) with JUnit

object loadAtaDocumentSpec extends Specification {

  val ipcDoc = <aipc>
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
  </item></aipc>

  "has created ata document from xml element" in {
    val ataDocument = new AtaDocument(ipcDoc)
    Console.println(ataDocument.normalized)
  }
}