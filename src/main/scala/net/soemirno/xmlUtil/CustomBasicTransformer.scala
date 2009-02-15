package net.soemirno.xmlUtil

import _root_.scala.xml.transform.BasicTransformer
import _root_.scala.xml.{ NodeBuffer, Node}
abstract class CustomBasicTransformer extends BasicTransformer {

   /** Returns a new node buffer with the first <code>pos</code> elements
   *  from <code>ns</code>.
   *
   *  @param pos ..
   *  @param ns  ..
   *  @return    ..
   */
  override protected def buffer(pos: Int, ns: Seq[Node]): NodeBuffer = {
    val nb = new NodeBuffer()
    var jt = ns.elements
    var j = 0; while (j < pos) {
      nb.append(jt.next)
      j += 1
    }
    nb
  }
}

