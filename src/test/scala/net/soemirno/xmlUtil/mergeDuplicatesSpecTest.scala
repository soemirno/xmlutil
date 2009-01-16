package net.soemirno.xmlUtil

import org.specs.runner.{Runner, JUnit}
import org.specs.Specification

class mergeDuplicatesSpecTest extends Runner(mergeDuplicatesSpec) with JUnit

object mergeDuplicatesSpec extends Specification {

  val listWithDuplicates = List(1, 2, 2, 3, 4)

  def normalizeList(list: List[int]): List[int] =
    {
      if (list.length == 1)
        list
      else {
        if (list.head == list.tail.head) {
          list.head :: normalizeList(list.tail.tail)
        } else {
          list.head :: normalizeList(list.tail)
        }
      }
    }

  "normalized list contains 4 items" in {
    (normalizeList(listWithDuplicates)).size must_== 4
  }

  "normalized list has '3' as third element" in {
    (normalizeList(listWithDuplicates))(2) must_== 3
  }


}