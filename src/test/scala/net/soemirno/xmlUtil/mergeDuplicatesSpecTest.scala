package net.soemirno.xmlUtil


import org.specs.runner.{ Runner, JUnit}
import org.specs.Specification

class mergeDuplicatesSpecTest extends Runner(mergeDuplicatesSpec) with JUnit


object mergeDuplicatesSpec extends Specification {
  "'hello world' has 11 characters" in {
      "hello world".size must_== 11
  }

  "'hello world' matches 'h.* w.*'" in {
    "hello world" must beMatching("h.* w.*")
  }
}