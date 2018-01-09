package com.github.hibou107

import org.scalatest.{FlatSpec, Matchers}

class ZipperTests extends FlatSpec with Matchers {
  it should "work" in {
    val tree = Branch(Branch(Branch(Empty(), 1.0, Empty()), 3.0, Branch(Empty(), 2.0, Empty())), 1.0, Empty())
    val zipper = ZipperTree.create(tree)
    val bottom = zipper.goLeft().goLeft()
    bottom.v shouldBe 1.0
    val head = bottom.goUp().goUp()
    head.v shouldBe 1.0
  }

}
