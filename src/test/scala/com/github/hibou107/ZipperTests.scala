package com.github.hibou107

import com.github.hibou107.List.ZipperList
import com.github.hibou107.tree.{Branch, Empty, ZipperTree}
import org.scalatest.{FlatSpec, Matchers}

class ZipperTests extends FlatSpec with Matchers {
  "ZipperTree" should "work" in {
    val tree = Branch(Branch(Branch(Empty(), 1.0, Empty()), 3.0, Branch(Empty(), 2.0, Empty())), 1.0, Empty())
    val zipper = ZipperTree.create(tree)
    val bottom = zipper.goLeft().goLeft()
    bottom.v shouldBe 1.0
    val head = bottom.goUp().goUp()
    head.v shouldBe 1.0
  }

  "ZipperList" should "work" in {
   val zipper = ZipperList.create(1 :: 2 :: 3 :: 4 :: Nil)
    zipper.goNext().goNext().get.v shouldBe 3
    zipper.goNext().goNext().goBack().get.v shouldBe 2


  }


}
