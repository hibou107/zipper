package com.github.hibou107

sealed trait Tree[A]

case class Empty[A]() extends Tree[A]

case class Branch[A](left: Tree[A], v: A, right: Tree[A]) extends Tree[A]

sealed trait Context[A]

case class Top[A]() extends Context[A]
case class Right[A](v: A, tree: Tree[A], context: Context[A]) extends Context[A]
case class Left[A](v: A, tree: Tree[A], context: Context[A]) extends Context[A]

case class ZipperTree[A](lefts: Tree[A], v: A, rights: Tree[A], context: Context[A]) {
  def goLeft(): ZipperTree[A] = lefts match {
    case Empty() => throw new RuntimeException("Cannot go left")
    case Branch(left, value, right) =>
      ZipperTree(left, value, right, Right(v, rights, context))
  }

  def goRight(): ZipperTree[A] = rights match {
    case Empty() => throw new RuntimeException("Cannot go left")
    case Branch(left, value, right) =>
      ZipperTree(left, value, right, Left(v, lefts, context))
  }

  def goUp(): ZipperTree[A] = context match {
    case Top() => throw new RuntimeException("Cannot go up")
    case Right(thisValue, thisTree, thisContext) => ZipperTree(Branch(lefts, v, rights), thisValue, thisTree, thisContext)
    case Left(thisValue, thisTree, thisContext) => ZipperTree(thisTree, thisValue, Branch(lefts, v, rights), thisContext)
  }

  def update(newValue: A): ZipperTree[A] = this.copy(v = newValue)

  def remove(): ZipperTree[A] = this.goUp().copy(lefts = Empty())

}

object ZipperTree {
  def create[A](tree: Tree[A]): ZipperTree[A] = {
    tree match {
      case Empty() => throw new RuntimeException("Cannot create Zipper of empty tree")
      case Branch(left, v, right) => ZipperTree(left, v, right, Top())
    }
  }
}

