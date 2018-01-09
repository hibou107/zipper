package com.github.hibou107

sealed trait Tree[A]

case class Empty[A]() extends Tree[A]

case class Branch[A](left: Tree[A], v: A, right: Tree[A]) extends Tree[A]

sealed trait Context[A]

case class Top[A]() extends Context[A]
case class Right[A](v: A, tree: Tree[A], context: Context[A]) extends Context[A]
case class Left[A](v: A, tree: Tree[A], context: Context[A]) extends Context[A]

case class Zipper[A](lefts: Tree[A], v: A, rights: Tree[A], context: Context[A]) {
  def goLeft(): Zipper[A] = lefts match {
    case Empty() => throw new RuntimeException("Cannot go left")
    case Branch(left, value, right) =>
      Zipper(left, value, right, Right(v, rights, context))
  }

  def goRight(): Zipper[A] = rights match {
    case Empty() => throw new RuntimeException("Cannot go left")
    case Branch(left, value, right) =>
      Zipper(left, value, right, Left(v, lefts, context))
  }

  def goUp(): Zipper[A] = context match {
    case Top() => throw new RuntimeException("Cannot go up")
    case Right(thisValue, thisTree, thisContext) => Zipper(Branch(lefts, v, rights), thisValue, thisTree, thisContext)
    case Left(thisValue, thisTree, thisContext) => Zipper(thisTree, thisValue, Branch(lefts, v, rights), thisContext)
  }

  def update(newValue: A): Zipper[A] = this.copy(v = newValue)

}

object Zipper {
  def create[A](tree: Tree[A]): Zipper[A] = {
    tree match {
      case Empty() => throw new RuntimeException("Cannot create Zipper of empty tree")
      case Branch(left, v, right) => Zipper(left, v, right, Top())
    }
  }
}

