package com.github.hibou107.List

sealed trait Context[A]

case class ZipperList[A](previous: List[A], v: A, next: List[A]) {
  def goNext(): Option[ZipperList[A]] = {
    next match {
      case Nil => None
      case x :: rest => Some(ZipperList(v :: previous, x, rest))
    }
  }

  def goBack(): Option[ZipperList[A]] = {
    previous match {
      case Nil => None
      case x :: rest => Some(ZipperList(rest, x, v :: next))
    }
  }
}

object ZipperList {

  def create[A](l: List[A]): Option[ZipperList[A]] = {
    l match {
      case Nil => None
      case x :: rest => Some(ZipperList(Nil, x, rest))
    }
  }

  implicit class ZipperListOps[A](input: Option[ZipperList[A]]) {
    def goNext(): Option[ZipperList[A]] = {
      input.fold(Option.empty[ZipperList[A]])(_.goNext())
    }
    def goBack(): Option[ZipperList[A]] = {
      input.fold(Option.empty[ZipperList[A]])(_.goBack())
    }
  }
}
