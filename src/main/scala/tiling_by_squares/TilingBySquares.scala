package tiling_by_squares

import math._
import scala.collection.mutable
import scala.util._
import scala.io.StdIn._

import scala.io.StdIn.readLine

object TilingBySquares extends App {
  val costsMap: mutable.Map[Rectangle, Int] = mutable.Map.empty
  //  val Array(w, h) = (readLine split " ").map(_.toInt)
  val w = 7
  val h = 27
  // Write an answer using println
  // To debug: Console.err.println("Debug messages...")

  val tree: Tree = Tree(Node(Rectangle.build(w, h)))

  while (tree.rootNode.cost.isEmpty || tree.rootNode.children.exists(_.nonEmpty))
    buildTree(tree.rootNode)

  println(tree.rootNode.cost.get)

  lazy val nodeIsUnProcessed: Node => Boolean = node =>
    node.cost.isEmpty || node.children.isEmpty || node.children.exists(nodeChildren => nodeChildren.nonEmpty)

  lazy val nodeContainsUnprocessedChildren: Node => Boolean = node => {
    node.children.isEmpty ||
    node.children
      .exists(nodeChildren => nodeChildren.nonEmpty && nodeChildren.exists(nodeIsUnProcessed))
  }

  lazy val nodeContainsUnprocessedGrandChildren: Node => Boolean = node =>
    node.children
      .exists(children => children.exists(nodeContainsUnprocessedChildren))

  lazy val nodeIsReadyForCalc: Node => Boolean = node =>
    node.children
      .exists(nodeChildren => nodeChildren.forall(nodeChild => nodeChild.children.exists(_.isEmpty)))

  private def calculateNode(node: Node) = {
    val nodeCost = node.children.get.flatMap(_.cost).min
    costsMap.put(node.rectangle, nodeCost)

    node.cost = Some(nodeCost + node.cost.getOrElse(0))
    node.children = Some(mutable.Seq.empty)
  }

  private def buildTree(node: Node): Unit = {
    node match {
      case Node(_, None, _) => buildChildren(node)
      case Node(_, Some(children), _) if nodeIsReadyForCalc(node) => calculateNode(node)
      case Node(_, Some(children), _) if nodeContainsUnprocessedChildren(node) =>
        children
          .collectFirst {
            case childNode: Node if nodeIsUnProcessed(childNode) => buildTree(childNode)
          }
    }
  }

  private def buildChildren(node: Node): Unit = {
    costsMap
      .get(node.rectangle)
      .orElse(getBaseCase(node.rectangle))
      .fold {
        val widthChildrenNodes  = buildAllChildrenForSide(node.rectangle.w, node.rectangle.l)
        val lengthChildrenNodes = buildAllChildrenForSide(node.rectangle.l, node.rectangle.w)
        node.children = Some(widthChildrenNodes ++ lengthChildrenNodes)
      } { cost =>
        node.cost = Some(node.cost.getOrElse(0) + cost)
        node.children = Some(mutable.Seq.empty)
      }
  }

  private def buildAllChildrenForSide(x1: Int, x2: Int): mutable.Seq[Node] = {
    val children: mutable.Seq[(Rectangle, Int)] = if (x1 % 2 == 0) {
      mutable.Seq.empty
        .:+(slashingAlgo(x1, x2, 1, 1, 1))
        .:+(slashingAlgo(x1, x2, 1, 2, 2))
        .flatten
    } else {
      val spiralResult = spiralAlgo(x1, x2)
      if (x1 % 3 == 0) {
        mutable.Seq.empty
          .:+(slashingAlgo(x1, x2, 1, 1, 1))
          .:+(slashingAlgo(x1, x2, 1, 3, 3))
          .:+(slashingAlgo(x1, x2, 2, 3, 3))
          .:+(spiralResult)
          .flatten
      } else {
        mutable.Seq.empty
          .:+(slashingAlgo(x1, x2, 1, 1, 1))
          .:+(spiralResult)
          .flatten
      }
    }
    children.map(tuple => Node(rectangle = tuple._1, cost = Some(tuple._2)))
  }

  private def getBaseCase(rectangle: Rectangle): Option[Int] = {
    if (rectangle.w == 0 || rectangle.l == 0) {
      Some(0)
    } else if (rectangle.l % rectangle.w == 0) {
      Some(rectangle.l / rectangle.w)
    } else {
      None
    }
  }

  def slashingAlgo(
      x1: Int,
      x2: Int,
      slashingSquareNumerator: Int,
      slashingSquareDenominator: Int,
      slashingSquareCost: Int,
  ): Option[(Rectangle, Int)] = {
    if (x1 * slashingSquareNumerator / slashingSquareDenominator <= x2) {
      val newRectangle = Rectangle.build(x1, x2 - x1.*(slashingSquareNumerator)./(slashingSquareDenominator))

      Some((newRectangle, slashingSquareCost))
    } else {
      None
    }
  }

  def spiralAlgo(x1: Int, x2: Int): Option[(Rectangle, Int)] = {
    if ((x1 > x2 && (x1/2 + 1) > x2) || (x1 < x2 && (x2 - (x1/2 + 1)) > x1)) {
      None
    } else {
      val lowerLeftSquareLength = (x1 / 2) + 1
      val lowerRightSquareLength = x1 / 2

      if (x2 < lowerLeftSquareLength*2) {
        val upperRightSquareLength = x2 - lowerRightSquareLength
        val upperRightSquareCount = lowerRightSquareLength / upperRightSquareLength
        val smallSquares = lowerRightSquareLength - (upperRightSquareLength * upperRightSquareCount)
        val newRectangle = Rectangle.build(lowerLeftSquareLength + smallSquares, x2 - lowerLeftSquareLength)
        Some((newRectangle, 2 + upperRightSquareCount + smallSquares))
      } else {
        val upperLeftSquareLength = x2 - lowerLeftSquareLength
        val smallSquares  = upperLeftSquareLength - lowerLeftSquareLength

        val newRectangle = Rectangle.build(x1 - upperLeftSquareLength, x2 - lowerRightSquareLength)
        Some((newRectangle, 3 + smallSquares))
      }
    }
  }

}

case class Tree(rootNode: Node)

case class Node(rectangle: Rectangle, var children: Option[mutable.Seq[Node]] = None, var cost: Option[Int] = None)

case class Rectangle(w: Int, l: Int)

object Rectangle {
  def build(w: Int, l: Int): Rectangle =
    Rectangle(w.min(l), w.max(l))
}
