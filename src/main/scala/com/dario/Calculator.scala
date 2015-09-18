package com.dario

import scala.collection.mutable
import scala.util.{Failure, Success, Try}


object Operator {
  def operatorFromChar(char: Char) : Try[Operator] = char match {
    case '+' => Success(Add())
    case '-' => Success(Substract())
    case '/' => Success(Divide())
    case '*' => Success(Multiply())
    case o => Failure(new Throwable(s"Unable to parse operator $o"))
  }
}

class Element

case class Number(number: Float) extends Element

class Operator() extends Element {
  def compute(a : Float, b : Float) : Float = ???
}

case class Add() extends Operator {
  override def compute(a : Float, b : Float) : Float = a + b
}

case class Divide() extends Operator {
  override def compute(a : Float, b : Float) : Float = a / b
}

case class Multiply() extends Operator {
  override def compute(a : Float, b : Float) : Float = a * b
}

case class Substract() extends Operator {
  override def compute(a : Float, b : Float) : Float = a - b
}

class Calculator {

  def compute(expression : String ) : Try[Float] = {
    for {elements <- toElements(expression.replace(" ", ""))
         result <- doMath(elements.toList)
    } yield result
  }

  private def toElements(expression : String) : Try[Array[Element]] = {
    val elements  = new mutable.MutableList[Element]()
    val numberBuffer = new StringBuffer()
    val iterator = expression.iterator
    var failure : Option[Failure[Array[Element]]] = None
    while (iterator.hasNext && failure.isEmpty) {
      val next = iterator.next()
      if(next.isDigit || next == '.') {
        numberBuffer.append(next)
      } else {
        if(numberBuffer.length() > 0) {
          elements += Number(numberBuffer.toString.toFloat)
          numberBuffer.setLength(0)
        } else {
          failure = Some(Failure(new Throwable("Invalid expression since there's no number before operator")))
        }
        Operator.operatorFromChar(next) map {elements += _} recover {
          case op : Throwable =>
            failure = Some(Failure(op))
        }
      }
    }

    failure getOrElse {
      if(numberBuffer.length > 0) {
        elements += Number(numberBuffer.toString.toFloat)
      }
      Success(elements.toArray)
    }
  }

  private def doMath(operations : List[Element]) : Try[Float] = {

    def helper(list: List[Element], acc : Float): Try[Float] = {
      list match {
        case Nil =>
          Success(acc)

        case (o: Operator) :: Number(n) :: tail =>
          helper(tail, o.compute(acc, n))

        case rest =>
          Failure(new Throwable("unable to parse expression at "+ rest))
      }
    }

    operations.headOption match {
      case Some(Number(n)) =>
        helper(operations.tail, n)
      case _ =>
        Failure(new Throwable("invalid expression since it does not start with a number"))
    }
  }

}
