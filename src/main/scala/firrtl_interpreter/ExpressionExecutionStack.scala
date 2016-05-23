// See LICENSE for license details.

package firrtl_interpreter

import firrtl.Expression

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class ExpressionExecutionStack(dependencyGraph: DependencyGraph) {
  val MaxExecutionDepth = 1000

  val expressionStack = new ArrayBuffer[StackItem]
  val stackKeys = new mutable.HashSet[String]

  case class StackItem(lhsOpt: Option[String], expression: Expression) {
    override def toString: String = {
      s"${dependencyGraph.addKind(lhsOpt.getOrElse("     "))} -> ${expression.serialize}"
    }
  }

  def stackListing: String = {
    expressionStack.filter(_.lhsOpt.nonEmpty).zipWithIndex.map { case (entry,index) =>
      f"$index%4d $entry"
    }.mkString("\n")
  }

  def push(keyOption: Option[String], expression: Expression): Unit = {
    expressionStack += StackItem(keyOption, expression)
    if(expressionStack.length > MaxExecutionDepth) {
//      println(s"ExpressionStack to deep, max is ${MaxExecutionDepth}")
//      println(stackListing)
      throw new InterruptedException(s"ExpressionStack to deep, max is ${MaxExecutionDepth}")
    }
    keyOption.foreach { expressionKey =>
      if(stackKeys.contains(expressionKey)) {
//        println(s"key ${expressionKey} already in stack of size ${expressionStack.length}")
//        println(stackListing)
        throw new InterpreterException(s"Expression key $expressionKey already in stack")
      }
      stackKeys += expressionKey
    }
  }

  def pop(): StackItem = {
    expressionStack.remove(expressionStack.length-1)
  }
}
