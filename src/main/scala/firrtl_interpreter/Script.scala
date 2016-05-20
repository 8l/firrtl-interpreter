/*
Copyright (c) 2014 - 2016 The Regents of the University of
California (Regents). All Rights Reserved.  Redistribution and use in
source and binary forms, with or without modification, are permitted
provided that the following conditions are met:
   * Redistributions of source code must retain the above
     copyright notice, this list of conditions and the following
     two paragraphs of disclaimer.
   * Redistributions in binary form must reproduce the above
     copyright notice, this list of conditions and the following
     two paragraphs of disclaimer in the documentation and/or other materials
     provided with the distribution.
   * Neither the name of the Regents nor the names of its contributors
     may be used to endorse or promote products derived from this
     software without specific prior written permission.
IN NO EVENT SHALL REGENTS BE LIABLE TO ANY PARTY FOR DIRECT, INDIRECT,
SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING LOST PROFITS,
ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF
REGENTS HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
REGENTS SPECIFICALLY DISCLAIMS ANY WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE. THE SOFTWARE AND ACCOMPANYING DOCUMENTATION, IF
ANY, PROVIDED HEREUNDER IS PROVIDED "AS IS". REGENTS HAS NO OBLIGATION
TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
MODIFICATIONS.
*/
package firrtl_interpreter

import java.io.File

case class ScriptFactory(parent: FirrtlRepl) {
  val console = parent.console
  var lastFileOption: Option[String] = None

  def apply(fileName: String): Option[Script] = {
    try {
      val file = new File(fileName)
      if(! file.exists()) {
        throw new Exception(s"file $fileName does not exist")
      }
      val scriptLines = io.Source.fromFile(file).mkString.split("\n").toArray
      val script = new Script(fileName, scriptLines)
      Some(script)
    }
    catch {
      case e: Exception =>
        parent.error(s"exception ${e.getMessage} $e")
        e.printStackTrace()
        None
    }
  }
}

class Script(val fileName: String, val lines: Array[String]) {
  var currentLine = -1
  var linesLeftToRun = 0

  def getNextLineOption(): Option[String] = {
    if(hasNext) {
      currentLine += 1
      linesLeftToRun -= 1
      val nextLine = lines(currentLine)
      Some(nextLine)
    }
    else {
      None
    }
  }

  def hasNext: Boolean = {
    currentLine < lines.length-1 && linesLeftToRun > 0
  }

  def length: Int = lines.length

  def setLinesToRun(n: Int): Unit = {
    linesLeftToRun = n.max((lines.length - currentLine)+1)
  }

  def runRemaining(): Unit = {
    linesLeftToRun = (lines.length - currentLine)+1
  }

  def atEnd: Boolean = {
    currentLine == lines.length - 1
  }

  def reset(): Unit = {
    currentLine = -1
    linesLeftToRun = lines.length
  }

  override def toString: String = {
    lines.zipWithIndex.map { case (line, index) =>
      f"$index%3d" +
        (if(index == currentLine) "* " else "  " ) +
        line
    }.mkString("\n")
  }
}
