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
import java.util

import scala.tools.jline.console.ConsoleReader
import scala.tools.jline.console.history.FileHistory
import scala.tools.jline.TerminalFactory

abstract class Command(val name: String) {
  def run(args: Array[String])
}

class FirrtlRepl {
  val terminal = TerminalFactory.create()
  val console = new ConsoleReader
  val historyPath = "~/.firrtl_repl_history".replaceFirst("^~",System.getProperty("user.home"))
  val historyFile = new File(historyPath)
  if(! historyFile.exists()) {
    println(s"creating ${historyFile.getName}")
    historyFile.createNewFile()
  }
  val history = new FileHistory(historyFile)

  history.load(historyFile)
  console.setHistory(history)

  var interpreter: Option[FirrtlTerp] = None
  var args = Array.empty[String]
  var done = false

  object Commands {
    def getOneArg(failureMessage: String): Option[String] = {
      if(args.length == 2) {
        Some(args(1))
      }
      else {
        error(failureMessage)
        None
      }
    }
    val commands = Array(
      new Command("load") {
        def run(args: Array[String]): Unit = {
          getOneArg("load filename") match {
            case Some(fileName) =>
              try {
                val input = io.Source.fromFile(fileName).mkString
                interpreter = Some(FirrtlTerp(input))
              }
              catch {
                case e: Exception =>
                  error(s"exception ${e.getMessage} $e")
              }
            case _ =>
          }
        }
      },
      new Command("poke") {
        def run(args: Array[String]): Unit = {
          println("poke")
        }
      },
      new Command("peek") {
        def run(args: Array[String]): Unit = {
          println("peek")
        }
      },
      new Command("peek") {
        def run(args: Array[String]): Unit = {

        }
      },
      new Command("expect") {
        def run(args: Array[String]): Unit = {
          println("expect")
        }
      },
      new Command("quit") {
        def run(args: Array[String]): Unit = {
          history.removeLast()
          done = true
        }
      }
    )
    val commandMap = commands.map(command => command.name -> command).toMap
  }

  def run() {
    console.setPrompt("firrtl>> ")

    while (! done) {
      try {

        val line = console.readLine()

        args = line.split((" "))

        if (args.length > 0) {
          if (Commands.commandMap.contains(args.head)) {
            Commands.commandMap(args.head).run(args.tail)
          }
        }
        else {
          error("unknown command")
        }
      }
      catch {
        case e: NullPointerException =>
          error(s"repl error ${e.getMessage}")
          done = true
      }
    }
    println(s"saving history ${history.size()}")
    history.flush()
    println("flush done")
    console.shutdown()
    terminal.restore()
  }

  def error(message: String): Unit = {
    println(s"Error: $message")
  }
}

object FirrtlRepl {
  def main(args: Array[String]): Unit = {
    val repl = new FirrtlRepl
    repl.run()
  }
}
