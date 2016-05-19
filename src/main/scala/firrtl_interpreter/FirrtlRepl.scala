// See LICENSE for license details.
package firrtl_interpreter

import java.io.File

import scala.collection.mutable.ArrayBuffer
import scala.tools.jline.console.ConsoleReader
import scala.tools.jline.console.history.FileHistory
import scala.tools.jline.TerminalFactory
import scala.tools.jline.console.completer._
import collection.JavaConverters._

abstract class Command(val name: String) {
  def run(args: Array[String])
  def usage: (String, String)
  def completer: Option[ArgumentCompleter] = {
    Some(new ArgumentCompleter(
      new StringsCompleter({name})
    ))
  }
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

  var interpreterOpt: Option[FirrtlTerp] = None
  def interpreter: FirrtlTerp = interpreterOpt.get
  var args = Array.empty[String]
  var done = false

  object Commands {
    def getOneArg(failureMessage: String, argOption: Option[String] = None): Option[String] = {
      if(args.length == 2) {
        Some(args(1))
      }
      else if(args.length == 1 && argOption.isDefined) {
        Some(argOption.get)
      }
      else {
        error(failureMessage)
        None
      }
    }
    def getTwoArgs(failureMessage: String,
                   arg1Option: Option[String] = None,
                   arg2Option: Option[String] = None
                  ): Option[(String,String)] = {
      if(args.length == 3) {
        Some(args(1), args(2))
      }
      else if(args.length == 2 && arg2Option.isDefined) {
        Some(args(1), arg2Option.get)
      }
      else if(args.length == 1 && arg1Option.isDefined && arg2Option.isDefined) {
        Some(arg1Option.get, arg2Option.get)
      }
      else {
        error(failureMessage)
        None
      }
    }
    def getThreeArgs(failureMessage: String,
                   arg1Option: Option[String] = None,
                   arg2Option: Option[String] = None,
                   arg3Option: Option[String] = None
                  ): Option[(String,String,String)] = {
      if(args.length == 4) {
        Some(args(1), args(2), args(3))
      }
      else if(args.length == 3 && arg3Option.isDefined) {
        Some(args(1), args(2), arg3Option.get)
      }
      else if(args.length == 2 && arg2Option.isDefined && arg3Option.isDefined) {
        Some(args(1), arg2Option.get, arg3Option.get)
      }
      else if(args.length == 1 && arg1Option.isDefined && arg2Option.isDefined && arg3Option.isDefined) {
        Some(arg1Option.get, arg2Option.get, arg3Option.get)
      }
      else {
        error(failureMessage)
        None
      }
    }
    val commands = ArrayBuffer.empty[Command]
    commands ++= Seq(
      new Command("load") {
        def usage: (String, String) = ("load fileName", "load/replace the current firrtl file")
        override def completer: Option[ArgumentCompleter] = {
          Some(new ArgumentCompleter(
            new StringsCompleter({"load"}),
            new FileNameCompleter
          ))
        }
        def run(args: Array[String]): Unit = {
          getOneArg("load filename") match {
            case Some(fileName) =>
              try {
                var file = new File(fileName)
                if(! file.exists()) {
                  file = new File(fileName + ".fir")
                  if(! file.exists()) {
                    throw new Exception(s"file $fileName does not exist")
                  }
                }
                val input = io.Source.fromFile(file).mkString
                interpreterOpt = Some(FirrtlTerp(input))
                buildCompletions()
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
        def usage: (String, String) = ("poke inputPortName value", "set an input port to the given integer value")
        override def completer: Option[ArgumentCompleter] = {
          if(interpreterOpt.isEmpty) {
            None
          }
          else {
            val inputPorts = ArrayBuffer.empty[String]
            inputPorts ++= interpreter.dependencyGraph.inputPorts.toSeq
            val list: java.util.List[String] = inputPorts.asJava
            Some(new ArgumentCompleter(
              new StringsCompleter({
                "poke"
              }),
              new StringsCompleter(list)
            ))
          }
        }
        def run(args: Array[String]): Unit = {
          getTwoArgs("poke inputPortName value") match {
            case Some((portName, valueString)) =>
              try {
                val value = valueString.toInt
                interpreter.setValueWithBigInt(portName, value)
              }
              catch {
                case e: Exception =>
                  error(s"exception ${e.getMessage} $e")
              }
            case _ =>
          }
        }
      },
      new Command("peek") {
        def usage: (String, String) = ("peek componentName", "show the current value of the named circuit component")
        override def completer: Option[ArgumentCompleter] = {
          if(interpreterOpt.isEmpty) {
            None
          }
          else {
            val inputPorts = ArrayBuffer.empty[String]
            inputPorts ++= interpreter.circuitState.validNames.toSeq
            val list: java.util.List[String] = inputPorts.asJava
            Some(new ArgumentCompleter(
              new StringsCompleter({
                "peek"
              }),
              new StringsCompleter(list)
            ))
          }
        }
        def run(args: Array[String]): Unit = {
          getOneArg("peek componentName") match {
            case Some(componentName) =>
              try {
                val value = interpreter.getValue(componentName)
                console.println(s"peek $componentName $value")
              }
              catch {
                case e: Exception =>
                  error(s"exception ${e.getMessage} $e")
              }
            case _ =>
          }
        }
      },
      new Command("randomize") {
        def usage: (String, String) = ("randomize",
          "randomize all inputs except reset)")
        def run(args: Array[String]): Unit = {
          for{
            (inputPortName, value) <- interpreter.circuitState.inputPorts
            if inputPortName != "reset"
          } {
            interpreter.setValue(inputPortName, TypeInstanceFactory(value, randomBigInt(value.width)))
          }
          console.println(interpreter.circuitState.prettyString())
        }
      },
      new Command("reset") {
        def usage: (String, String) = ("reset [numberOfSteps]",
          "assert reset (if present) for numberOfSteps (default 1)")
        def run(args: Array[String]): Unit = {
          getOneArg("reset [numberOfSteps]", Some("1")) match {
            case Some(numberOfStepsString) =>
              try {
                interpreter.setValueWithBigInt("reset", 1)
                val numberOfSteps = numberOfStepsString.toInt
                for(stepNumber <- 0 until numberOfSteps) {
                  interpreter.cycle(showState = false)
                  interpreter.evaluateCircuit()
                }
                interpreter.setValueWithBigInt("reset", 0)
                console.println(interpreter.circuitState.prettyString())
              }
              catch {
                case e: Exception =>
                  error(s"exception ${e.getMessage} $e")
              }
            case _ =>
          }
        }
      },
      new Command("step") {
        def usage: (String, String) = ("step [numberOfSteps]",
          "cycle the clock numberOfSteps (default 1) times, and show state")
        def run(args: Array[String]): Unit = {
          getOneArg("step [numberOfSteps]", Some("1")) match {
            case Some(numberOfStepsString) =>
              try {
                val numberOfSteps = numberOfStepsString.toInt
                for(stepNumber <- 0 until numberOfSteps) {
                  interpreter.cycle(showState = false)
                  interpreter.evaluateCircuit()
                }
                console.println(interpreter.circuitState.prettyString())
              }
              catch {
                case e: Exception =>
                  error(s"exception ${e.getMessage} $e")
              }
            case _ =>
          }
        }
      },
      new Command("waitfor") {
        def usage: (String, String) = ("waitfor componentName value [maxNumberOfSteps]",
          "wait for particular value (default 1) on component, up to maxNumberOfSteps (default 100)")
        def run(args: Array[String]): Unit = {
          getThreeArgs(
            "waitfor componentName [value] [maxNumberOfSteps]",
            arg2Option = Some("1"),
            arg3Option = Some("100")
          ) match {
            case Some((componentName, valueString, maxNumberOfStepsString)) =>
              try {
                val maxNumberOfSteps = maxNumberOfStepsString.toInt
                val value = valueString.toInt

                var tries = 0
                while(tries < maxNumberOfSteps && interpreter.getValue(componentName).value != BigInt(value)) {
                  interpreter.cycle()
                  tries += 1
                }
                if(interpreter.getValue(componentName).value != BigInt(value)) {
                  console.println(
                    s"waitfor exhausted $componentName did not take on value $value in $maxNumberOfSteps cycles")
                }
                else {
                  console.println(s"$componentName == value $value in $tries cycles")
                }
              }
              catch {
                case e: Exception =>
                  error(s"exception ${e.getMessage} $e")
              }
            case _ =>
          }
        }
      },
      new Command("show") {
        def usage: (String, String) = ("show", "show the state of the circuit")
        def run(args: Array[String]): Unit = {
          console.println(interpreter.circuitState.prettyString())
        }
      },
      new Command("help") {
        def usage: (String, String) = ("help", "show available commands")
        def run(args: Array[String]): Unit = {
          val maxColumn1Width = Commands.commands.map(_.usage._1.length).max + 2
          Commands.commands.foreach { command =>
            val (column1, column2) = command.usage
            terminal.getWidth

            console.println(s"$column1${" "*(maxColumn1Width - column1.length)} $column2")
          }
        }
      },
      new Command("quit") {
        def usage: (String, String) = ("quit", "exit the interpreter")
        def run(args: Array[String]): Unit = {
          history.removeLast()
          done = true
        }
      }
    )
    val commandMap = commands.map(command => command.name -> command).toMap
  }

  def buildCompletions(): Unit = {
    console.setCompletionHandler(new CandidateListCompletionHandler {})
    Commands.commands.flatMap { command =>
      command.completer
    }.foreach { completer =>
      console.addCompleter(completer)
    }
  }

  def run() {
    buildCompletions()
    console.setPrompt("firrtl>> ")

    while (! done) {
      try {

        val line = console.readLine()

        args = line.split(" ")

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
    console.println(s"saving history ${history.size()}")
    history.flush()
    console.shutdown()
    terminal.restore()
  }

  def error(message: String): Unit = {
    console.println(s"Error: $message")
  }
}

object FirrtlRepl {
  def main(args: Array[String]): Unit = {
    val repl = new FirrtlRepl
    repl.run()
  }
}
