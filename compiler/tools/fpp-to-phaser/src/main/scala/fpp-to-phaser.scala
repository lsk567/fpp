package fpp.compiler.tools

import fpp.compiler.analysis._
import fpp.compiler.ast._
import fpp.compiler.codegen._
import fpp.compiler.syntax._
import fpp.compiler.transform._
import fpp.compiler.util._
import scopt.OParser

object FPPToPhaser {

  case class Options(
    numPhasers: Int = 1
  )

  def command(options: Options) = {
    for {
      
    } yield ()
  }

  def main(args: Array[String]) =
    Tool(name).mainMethod(args, oparser, Options(), command)

  val builder = OParser.builder[Options]

  val name = "fpp-to-phaser"

  val oparser = {
    import builder._
    OParser.sequence(
      programName(name),
      head(name, Version.v),
      help('h', "help").text("print this message and exit"),
      opt[Int]('n', "num-phasers")
        .valueName("<size>")
        .validate(s => if (s > 0) success else failure("Number of phasers must be greater than zero"))
        .action((s, c) => c.copy(numPhasers = s))
        .text("set the number of phasers to map tasks to"),
    )
  }

}
