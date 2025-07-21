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
    numPhasers: Int = 1,
    dir: Option[String] = None,
    files: List[File] = Nil,
  )

  def command(options: Options) = {
    val files = options.files.reverse match {
      case Nil => List(File.StdIn)
      case list => list
    }
    val a = Analysis(inputFileSet = options.files.toSet)
    for {
      tulFiles <- Result.map(files, Parser.parseFile (Parser.transUnit) (None) _)
      aTulFiles <- ResolveSpecInclude.transformList(
        a,
        tulFiles, 
        ResolveSpecInclude.transUnit
      )
      tulFiles <- Right(aTulFiles._2)

      // Extract all rate group info from instance.fpp and annotations.
      _ <- {
        val dir = options.dir match {
          case Some(dir1) => dir1
          case None => "."
        }
        val state = RateGroupState()
        RateGroupVisitor.tuList(state, tulFiles)
      }

      // Compute SSFA by unrolling rate group execution.

      // Generate phaser configurations.

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
      opt[String]('d', "directory")
        .valueName("<dir>")
        .action((d, c) => c.copy(dir = Some(d)))
        .text("output directory"),
      opt[Int]('n', "num-phasers")
        .valueName("<size>")
        .validate(s => if (s > 0) success else failure("Number of phasers must be greater than zero"))
        .action((s, c) => c.copy(numPhasers = s))
        .text("set the number of phasers to map tasks to"),
      arg[String]("file ...")
        .unbounded()
        .optional()
        .action((f, c) => c.copy(files = File.fromString(f) :: c.files))
        .text("files to translate"),
    )
  }

}
