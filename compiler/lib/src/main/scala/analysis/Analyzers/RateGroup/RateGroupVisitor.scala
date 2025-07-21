package fpp.compiler.analysis

import fpp.compiler.ast._
import fpp.compiler.util._

case class RateGroupState(
    periodMap: Map[String, String] = Map()
)

object RateGroupVisitor extends AstStateVisitor {

  type State = RateGroupState

  override def transUnit(s: State, tu: Ast.TransUnit) =
    visitList(s, tu.members, matchTuMember)

  def tuList(s: State, tul: List[Ast.TransUnit]): Result.Result[Unit] =
    for {
      _ <- visitList(s, tul, transUnit)
    } yield ()

  override def defModuleAnnotatedNode(
    s: State,
    aNode: Ast.Annotated[AstNode[Ast.DefModule]]
  ) = {
    val node = aNode._2
    val data = node.data
    visitList(s, data.members, matchModuleMember)
  }

  override def defComponentInstanceAnnotatedNode(
    s: State, aNode: Ast.Annotated[AstNode[Ast.DefComponentInstance]]
  ): Result = {
    val annotations = aNode._3
    for {
      _ <- {
        annotations.foldLeft[Result.Result[Unit]](Right(())) {
          case (acc, str) =>
            acc.flatMap { _ =>
              (PeriodParser.parse(str), OffsetParser.parse(str)) match {
                case (Right(period), _) =>
                  println(s"Period parsed: $period")
                  Right(())
                case (_, Right(offset)) =>
                  println(s"Offset parsed: $offset")
                  Right(())
                case (Left(err1), Left(err2)) =>
                  println(s"Failed to parse: $str")
                  println(s"Errors: $err1 | $err2")
                  Right(())
              }
            }
        }
      }
    } yield s
  }

}