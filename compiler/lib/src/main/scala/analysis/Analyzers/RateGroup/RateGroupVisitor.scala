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
      _ <- annotations.headOption match {
        case Some(str) =>
          PeriodParser.parse(str) match {
            case Right(time) =>
              println(s"Parsed time: $time") // will be MS(1), US(5), etc.
              Right(())
            case Left(error) =>
              println(error)
              Right(())
          }
        case None => Right(())
      }
    } yield s
  }

}