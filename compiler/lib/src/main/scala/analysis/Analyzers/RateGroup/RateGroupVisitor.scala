package fpp.compiler.analysis

import fpp.compiler.ast._
import fpp.compiler.ast.Ast.SpecConnectionGraph
import fpp.compiler.util._

case class RateGroupState(
    analysis: Analysis = Analysis(), // Analysis object from CheckSemantics 
    periodMap: Map[Symbol.ComponentInstance, Time] = Map(), // Map from a rate group instance name to its period.
    offsetMap: Map[Symbol.ComponentInstance, Time] = Map(), // Map from a rate group instance name to its offset.
    taskMap: Map[Symbol.ComponentInstance, List[PortInstanceIdentifier]] = Map() // Map from a rate group instance to a list of ports it calls within a period.
)

object RateGroupVisitor extends AstStateVisitor {

  type State = RateGroupState

  override def transUnit(s: State, tu: Ast.TransUnit): Result.Result[State] =
    visitList(s, tu.members, matchTuMember)

  def tuList(s: State, tul: List[Ast.TransUnit]): Result.Result[State] =
    for {
      s <- visitList(s, tul, transUnit)
    } yield s

  override def defModuleAnnotatedNode(
    s: State,
    aNode: Ast.Annotated[AstNode[Ast.DefModule]]
  ) = {
    val node = aNode._2
    val data = node.data
    visitList(s, data.members, matchModuleMember)
  }

  override def defTopologyAnnotatedNode(
    s: State,
    aNode: Ast.Annotated[AstNode[Ast.DefTopology]]
  ) = {
    val node = aNode._2
    val data = node.data
    visitList(s, data.members, matchTopologyMember)
  }

  override def defComponentInstanceAnnotatedNode(
    s: State, aNode: Ast.Annotated[AstNode[Ast.DefComponentInstance]]
  ) = {
    val node = aNode._2
    val postNotations = aNode._3
    for {
      s <- {
        postNotations.foldLeft[Result.Result[State]](Right(s)) {
          case (acc, str) =>
            acc.flatMap { state =>
              (PeriodParser.parse(str), OffsetParser.parse(str)) match {
                case (Right(period), _) =>
                  println(s"Period parsed: $period")
                  val periodMap = s.periodMap + (Symbol.ComponentInstance(aNode) -> period)
                  Right(state.copy(periodMap = periodMap))
                case (_, Right(offset)) =>
                  println(s"Offset parsed: $offset")
                  val offsetMap = state.offsetMap + (Symbol.ComponentInstance(aNode) -> offset)
                  Right(state.copy(offsetMap = offsetMap))
                case (Left(err1), Left(err2)) =>
                  println(s"Failed to parse: $str")
                  println(s"Errors: $err1 | $err2")
                  Right(state)
              }
            }
        }
      }
    } yield s
  }

  override def specConnectionGraphAnnotatedNode(
    s: State, aNode: Ast.Annotated[AstNode[Ast.SpecConnectionGraph]]
  ) = {
    val node = aNode._2
    val data = node.data
    for {
      s <- {
        // println(s.analysis.useDefMap)
        data match {
          case SpecConnectionGraph.Direct(name, connections) => {
            println(s"$name, $connections")
            connections.map { connection => {
              // Get fromPort, check if it comes from an active rate group,
              // and if it is a non-special output port with Sched type.
              // Assumption: The active rate group only has one
              // non-special output port with Sched type.
              // This assumption must be satisfied by the implementation of
              // active rate groups.
              val fromPort = connection.fromPort
              val fromIndex = connection.fromIndex
              // Get port def from port instance identifier
              // val portInstance = s.analysis.getComponentInstance(fromPort.id)
              // val portDef = s.analysis.useDefMap(fromPort.id)

              // If so, get the rate group instance, then add a toPort to the
              // list mapped from the rate group instance. 
            }}
            Right(s)
          }
          case SpecConnectionGraph.Pattern(kind, source, targets) => Right(s)
        }
      }
    } yield s
  }

}