package fpp.compiler.codegen

import fpp.compiler.ast._
import fpp.compiler.util._

object PhaserCppWriter extends CppWriter {

  override def tuList(s: State, tul: List[Ast.TransUnit]): Result.Result[Unit] =
    for {
      _ <- {
        println("In tuList!")
        Right(())
      }
      _ <- super.tuList(s, tul)
    }
    yield ()

  override def defModuleAnnotatedNode(
    s: State,
    aNode: Ast.Annotated[AstNode[Ast.DefModule]]
  ) = {
    val node = aNode._2
    val data = node.data
    println("In defModuleAnnotatedNode!")
    visitList(s, data.members, matchModuleMember)
  }

  override def defComponentInstanceAnnotatedNode(
    s: State, aNode: Ast.Annotated[AstNode[Ast.DefComponentInstance]]
  ): Result = {
    for {
      _ <- {
        println("In defComponentInstanceAnnotatedNode!")
        Right(s)
      }
    }
    yield s
  }

}