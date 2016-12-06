package compiler

import scala.util.parsing.input.Position

trait CompilationError

case class LexerError(location: Location,message: String) extends CompilationError
case class ParserError(location: Location,message: String) extends CompilationError
case class TypeCheckError(location: Location,message: String) extends CompilationError

case class Location(position: Position) {
  override def toString = s"${position.line}:${position.column}"
}
