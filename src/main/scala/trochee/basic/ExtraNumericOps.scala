package trochee.basic

import scala.virtualization.lms.common.{BaseExp, Base}
import scala.reflect.SourceContext

/**
 * TODO
 *
 * @author dlwh
 **/
trait ExtraNumericOps { this : Base =>
  implicit class MoreNumericOperations[T:Manifest:Numeric](v: Rep[T]) {
    def %(other: Rep[T])(implicit pos: SourceContext):Rep[T] = infix_%(v, other)(implicitly, implicitly, pos)
    def **(other: Rep[T])(implicit pos: SourceContext):Rep[T] = infix_**(v, other)(implicitly, implicitly, pos)
  }

  def infix_%[T:Manifest:Numeric](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext):Rep[T]
  def infix_**[T:Manifest:Numeric](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext):Rep[T]
  def infix_boolean_^(lhs: Rep[Boolean], rhs: Rep[Boolean])(implicit pos: SourceContext):Rep[Boolean]

  implicit class MoreBooleanOps(v: Rep[Boolean]) {
    def ^(other: Rep[Boolean])(implicit pos: SourceContext):Rep[Boolean] = infix_boolean_^(v, other)(pos)
  }

}


trait ExtraNumericOpsExp extends ExtraNumericOps { this : BaseExp =>
  def infix_%[T:Manifest:Numeric](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext):Rep[T] = ExtraNumericMod(lhs, rhs)(implicitly, implicitly, pos)

  def infix_**[T: Manifest : Numeric](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[T] = ExtraNumericPow(lhs, rhs)(implicitly, implicitly, pos)

  case class ExtraNumericMod[T:Manifest:Numeric](lhs: Rep[T], rhs: Rep[T])(implicit val pos: SourceContext) extends Def[T]
  case class ExtraNumericPow[T:Manifest:Numeric](lhs: Rep[T], rhs: Rep[T])(implicit val pos: SourceContext) extends Def[T] {
    def manifest = implicitly[Manifest[Rep[T]]]
  }

  case class ExtraNumericXor(lhs: Rep[Boolean], rhs: Rep[Boolean])(implicit val pos: SourceContext) extends Def[Boolean]

  def infix_boolean_^(lhs: Rep[Boolean], rhs: Rep[Boolean])(implicit pos: SourceContext): Rep[Boolean] = ExtraNumericXor(lhs, rhs)(pos)
}
