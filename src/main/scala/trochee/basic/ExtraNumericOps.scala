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

  }

  def infix_%[T:Manifest:Numeric](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext):Rep[T]

}


trait ExtraNumericOpsExp extends ExtraNumericOps { this : BaseExp =>
  def infix_%[T:Manifest:Numeric](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext):Rep[T] = ExtraNumericMod(lhs, rhs)(implicitly, implicitly, pos)
  case class ExtraNumericMod[T:Manifest:Numeric](lhs: Rep[T], rhs: Rep[T])(implicit val pos: SourceContext) extends Def[T]
}