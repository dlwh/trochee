package trochee.basic

import scala.virtualization.lms.common.BaseExp
import scala.reflect.SourceContext
import scala.virtualization.lms.internal.Expressions

/**
 * TODO
 *
 * @author dlwh
 **/
trait SimpleFieldsExp extends BaseExp with Expressions {
  case class FieldDeref[T:Manifest, R:Manifest](v: Rep[T], fieldName: String)(implicit val pos: SourceContext) extends Def[R]
  case class NewObject[T:Manifest](args: Rep[Any]*)(implicit val pos: SourceContext) extends Def[T] {
    def tp = implicitly[Manifest[T]]
  }

  case class StaticMethodInvocation[T:Manifest](clss: String, method: String, targs: IndexedSeq[Manifest[_]], args: Rep[Any]*)(implicit val pos: SourceContext) extends Def[T] {
    def tp = implicitly[Manifest[T]]
  }

}
