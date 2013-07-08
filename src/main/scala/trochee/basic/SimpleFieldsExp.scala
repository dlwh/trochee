package trochee.basic

import scala.virtualization.lms.common.BaseExp
import scala.reflect.SourceContext

/**
 * TODO
 *
 * @author dlwh
 **/
trait SimpleFieldsExp { this: BaseExp =>
  case class FieldDeref[T:Manifest, R:Manifest](v: Rep[T], fieldName: String)(implicit val pos: SourceContext) extends Def[R]
  case class NewObject[T:Manifest](_args: Rep[Any]*)(implicit val pos: SourceContext) extends Def[T] {
    def args = _args

  }

}
