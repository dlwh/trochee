package trochee.codegen

import scala.virtualization.lms.internal._
import trochee.basic.SimpleFieldsExp
import trochee.util.NiceNamesGen

/**
 * TODO
 *
 * @author dlwh
 **/
trait ScalaSimpleFieldsGen extends NiceNamesGen { this: ScalaFatCodegen =>

  val IR: Expressions with Effects with FatExpressions with SimpleFieldsExp

  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case a@IR.FieldDeref(v, name) => quote(addPos(sym,a))

  }
}
