package trochee.util

import virtualization.lms.common.BaseExp

/**
 * 
 * @author dlwh
 */
trait CStructExp { this: BaseExp =>
  case class FieldDeref[T,U:Manifest](lhs: Rep[T], field: String) extends Rep[U]

}
