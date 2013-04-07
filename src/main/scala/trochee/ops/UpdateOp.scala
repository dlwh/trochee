package trochee.ops

/**
 * 
 * @author dlwh
 */
trait UpdateOp[Arr, I, V, R] {
  def apply(a: Arr, i: I, v: V):R
}
