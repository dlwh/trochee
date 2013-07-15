package trochee.breeze

import breeze.linalg.SparseVector
import scala.virtualization.lms.common._
import trochee.basic.ExtraBase
import breeze.math.Semiring
import scala.virtualization.lms.internal.Effects

/**
 * TODO
 *
 * @author dlwh
 **/
trait SparseVectorBuilderOps extends OpGeneratorOps with SparseVectorOps with BooleanOps { this: Base with ExtraBase with NumericOps with OrderingOps with RangeOps with Variables with Effects with LiftVariables with While with IfThenElse with PrimitiveOps =>

  def sparseVectorHelper[L:Manifest, R:Manifest, Res:Manifest:Semiring] = new VectorOpHelper[SparseVector[L], L, SparseVector[R], R, SparseVector[Res], Res] {

    def fullRange(lhs: Rep[SparseVector[L]], rhs: Rep[SparseVector[R]]): VectorBuilder[L, R, SparseVector[Res], Res] = {
      new VectorBuilder[L, R, SparseVector[Res], Res] {
        def map(f: (Rep[L], Rep[R]) => Rep[Res]): Rep[SparseVector[Res]] = {
          val builder: Rep[breeze.linalg.VectorBuilder[Res]] = newSparseVectorBuilder(lhs.length, lhs.length)
          for(i <- unit(0) until lhs.length) {
            builder.add(i, f(lhs(i), rhs(i)))
          }
          builder.toSparseVector(alreadySorted=unit(true), keysAlreadyUnique = unit(true))
        }
      }
    }

    def union(lhs: Rep[SparseVector[L]], rhs: Rep[SparseVector[R]]): VectorBuilder[L, R, SparseVector[Res], Res] = fullRange(lhs, rhs)

    def intersected(lhs: Rep[SparseVector[L]], rhs: Rep[SparseVector[R]]): VectorBuilder[L, R, SparseVector[Res], Res] = {
      new VectorBuilder[L, R, SparseVector[Res], Res] {
        def map(f: (Rep[L], Rep[R]) => Rep[Res]): Rep[SparseVector[Res]] = {
          val builder: Rep[breeze.linalg.VectorBuilder[Res]] = newSparseVectorBuilder(lhs.length, lhs.length)
          val ldata = lhs.data
          val rdata = rhs.data
          val lindex = lhs.index
          val rindex = rhs.index
          val lsize = lhs.activeSize
          val rsize = rhs.activeSize
          var lpos = unit(0)
          var rpos = unit(0)
          while(lpos < lsize && rpos < rsize) {
            val i = lindex(lpos)
            val newrpos = Arrays.binarySearch(rindex, rpos, rsize, i)
            if(newrpos < unit(0)) {
              rpos = ~newrpos
            } else {
              builder.add(i, f(ldata(lpos), rdata(rpos)))
              rpos = newrpos
            }
            lpos += 1
          }
          builder.toSparseVector(alreadySorted=unit(true), keysAlreadyUnique = unit(true))
        }
      }
    }

  }

  def sparseVectorScalarHelper[L:Manifest, R:Manifest, Res:Manifest:Semiring] = new VectorOpHelper[SparseVector[L], L, R, R, SparseVector[Res], Res] {

    def fullRange(lhs: Rep[SparseVector[L]], rhs: Rep[R]): VectorBuilder[L, R, SparseVector[Res], Res] = {
      new VectorBuilder[L, R, SparseVector[Res], Res] {
        def map(f: (Rep[L], Rep[R]) => Rep[Res]): Rep[SparseVector[Res]] = {
          val builder: Rep[breeze.linalg.VectorBuilder[Res]] = newSparseVectorBuilder(lhs.length, lhs.length)
          for(i <- unit(0) until lhs.length) {
            builder.add(i, f(lhs(i), rhs))
          }
          builder.toSparseVector(alreadySorted=unit(true), keysAlreadyUnique = unit(true))
        }
      }
    }

    def union(lhs: Rep[SparseVector[L]], rhs: Rep[R]): VectorBuilder[L, R, SparseVector[Res], Res] = fullRange(lhs, rhs)

    def intersected(lhs: Rep[SparseVector[L]], rhs: Rep[R]): VectorBuilder[L, R, SparseVector[Res], Res] = fullRange(lhs, rhs)
  }


  def sparseVectorTransformer[L:Manifest:Semiring, R:Manifest] = new VectorTransformHelper[SparseVector[L], L, SparseVector[R], R] {
    val helper = sparseVectorHelper[L, R, L]

    def fullRange(lhs: Rep[SparseVector[L]], rhs: Rep[SparseVector[R]]): VectorUpdater[L, R] = {
      new VectorUpdater[L, R] {
        def updateLHS(f: (Rep[L], Rep[R]) => Rep[L]): Rep[Unit] = {
          val tmp = helper.fullRange(lhs, rhs).map(f)
          lhs.use(tmp.index, tmp.data, tmp.activeSize)
        }
      }
    }

    def union(lhs: Rep[SparseVector[L]], rhs: Rep[SparseVector[R]]): VectorUpdater[L, R] = {
      new VectorUpdater[L, R] {
        def updateLHS(f: (Rep[L], Rep[R]) => Rep[L]): Rep[Unit] = {
          val tmp = helper.union(lhs, rhs).map(f)
          lhs.use(tmp.index, tmp.data, tmp.activeSize)
        }
      }
    }

    def intersected(lhs: Rep[SparseVector[L]], rhs: Rep[SparseVector[R]]): VectorUpdater[L, R] = {
      new VectorUpdater[L, R] {
        def updateLHS(f: (Rep[L], Rep[R]) => Rep[L]): Rep[Unit] = {
          val tmp = helper.intersected(lhs, rhs).map(f)
          lhs.use(tmp.index, tmp.data, tmp.activeSize)
        }
      }
    }
  }

  def sparseVectorScalarTransformer[L:Manifest:Semiring, R:Manifest] = new VectorTransformHelper[SparseVector[L], L, R, R] {
    val helper = sparseVectorScalarHelper[L, R, L]

    def fullRange(lhs: Rep[SparseVector[L]], rhs: Rep[R]): VectorUpdater[L, R] = {
      new VectorUpdater[L, R] {
        def updateLHS(f: (Rep[L], Rep[R]) => Rep[L]) = {
          val tmp = helper.fullRange(lhs, rhs).map(f)
          lhs.use(tmp.index, tmp.data, tmp.activeSize)
        }
      }
    }

    def union(lhs: Rep[SparseVector[L]], rhs: Rep[R]): VectorUpdater[L, R] = fullRange(lhs, rhs)

    def intersected(lhs: Rep[SparseVector[L]], rhs: Rep[R]): VectorUpdater[L, R] = fullRange(lhs, rhs)
  }



}
