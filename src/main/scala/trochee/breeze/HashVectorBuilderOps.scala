package trochee.breeze

import breeze.linalg.HashVector
import scala.virtualization.lms.common._
import trochee.basic.{ExtraNumericOps, ExtraBase}
import breeze.math.Semiring
import scala.virtualization.lms.internal.Effects
import trochee.basic.collection.BitSetOps


/**
 * TODO
 *
 * @author dlwh
 **/
trait HashVectorBuilderOps extends OpGeneratorOps with HashVectorOps with BooleanOps with BitSetOps { this: Base with ExtraBase with NumericOps with OrderingOps with RangeOps with Variables with Effects with LiftVariables with While with IfThenElse with PrimitiveOps with ExtraNumericOps =>

  def hashVectorHelper[L:Manifest:Semiring, R:Manifest:Semiring, Res:Manifest:Semiring] = new VectorOpHelper[HashVector[L], L, HashVector[R], R, HashVector[Res], Res] {

    def fullRange(lhs: Rep[HashVector[L]], rhs: Rep[HashVector[R]]): VectorBuilder[L, R, HashVector[Res], Res] = {
      new VectorBuilder[L, R, HashVector[Res], Res] {
        def map(f: (Rep[L], Rep[R]) => Rep[Res]): Rep[HashVector[Res]] = {
          val builder: Rep[breeze.linalg.VectorBuilder[Res]] = newHashVectorBuilder(lhs.length, lhs.length)
          for(i <- unit(0) until lhs.length) {
            builder.add(i, f(lhs(i), rhs(i)))
          }
          builder.toHashVector
        }
      }
    }

    def union(lhs: Rep[HashVector[L]], rhs: Rep[HashVector[R]]): VectorBuilder[L, R, HashVector[Res], Res] = {
      new VectorBuilder[L, R, HashVector[Res], Res] {
        def map(f: (Rep[L], Rep[R]) => Rep[Res]): Rep[HashVector[Res]] = {
          val lsize = lhs.activeSize
          val rsize = rhs.activeSize
          val builder: Rep[breeze.linalg.VectorBuilder[Res]] = newHashVectorBuilder(lhs.length, (lsize + rsize).min(lhs.length))
          val hitKeys = newBitSet()
          val ldata = lhs.data
          val lindex = lhs.index
          var lpos = unit(0)
          while(lpos < lsize) {
            if(lhs.isActive(lpos)) {
              val i = lindex(lpos)
              builder.add(i, f(ldata(lpos), rhs(i)))
              hitKeys += i
            }

            lpos += 1
          }

          val rdata = rhs.data
          val rindex = rhs.index
          var rpos = unit(0)
          while(rpos < rsize) {
            if(rhs.isActive(rpos)) {
              val i = rindex(rpos)
              if( !hitKeys(i))
                builder.add(i, f(lhs(i), rdata(rpos)))
            }

            rpos += 1
          }


          builder.toHashVector
        }
      }
    }

    def intersected(lhs: Rep[HashVector[L]], rhs: Rep[HashVector[R]]): VectorBuilder[L, R, HashVector[Res], Res] = {
      new VectorBuilder[L, R, HashVector[Res], Res] {
        def map(f: (Rep[L], Rep[R]) => Rep[Res]): Rep[HashVector[Res]] = {
          val lsize = lhs.iterableSize
          val rsize = rhs.iterableSize
          val builder: Rep[breeze.linalg.VectorBuilder[Res]] = newHashVectorBuilder(lhs.length, lhs.activeSize min rhs.activeSize)

          if(lsize < rsize) {
            val ldata = lhs.data
            val lindex = lhs.index
            var lpos = unit(0)
            while(lpos < lsize) {
              if(lhs.isActive(lpos)) {
                val i = lindex(lpos)
                builder.add(i, f(ldata(lpos), rhs(i)))
              }

              lpos += 1
            }
          } else {
            val rdata = rhs.data
            val rindex = rhs.index
            var rpos = unit(0)
            while(rpos < rsize) {
              if(rhs.isActive(rpos)) {
                val i = rindex(rpos)
                builder.add(i, f(lhs(i), rdata(rpos)))
              }

              rpos += 1
            }

          }

          builder.toHashVector
        }
      }
    }

  }

  def hashVectorScalarHelper[L:Manifest, R:Manifest, Res:Manifest:Semiring] = new VectorOpHelper[HashVector[L], L, R, R, HashVector[Res], Res] {

    def fullRange(lhs: Rep[HashVector[L]], rhs: Rep[R]): VectorBuilder[L, R, HashVector[Res], Res] = {
      new VectorBuilder[L, R, HashVector[Res], Res] {
        def map(f: (Rep[L], Rep[R]) => Rep[Res]): Rep[HashVector[Res]] = {
          val builder: Rep[breeze.linalg.VectorBuilder[Res]] = newHashVectorBuilder(lhs.length, lhs.length)
          for(i <- unit(0) until lhs.length) {
            builder.add(i, f(lhs(i), rhs))
          }
          builder.toHashVector
        }
      }
    }

    def union(lhs: Rep[HashVector[L]], rhs: Rep[R]): VectorBuilder[L, R, HashVector[Res], Res] = fullRange(lhs, rhs)

    def intersected(lhs: Rep[HashVector[L]], rhs: Rep[R]): VectorBuilder[L, R, HashVector[Res], Res] = {
      new VectorBuilder[L, R, HashVector[Res], Res] {
        def map(f: (Rep[L], Rep[R]) => Rep[Res]): Rep[HashVector[Res]] = {
          val lsize = lhs.activeSize
          val builder: Rep[breeze.linalg.VectorBuilder[Res]] = newHashVectorBuilder(lhs.length, lsize)
          val ldata = lhs.data
          val lindex = lhs.index
          var lpos = unit(0)
          while(lpos < lsize) {
            if(lhs.isActive(lpos)) {
              val i = lindex(lpos)
              builder.add(i, f(ldata(lpos), rhs))
            }
            lpos += 1
          }
          builder.toHashVector
        }
      }

    }
  }

  def hashVectorDotProductHelper[L:Manifest:Semiring, R:Manifest:Semiring, Res:Numeric:Manifest:Semiring] = new VectorOpHelper[HashVector[L], L, HashVector[R], R, Res, Res] {

    def fullRange(lhs: Rep[HashVector[L]], rhs: Rep[HashVector[R]]): VectorBuilder[L, R, Res, Res] = ???

    def union(lhs: Rep[HashVector[L]], rhs: Rep[HashVector[R]]): VectorBuilder[L, R, Res, Res] = ???

    def intersected(lhs: Rep[HashVector[L]], rhs: Rep[HashVector[R]]): VectorBuilder[L, R, Res, Res] = {
      new VectorBuilder[L, R, Res, Res] {
        def map(f: (Rep[L], Rep[R]) => Rep[Res]): Rep[Res] = {
          var res = unit(implicitly[Semiring[Res]].zero)
          val lsize = lhs.iterableSize
          val rsize = rhs.iterableSize

          if(lsize < rsize) {
            val ldata = lhs.data
            val lindex = lhs.index
            var lpos = unit(0)
            while(lpos < lsize) {
              if(lhs.isActive(lpos)) {
                val i = lindex(lpos)
                res += f(ldata(lpos), rhs(i))
              }

              lpos += 1
            }
          } else {
            val rdata = rhs.data
            val rindex = rhs.index
            var rpos = unit(0)
            while(rpos < rsize) {
              if(rhs.isActive(rpos)) {
                val i = rindex(rpos)
                res += f(lhs(i), rdata(rpos))
              }

              rpos += 1
            }

          }

          res
        }
      }
    }

  }



  def hashVectorTransformer[L:Manifest:Semiring, R:Manifest:Semiring] = new VectorTransformHelper[HashVector[L], L, HashVector[R], R] {
    val helper = hashVectorHelper[L, R, L]

    def fullRange(lhs: Rep[HashVector[L]], rhs: Rep[HashVector[R]]): VectorUpdater[L, R] = {
      new VectorUpdater[L, R] {
        def updateLHS(f: (Rep[L], Rep[R]) => Rep[L]): Rep[Unit] = {
          val tmp = helper.fullRange(lhs, rhs).map(f)
          lhs.use(tmp.index, tmp.data, tmp.activeSize)
        }
      }
    }

    def union(lhs: Rep[HashVector[L]], rhs: Rep[HashVector[R]]): VectorUpdater[L, R] = {
      new VectorUpdater[L, R] {
        def updateLHS(f: (Rep[L], Rep[R]) => Rep[L]): Rep[Unit] = {
          val tmp = helper.union(lhs, rhs).map(f)
          lhs.use(tmp.index, tmp.data, tmp.activeSize)
        }
      }
    }

    def intersected(lhs: Rep[HashVector[L]], rhs: Rep[HashVector[R]]): VectorUpdater[L, R] = {
      new VectorUpdater[L, R] {
        def updateLHS(f: (Rep[L], Rep[R]) => Rep[L]): Rep[Unit] = {
          val tmp = helper.intersected(lhs, rhs).map(f)
          lhs.use(tmp.index, tmp.data, tmp.activeSize)
        }
      }
    }
  }

  def hashVectorScalarTransformer[L:Manifest:Semiring, R:Manifest] = new VectorTransformHelper[HashVector[L], L, R, R] {
    val helper = hashVectorScalarHelper[L, R, L]

    def fullRange(lhs: Rep[HashVector[L]], rhs: Rep[R]): VectorUpdater[L, R] = {
      new VectorUpdater[L, R] {
        def updateLHS(f: (Rep[L], Rep[R]) => Rep[L]) = {
          val tmp = helper.fullRange(lhs, rhs).map(f)
          lhs.use(tmp.index, tmp.data, tmp.activeSize)
        }
      }
    }

    def union(lhs: Rep[HashVector[L]], rhs: Rep[R]): VectorUpdater[L, R] = {
      new VectorUpdater[L, R] {
        def updateLHS(f: (Rep[L], Rep[R]) => Rep[L]) = {
          val tmp = helper.union(lhs, rhs).map(f)
          lhs.use(tmp.index, tmp.data, tmp.activeSize)
        }
      }
    }

    def intersected(lhs: Rep[HashVector[L]], rhs: Rep[R]): VectorUpdater[L, R] = {
      new VectorUpdater[L, R] {
        def updateLHS(f: (Rep[L], Rep[R]) => Rep[L]) = {
          val tmp = helper.union(lhs, rhs).map(f)
          lhs.use(tmp.index, tmp.data, tmp.activeSize)
        }
      }
    }
  }



}
