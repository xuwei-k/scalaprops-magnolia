package scalaprops

import scalaz.{Endo, Monoid}
import magnolia.{CaseClass, Magnolia, SealedTrait}

object ScalapropsMagnoliaCogen {
  type Typeclass[T] = scalaprops.Cogen[T]

  private[this] val MonoidEndoRand = Monoid[Endo[Rand]]
  private[this] val RandNextEndo = Endo[Rand](_.next)

  def combine[T](ctx: CaseClass[Typeclass, T]): Typeclass[T] =
    new Cogen[T] {
      def cogen[B](t: T, g: CogenState[B]) = {
        ctx.parameters.zipWithIndex.foldLeft(g) {
          case (state, (p, i)) =>
            Cogen[Int].cogen(
              i,
              Cogen[String].cogen(
                p.label,
                p.typeclass.cogen(p.dereference(t), state)))
        }
      }
    }

  def dispatch[T](ctx: SealedTrait[Typeclass, T]): Typeclass[T] =
    new Cogen[T] {
      def cogen[B](t: T, g: CogenState[B]) = {
        val index = ctx.subtypes.indexWhere(_.cast.isDefinedAt(t))
        if (index >= 0) {
          val s = ctx.subtypes(index)
          val r = MonoidEndoRand.multiply(RandNextEndo, index + s.typeName.full.## % 31).run(g.rand)
          Cogen[String].cogen(s.typeName.full, s.typeclass.cogen(s.cast(t), g.copy(rand = r)))
        } else {
          sys.error(s"bug? $ctx $t $g $index")
        }
      }
    }

  implicit def derivingScalapropsCogen[T]: Typeclass[T] =
    macro Magnolia.gen[T]
}
