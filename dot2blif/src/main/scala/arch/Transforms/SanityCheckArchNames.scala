package arch

object SanityCheckArchNames {
  val keywords = Set("bypass", "bypassOut", "bypassIn")

  def apply(t: Tile): Unit = {
    def rec(aMode: AbsMode): Seq[String] = {
      aMode match {
        case m: PrimMode => {
          if (keywords contains m.name) {
            m.name :: Nil
          } else {
            m.name :: Nil
          }
        }

        case m: Mode => {
          m.name :: m.modes.map(rec(_)).flatten
        }
      }
    }

    val allNames = t.name :: t.modes.map(rec(_)).flatten
    allNames
      .groupBy(
        n => n
      )
      .foreach {
        (name, matches) =>
          {
            assert((matches.size == 1) || (keywords contains name), name + " name is duplicated in the architecture")
          }
      }
  }
}
