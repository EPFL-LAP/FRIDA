package arch

// is there to prevent the | tree with empty interconnect on top
// Then there are not weird edge cases in FindMolecules, etc...

object SimplifyAbsArch {
  def apply(t: Tile): Tile = {
    def rec(aMode: AbsMode, isBypassed: Boolean): List[AbsMode] = {
      aMode match {
        case p: PrimMode                            => p :: Nil
        case m: Mode if (m.name.contains("bypass")) => m :: Nil
        case m: Mode => {
          val recByp = m.modes.exists(Bypass.isModeBypass(_)) && false
          val recModes = m.modes.map(rec(_, recByp)).flatten

          if (
            (m.combinator == TileOr)
            && (recModes.head.combinator == m.combinator)
            // && (!m.annotations.contains(AMolecule))
            && (!isBypassed)
          ) {
            println("Flattening mode: " + m.name)
            recModes
          } else {
            Mode(m.name, recModes, m.combinator, m.locConfig, m.annotations, m.castedPorts, m.dagId) :: Nil
          }

        }
      }
    }

    val recModes = t.modes.map(rec(_, false)).flatten
    Tile(t.name, recModes, t.annotations, t.vprConfig)
  }
}
