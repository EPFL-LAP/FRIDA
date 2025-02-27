package crkt

import arch.BlockPortID
import arch.PortMeaningWrapper
import arch.PortType
import arch.PTInput
import arch.PTOutput
import arch.Hs
import archs.Exit
import archs.Entry

object CrktChecks {
  def sanityCheck(nodes: List[TNode]): Unit = {
    nodes.foreach {
      n =>
        {
          n.ports.foreach {
            (id, ports) =>
              {
                ports.foreach {
                  p =>
                    {
                      val exitOut = (n.nType == Exit) && (p.pt == PTOutput)
                      val entryIn = (n.nType == Entry) && (p.pt == PTInput)

                      if (!exitOut && !entryIn) {
                        if (p.distPorts.isEmpty) {
                          val hsID = BlockPortID(p.width, p.pt, PortMeaningWrapper(p.pm, Hs), p.dummy)

                          if (!n.ports.contains(hsID)) {
                            // println(n)
                            // println(p.distPorts)
                            assert(p.distPorts.nonEmpty)
                          }
                        }

                        p.distPorts.foreach {
                          (id, dp: Port) =>
                            {
                              if (!dp.distPorts.contains(p.nodeID())) {
                                println("----")
                                println(p.toString() + " -> " + dp.toString())
                                println("" + p.nodeID() + " not in " + dp.distPorts.keySet)
                                println(dp.distPorts.mkString("\n"))
                              }

                              assert(dp.distPorts contains p.nodeID())
                            }
                        }

                        p.distPorts.foreach {
                          (id, dp: Port) =>
                            {
                              assert(p.pt != dp.pt)
                            }
                        }
                      }
                    }
                }
              }
          }
        }
    }
  }
}
