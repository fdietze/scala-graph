package scalax.collection.edge

import scalax.collection.GraphEdge._,
       scalax.collection.GraphPredef._
import scalax.collection.Graph
import WBase._, LBase._

// ------------------------------------------------------------------------- W*
/** weighted, undirected hyperedge. */
@SerialVersionUID(70L)
class WHyperEdge[N](nodes: Product,
                    override val weight: Long)
  extends HyperEdge[N](nodes)
  with    WEdge    [N]
  with    EdgeCopy [WHyperEdge]
  with    OuterEdge[N,WHyperEdge]
{
  override protected[collection]
  def copy[NN](newNodes: Product) = WHyperEdge.newEdge[NN](newNodes, weight)(CollectionKind.from(this))
}
object WHyperEdge extends WHyperEdgeCompanion[WHyperEdge] {
  override protected def newEdge[N](nodes: Product, weight: Long)(implicit endpointsKind: CollectionKind): WHyperEdge[N] =
    if (endpointsKind.orderSignificant) new WHyperEdge[N](nodes, weight) with OrderedEndpoints
    else                                new WHyperEdge[N](nodes, weight)
}
/** weighted directed hyperedge. */
@SerialVersionUID(71L)
class WDiHyperEdge[N](nodes: Product,
                      override val weight: Long)
  extends DiHyperEdge[N](nodes)
  with    WEdge      [N]
  with    EdgeCopy   [WDiHyperEdge]
  with    OuterEdge  [N,WDiHyperEdge]
{
  override protected[collection] def copy[NN](newNodes: Product) =
    WDiHyperEdge.newEdge[NN](newNodes, weight)(CollectionKind.from(this))
}
object WDiHyperEdge extends WHyperEdgeCompanion[WDiHyperEdge] {
  override protected def newEdge[N](nodes: Product, weight: Long)(implicit endpointsKind: CollectionKind): WDiHyperEdge[N] =
    if (endpointsKind.orderSignificant) new WDiHyperEdge[N](nodes, weight) with OrderedEndpoints
    else                                new WDiHyperEdge[N](nodes, weight)
}
// ------------------------------------------------------------------------ Wk*
import WkBase._
/** key-weighted undirected hyperedge. */
abstract class WkHyperEdge[N](nodes: Product, weight: Long)
  extends WHyperEdge[N](nodes, weight)
  with    OuterEdge [N,WkHyperEdge]
  with    WkEdge    [N]
object WkHyperEdge extends WkHyperEdgeCompanion[WkHyperEdge] {
  protected class Wk[N](nodes: Product, weight: Long)(implicit endpointsKind: CollectionKind)
      extends WkHyperEdge[N](nodes, weight)
      with    EdgeCopy   [WkHyperEdge] { 
    override protected[collection] def copy[NN](newNodes: Product) = newEdge[NN](newNodes, weight)
  }
  override protected def newEdge[N](nodes: Product, weight: Long)
                                   (implicit endpointsKind: CollectionKind): WkHyperEdge[N]  with EdgeCopy[WkHyperEdge] = {
    if (endpointsKind.orderSignificant) new Wk[N](nodes, weight) with OrderedEndpoints
    else                                new Wk[N](nodes, weight)
  }
}
/** key-weighted directed hyperedge. */
abstract class WkDiHyperEdge[N](nodes: Product, weight: Long)
  extends WkHyperEdge    [N](nodes, weight)
  with    DiHyperEdgeLike[N]
  with    OuterEdge      [N,WkDiHyperEdge]
object WkDiHyperEdge extends WkHyperEdgeCompanion[WkDiHyperEdge] {
  protected class WkDi[N](nodes: Product, weight: Long)(implicit endpointsKind: CollectionKind)
      extends WkDiHyperEdge[N](nodes, weight)
      with    EdgeCopy[WkDiHyperEdge] { 
    override protected[collection] def copy[NN](newNodes: Product) = newEdge[NN](newNodes, weight)
  }
  override protected def newEdge[N](nodes: Product, weight: Long)
                                   (implicit endpointsKind: CollectionKind): WkDiHyperEdge[N] with EdgeCopy[WkDiHyperEdge] = {
    if (endpointsKind.orderSignificant) new WkDi[N](nodes, weight) with OrderedEndpoints
    else                                new WkDi[N](nodes, weight)
  }
}
// ------------------------------------------------------------------------- L*
/** labeled undirected hyperedge. */
abstract class LHyperEdge[N](nodes: Product)
  extends HyperEdge[N](nodes)
  with    OuterEdge[N,LHyperEdge]
  with    LEdge    [N]
object LHyperEdge extends LHyperEdgeCompanion[LHyperEdge] {
  override protected def newEdge[N,L](nodes: Product, pLabel: L)
                                     (implicit endpointsKind: CollectionKind): LHyperEdge[N] with EdgeCopy[LHyperEdge]{type L1 = L} = {
    if (endpointsKind.orderSignificant)
      new LHyperEdge[N](nodes) with EdgeCopy[LHyperEdge] with OrderedEndpoints { 
        type L1 = L
        override val label = pLabel
        override protected[collection] def copy[NN](newNodes: Product) = newEdge[NN,L](newNodes, pLabel)
      }
    else
      new LHyperEdge[N](nodes) with EdgeCopy[LHyperEdge] { 
        type L1 = L
        override val label = pLabel
        override protected[collection] def copy[NN](newNodes: Product) = newEdge[NN,L](newNodes, pLabel)
      }
  }
}
/** Labeled directed hyperedge. */
abstract class LDiHyperEdge[N](nodes: Product)
  extends LHyperEdge     [N](nodes) 
  with    DiHyperEdgeLike[N]
  with    OuterEdge      [N,LDiHyperEdge]
object LDiHyperEdge extends LHyperEdgeCompanion[LDiHyperEdge] {
  override protected def newEdge[N,L](nodes: Product, pLabel: L)
                                     (implicit endpointsKind: CollectionKind): LDiHyperEdge[N] with EdgeCopy[LDiHyperEdge]{type L1 = L} = {
    if (endpointsKind.orderSignificant)
      new LDiHyperEdge[N](nodes) with EdgeCopy[LDiHyperEdge] with OrderedEndpoints { 
        type L1 = L
        override val label = pLabel
        override protected[collection] def copy[NN](newNodes: Product) = newEdge[NN,L](newNodes, pLabel)
      }
    else
      new LDiHyperEdge[N](nodes) with EdgeCopy[LDiHyperEdge] { 
        type L1 = L
        override val label = pLabel
        override protected[collection] def copy[NN](newNodes: Product) = newEdge[NN,L](newNodes, pLabel)
      }
  }
}
// ------------------------------------------------------------------------ Lk*
import LkBase._
/** key-labeled undirected hyperedge. */
abstract class LkHyperEdge[N](nodes: Product)
  extends LHyperEdge[N](nodes) 
  with    OuterEdge [N,LkHyperEdge]
  with    LkEdge    [N]
object LkHyperEdge extends LkHyperEdgeCompanion[LkHyperEdge] {
  override protected def newEdge[N,L](nodes: Product, pLabel: L)
                                     (implicit endpointsKind: CollectionKind): LkHyperEdge[N] with EdgeCopy[LkHyperEdge]{type L1 = L} = {
    if (endpointsKind.orderSignificant)
      new LkHyperEdge[N](nodes) with EdgeCopy[LkHyperEdge] with OrderedEndpoints { 
        type L1 = L
        override val label = pLabel
        override protected[collection] def copy[NN](newNodes: Product) = newEdge[NN,L](newNodes, pLabel)
      }
    else
      new LkHyperEdge[N](nodes) with EdgeCopy[LkHyperEdge] { 
        type L1 = L
        override val label = pLabel
        override protected[collection] def copy[NN](newNodes: Product) = newEdge[NN,L](newNodes, pLabel)
      }
  }
}
/** key-labeled directed hyperedge. */
abstract class LkDiHyperEdge[N](nodes: Product)
  extends LDiHyperEdge[N](nodes) 
     with OuterEdge   [N,LkDiHyperEdge]
     with LkEdge      [N]
object LkDiHyperEdge extends LkHyperEdgeCompanion[LkDiHyperEdge] {
  override protected def newEdge[N,L](nodes: Product, pLabel: L)
                                     (implicit endpointsKind: CollectionKind): LkDiHyperEdge[N] with EdgeCopy[LkDiHyperEdge]{type L1 = L} = {
    if (endpointsKind.orderSignificant)
      new LkDiHyperEdge[N](nodes) with EdgeCopy[LkDiHyperEdge] with OrderedEndpoints { 
        type L1 = L
        override val label = pLabel
        override protected[collection] def copy[NN](newNodes: Product) = newEdge[NN,L](newNodes, pLabel)
      }
    else
      new LkDiHyperEdge[N](nodes) with EdgeCopy[LkDiHyperEdge] { 
        type L1 = L
        override val label = pLabel
        override protected[collection] def copy[NN](newNodes: Product) = newEdge[NN,L](newNodes, pLabel)
      }
  }
}
// ------------------------------------------------------------------------ WL*
import WLBase._
/** weighted, labeled undirected hyperedge. */
abstract class WLHyperEdge[N](nodes: Product, weight: Long)
  extends WHyperEdge[N](nodes, weight)
  with    OuterEdge [N,WLHyperEdge]
  with    LEdge     [N]
  with    WLEdge    [N]
object WLHyperEdge extends WLHyperEdgeCompanion[WLHyperEdge] {
  override protected def newEdge[N,L](nodes: Product, weight: Long, pLabel: L)
                                     (implicit endpointsKind: CollectionKind): WLHyperEdge[N] with EdgeCopy[WLHyperEdge] = {
    if (endpointsKind.orderSignificant)
      new WLHyperEdge[N](nodes, weight) with EdgeCopy[WLHyperEdge] with OrderedEndpoints { 
        type L1 = L
        override val label = pLabel
        override protected[collection] def copy[NN](newNodes: Product) = newEdge[NN,L](newNodes, weight, pLabel)
      }
    else
      new WLHyperEdge[N](nodes, weight) with EdgeCopy[WLHyperEdge] { 
        type L1 = L
        override val label = pLabel
        override protected[collection] def copy[NN](newNodes: Product) = newEdge[NN,L](newNodes, weight, pLabel)
      }
  }
}
/** weighted, labeled directed hyperedge. */
abstract class WLDiHyperEdge[N](nodes: Product, weight: Long)
  extends WLHyperEdge    [N](nodes, weight) 
  with    DiHyperEdgeLike[N]
  with    OuterEdge         [N,WLDiHyperEdge]
object WLDiHyperEdge extends WLHyperEdgeCompanion[WLDiHyperEdge] {
  override protected def newEdge[N,L](nodes: Product, weight: Long, pLabel: L)
                                     (implicit endpointsKind: CollectionKind): WLDiHyperEdge[N] with EdgeCopy[WLDiHyperEdge] = {
    if (endpointsKind.orderSignificant)
      new WLDiHyperEdge[N](nodes, weight) with EdgeCopy[WLDiHyperEdge] with OrderedEndpoints { 
        type L1 = L
        override val label = pLabel
        override protected[collection] def copy[NN](newNodes: Product) = newEdge[NN,L](newNodes, weight, pLabel)
      }
    else
      new WLDiHyperEdge[N](nodes, weight) with EdgeCopy[WLDiHyperEdge] { 
        type L1 = L
        override val label = pLabel
        override protected[collection] def copy[NN](newNodes: Product) = newEdge[NN,L](newNodes, weight, pLabel)
      }
  }
}
// ----------------------------------------------------------------------- WkL*
import WkLBase._
/** key-weighted, labeled undirected hyperedge. */
abstract class WkLHyperEdge[N](nodes: Product, weight: Long)
  extends WLHyperEdge[N](nodes, weight)
  with    WkEdge     [N]
  with    OuterEdge  [N,WkLHyperEdge]
object WkLHyperEdge extends WkLHyperEdgeCompanion[WkLHyperEdge] {
  override protected def newEdge[N,L](nodes: Product, weight: Long, pLabel: L)
                                     (implicit endpointsKind: CollectionKind): WkLHyperEdge[N] with EdgeCopy[WkLHyperEdge] = {
    if (endpointsKind.orderSignificant)
      new WkLHyperEdge[N](nodes, weight) with EdgeCopy[WkLHyperEdge] with OrderedEndpoints { 
        type L1 = L
        override val label = pLabel
        override protected[collection] def copy[NN](newNodes: Product) = newEdge[NN,L](newNodes, weight, pLabel)
      }
    else
      new WkLHyperEdge[N](nodes, weight) with EdgeCopy[WkLHyperEdge] { 
        type L1 = L
        override val label = pLabel
        override protected[collection] def copy[NN](newNodes: Product) = newEdge[NN,L](newNodes, weight, pLabel)
      }
  }
}
/** key-weighted, labeled directed hyperedge. */
abstract class WkLDiHyperEdge[N](nodes: Product, weight: Long)
  extends WkLHyperEdge   [N](nodes, weight) 
  with    DiHyperEdgeLike[N]
  with    OuterEdge      [N,WkLDiHyperEdge]
object WkLDiHyperEdge extends WkLHyperEdgeCompanion[WkLDiHyperEdge] {
  override protected def newEdge[N,L](nodes: Product, weight: Long, pLabel: L)
                                     (implicit endpointsKind: CollectionKind): WkLDiHyperEdge[N] with EdgeCopy[WkLDiHyperEdge] = {
    if (endpointsKind.orderSignificant)
      new WkLDiHyperEdge[N](nodes, weight) with EdgeCopy[WkLDiHyperEdge] with OrderedEndpoints { 
        type L1 = L
        override val label = pLabel
        override protected[collection] def copy[NN](newNodes: Product) = newEdge[NN,L](newNodes, weight, pLabel)
      }
    else
      new WkLDiHyperEdge[N](nodes, weight) with EdgeCopy[WkLDiHyperEdge] { 
        type L1 = L
        override val label = pLabel
        override protected[collection] def copy[NN](newNodes: Product) = newEdge[NN,L](newNodes, weight, pLabel)
      }
  }
}
// ----------------------------------------------------------------------- WLk*
import WLkBase._
/** weighted, key-labeled undirected hyperedge. */
abstract class WLkHyperEdge[N](nodes: Product, weight: Long)
  extends WLHyperEdge[N](nodes, weight) 
  with    OuterEdge[N,WLkHyperEdge]
  with    LkEdge   [N]
  with    WLkEdge  [N]
object WLkHyperEdge extends WLkHyperEdgeCompanion[WLkHyperEdge] {
  override protected def newEdge[N,L](nodes: Product, weight: Long, pLabel: L)
                                     (implicit endpointsKind: CollectionKind): WLkHyperEdge[N] with EdgeCopy[WLkHyperEdge] = {
    if (endpointsKind.orderSignificant)
      new WLkHyperEdge[N](nodes, weight) with EdgeCopy[WLkHyperEdge] with OrderedEndpoints { 
        type L1 = L
        override val label = pLabel
        override protected[collection] def copy[NN](newNodes: Product) = newEdge[NN,L](newNodes, weight, pLabel)
      }
    else
      new WLkHyperEdge[N](nodes, weight) with EdgeCopy[WLkHyperEdge] { 
        type L1 = L
        override val label = pLabel
        override protected[collection] def copy[NN](newNodes: Product) = newEdge[NN,L](newNodes, weight, pLabel)
      }
  }
}
/** weighted, key-labeled directed hyperedge. */
abstract class WLkDiHyperEdge[N](nodes: Product, weight: Long)
  extends WLkHyperEdge   [N](nodes, weight) 
  with    DiHyperEdgeLike[N]
  with    OuterEdge      [N,WLkDiHyperEdge]
object WLkDiHyperEdge extends WLkHyperEdgeCompanion[WLkDiHyperEdge] {
  override protected def newEdge[N,L](nodes: Product, weight: Long, pLabel: L)
                                     (implicit endpointsKind: CollectionKind): WLkDiHyperEdge[N] with EdgeCopy[WLkDiHyperEdge] = {
    if (endpointsKind.orderSignificant)
      new WLkDiHyperEdge[N](nodes, weight) with EdgeCopy[WLkDiHyperEdge] with OrderedEndpoints { 
        type L1 = L
        override val label = pLabel
        override protected[collection] def copy[NN](newNodes: Product) = newEdge[NN,L](newNodes, weight, pLabel)
      }
    else
      new WLkDiHyperEdge[N](nodes, weight) with EdgeCopy[WLkDiHyperEdge] { 
        type L1 = L
        override val label = pLabel
        override protected[collection] def copy[NN](newNodes: Product) = newEdge[NN,L](newNodes, weight, pLabel)
      }
  }
}
// ---------------------------------------------------------------------- WkLk*
import WkLkBase._
/** key-weighted, key-labeled undirected hyperedge. */
abstract class WkLkHyperEdge[N](nodes: Product, weight: Long)
  extends WLHyperEdge[N](nodes, weight) 
  with    OuterEdge  [N,WkLkHyperEdge]
  with    WkLkEdge   [N]
object WkLkHyperEdge extends WkLkHyperEdgeCompanion[WkLkHyperEdge] {
  override protected def newEdge[N,L](nodes: Product, weight: Long, pLabel: L)
                                     (implicit endpointsKind: CollectionKind): WkLkHyperEdge[N] with EdgeCopy[WkLkHyperEdge] = {
    if (endpointsKind.orderSignificant)
      new WkLkHyperEdge[N](nodes, weight) with EdgeCopy[WkLkHyperEdge] with OrderedEndpoints { 
        type L1 = L
        override val label = pLabel
        override protected[collection] def copy[NN](newNodes: Product) = newEdge[NN,L](newNodes, weight, pLabel)
      }
    else
      new WkLkHyperEdge[N](nodes, weight) with EdgeCopy[WkLkHyperEdge] { 
        type L1 = L
        override val label = pLabel
        override protected[collection] def copy[NN](newNodes: Product) = newEdge[NN,L](newNodes, weight, pLabel)
      }
  }
}
/** key-weighted, key-labeled directed hyperedge. */
abstract class WkLkDiHyperEdge[N](nodes: Product, weight: Long)
  extends WkLkHyperEdge  [N](nodes, weight) 
  with    DiHyperEdgeLike[N]
  with    OuterEdge      [N,WkLkDiHyperEdge]
object WkLkDiHyperEdge extends WkLkHyperEdgeCompanion[WkLkDiHyperEdge] {
  override protected def newEdge[N,L](nodes: Product, weight: Long, pLabel: L)
                                     (implicit endpointsKind: CollectionKind): WkLkDiHyperEdge[N] with EdgeCopy[WkLkDiHyperEdge] = {
    if (endpointsKind.orderSignificant)
      new WkLkDiHyperEdge[N](nodes, weight) with EdgeCopy[WkLkDiHyperEdge] with OrderedEndpoints { 
        type L1 = L
        override val label = pLabel
        override protected[collection] def copy[NN](newNodes: Product) = newEdge[NN,L](newNodes, weight, pLabel)
      }
    else
      new WkLkDiHyperEdge[N](nodes, weight) with EdgeCopy[WkLkDiHyperEdge] { 
        type L1 = L
        override val label = pLabel
        override protected[collection] def copy[NN](newNodes: Product) = newEdge[NN,L](newNodes, weight, pLabel)
      }
  }
}