package scalasemweb.rdf.model.collections

import scalasemweb.rdf.model._
import scala.annotation._

/////////////////////////////////////////////////////////////////////////////////////////////
// Collections

trait RDFList extends StatementSet {
  def _first : Value
  def _rest : RDFList  
  def node : Resource
  def toValueList : List[Value] = _first :: _rest.toValueList
    
}

object RDFNil extends StdStatementSet(Set[Statement]()) with RDFList {
  def _first = throw new java.util.NoSuchElementException()
  def _rest = throw new java.util.NoSuchElementException()
  def node = RDF.nil
  override def toValueList : List[Value] = Nil
}

object RDFList {
  def apply(vals : Iterable[Value]) : RDFList = {
    if(vals.isEmpty) {
      RDFNil
    } else {
      val f = vals.head
      val r = apply(vals.tail)
      val n = AnonymousNode
      new StdStatementSet(Set(n %> RDF.first %> f, n %> RDF.rest %> r.node)) with RDFList {
        def _first = f
        def _rest = r
        def node = n
      }
    }
  }
  def getList(res : Resource, statSet : StatementSet) : Option[RDFList] = {
    val restTrip = statSet.get(Some(res),Some(RDF.rest),None)
    val firstTrip = statSet.get(Some(res),Some(RDF.first),None)
    if(restTrip.size == 1 && firstTrip.size == 1) {
      val r = restTrip.head.obj
      val f = firstTrip.head.obj
      if(r == RDF.nil) {
        Some(new StdStatementSet(Set(restTrip.head, firstTrip.head)) with RDFList {
          def _first = f
          def _rest = RDFNil
          def node = res
        })
      } else {
        r match {
          case r3 : Resource => getList(r3,statSet) match {
            case Some(r2) => Some(new StdStatementSet(Set(restTrip.head, firstTrip.head)) with RDFList {
              def _first = f
              def _rest = r2
              def node = res
            })
            case None => None
          }
          case x => throw new RDFCollectionException("The next value of a List was not a resource")
        }
      }
    } else {
      if(res == RDF.nil) {
        Some(RDFNil)
      } else {        
        None
      }
    }
  }       
    
  def unapplySeq(statSet : StatementSet) : Option[Seq[Value]] = {
    @tailrec def revBuildList(stats : StatementSet, node : Resource, vals : List[Value]) : Option[List[Value]] = {
      val restTrip = statSet.get(None,Some(RDF.rest),Some(node))
      if(restTrip.size == 1) {
        val n = restTrip.head.subj
        val firstTrip = statSet.get(Some(n),Some(RDF.first),None)
        if(firstTrip.size == 1) {
          revBuildList(stats - restTrip.head - firstTrip.head, n, firstTrip.head.obj :: vals)          
        } else {
          None
        }
      } else {
        if(stats.isEmpty) {
          Some(vals)
        } else {
          None
        }
      }   
    }
    revBuildList(statSet,RDF.nil,Nil)   
  }
}

class RDFCollectionException(message : String) extends RuntimeException(message)
