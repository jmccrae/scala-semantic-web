package scalasemweb.owl.restrictions

import scalasemweb.rdf.collection._
import scala.collection._
import scalasemweb.owl._
import scalasemweb.rdf.model._


////////////////////////////////////////////////////////////////////////////////////////////////
// Restrictions

/** A restriction */
sealed abstract class OWLRestriction(resource : Resource, triples : TripleSet) 
  extends OWLClass(resource,triples) {
  
}

object OWLRestriction {
  def apply(resource : Resource, triples : TripleSet) : OWLRestriction = {
    triples.get(Some(resource),Some(RDF._type),Some(OWL.Restriction)) headOption match {
      case Some(_) =>
      case None => throw new OWLNoSuchEntityException
    }
    triples.get(Some(resource),Some(OWL.onProperty),None) headOption match {
      case Some(_ %> _ %> (p : NamedNode)) => { // Unary Restriction
        try {
          val prop = OWLObjectProperty(p,triples)
          // Object property restriction
          applyObjProp(resource,triples,prop)
        } catch {
          case x : OWLNoSuchEntityException => 
            val prop = OWLDatatypeProperty(p,triples)
            applyDataProp(resource,triples,prop)
        }
      }
      case Some(_ %> _ %> r) => throw new OWLFormatException(r + " was indicated as the property of a restriction but is not a named node")
      case None => {
        triples.get(Some(resource),Some(OWL.onProperties),None) headOption match {
          case Some(_ %> _ %> (r : Resource)) => applyNAryProp(resource,triples,RDFList(r,triples))
          case Some(_ %> _ %> r) => throw new OWLFormatException(r + " was indicated as the property list of an n-ary restriction but is not a resource")
          case None => throw new OWLFormatException("Not a valid restriction")
        }
      }
    }
  }
  
  private def ifHasDo[Restriction <: OWLRestriction](resource : Resource, triples : TripleSet,has : NamedNode, action : (Value) => Restriction) = {
    triples.get(Some(resource),Some(has),None) headOption match {
      case Some(_ %> _ %> v) => Some(action(v))
      case None => None
    }
  }
      
  private def applyObjProp(r : Resource, t : TripleSet, prop : OWLObjectProperty) : OWLRestriction = {
    ifHasDo(r,t,OWL.onClass, {
      case r : Resource => {
        val clazz = OWLClass(r,t)
        ifHasDo(r,t,OWL.cardinality, {
          case l : Literal => {
            val n = Integer.parseInt(l.stringValue)
            new OWLObjectQualifiedExactCardinality(r,t,prop,n,clazz)
          }
          case x => throw new OWLFormatException(x + " was indicated as a cardinality value but is not a literal")
        }) getOrElse {
          ifHasDo(r,t,OWL.minCardinality, {
            case l : Literal => {
              val n = Integer.parseInt(l.stringValue)
              new OWLObjectQualifiedMinCardinality(r,t,prop,n,clazz)
            }
            case x => throw new OWLFormatException(x + " was indicated as a cardinality value but is not a literal")
          }) getOrElse {
            ifHasDo(r,t,OWL.maxCardinality, {
              case l : Literal => {
                val n = Integer.parseInt(l.stringValue)
                new OWLObjectQualifiedMinCardinality(r,t,prop,n,clazz)
              }
              case x => throw new OWLFormatException(x + " was indicated as a cardinality value but is not a literal")
            }) match {
              case Some(x) => x
              case None => throw new OWLFormatException("onClass without cardinality")
            }
          }
        }
      }
      case x => throw new OWLFormatException(x + " was indicated as a cardinality value but is not a literal")
    }) getOrElse {
      ifHasDo(r,t,OWL.allValuesFrom, {
        case r : Resource => {
          val clazz = OWLClass(r,t)
          new OWLObjectAllValuesFrom(r,t,prop,clazz)
        }
        case x =>  throw new OWLFormatException(x + " was indicated as an all values from class but is not a resource")
      }) getOrElse {
        ifHasDo(r,t,OWL.someValuesFrom, {
          case r : Resource => {
            val clazz = OWLClass(r,t)
            new OWLObjectSomeValuesFrom(r,t,prop,clazz)
          }
          case x => throw new OWLFormatException(x + " was indicated as a some values from class but is not a resource")
          }) getOrElse {
            ifHasDo(r,t,OWL.hasValue, {
              case r : Resource => {
                val indiv = OWLIndividual(r,t)
                new OWLObjectHasValue(r,t,prop,indiv)
              }
              case x => throw new OWLFormatException(x + " was indicated as a value of a object property restriction but is not a resource")
            }) match {
              case Some(x) => x
              case None => throw new OWLFormatException("Invalid object property restriction")
            }
          }
      }
    }
  }
  
  private def applyDataProp(r : Resource, t : TripleSet, prop : OWLDatatypeProperty) : OWLRestriction = {
    ifHasDo(r,t,OWL.onDatatype, {
      case r : Resource => {
        val clazz = OWLDatatype(r,t)
        ifHasDo(r,t,OWL.cardinality, {
          case l : Literal => {
            val n = Integer.parseInt(l.stringValue)
            new OWLDataQualifiedExactCardinality(r,t,prop,n,clazz)
          }
          case x => throw new OWLFormatException(x + " was indicated as a cardinality value but is not a literal")
        }) getOrElse {
          ifHasDo(r,t,OWL.minCardinality, {
            case l : Literal => {
              val n = Integer.parseInt(l.stringValue)
              new OWLDataQualifiedMinCardinality(r,t,prop,n,clazz)
            }
            case x => throw new OWLFormatException(x + " was indicated as a cardinality value but is not a literal")
          }) getOrElse {
            ifHasDo(r,t,OWL.maxCardinality, {
              case l : Literal => {
                val n = Integer.parseInt(l.stringValue)
                new OWLDataQualifiedMinCardinality(r,t,prop,n,clazz)
              }
              case x => throw new OWLFormatException(x + " was indicated as a cardinality value but is not a literal")
            }) match {
              case Some(x) => x
              case None => throw new OWLFormatException("onClass without cardinality")
            }
          }
        }
      }
      case x => throw new OWLFormatException(x + " was indicated as a cardinality value but is not a literal")
    }) getOrElse {
      ifHasDo(r,t,OWL.allValuesFrom, {
        case r : Resource => {
          val clazz = OWLDatatype(r,t)
          new OWLDataAllValuesFrom(r,t,prop,clazz)
        }
        case x =>  throw new OWLFormatException(x + " was indicated as an all values from class but is not a resource")
      }) getOrElse {
        ifHasDo(r,t,OWL.someValuesFrom, {
          case r : Resource => {
            val clazz = OWLDatatype(r,t)
            new OWLDataSomeValuesFrom(r,t,prop,clazz)
          }
          case x => throw new OWLFormatException(x + " was indicated as a some values from class but is not a resource")
          }) getOrElse {
            ifHasDo(r,t,OWL.hasValue, {
              case l : Literal => {
                new OWLDataHasValue(r,t,prop,l)
              }
              case x => throw new OWLFormatException(x + " was indicated as a value of a datatype property restriction but is not a literal")
            }) match {
              case Some(x) => x
              case None => throw new OWLFormatException("Invalid datatype property restriction")
            }
          }
      }
    }
  }
  
  private def applyNAryProp(r : Resource, t : TripleSet, propList : RDFList) : OWLRestriction = {
    /*val properties = propList map { 
      case x : NamedNode => OWLDatatypeProperty(x,t)
      case x => throw new OWLFormatException(x + " was indicated as a property in a n-ary property restriction but is not a named node")
    } toSet*/
      
    ifHasDo(r,t,OWL.allValuesFrom, {
      case r : Resource => {
        val clazz = OWLDatatype(r,t)
        new OWLNAryDataAllValuesFrom(r,t,propList,clazz)
      }
      case x => throw new OWLFormatException(x + " was indicated as an all values from class but is not a resource")
      }) getOrElse {
        ifHasDo(r,t,OWL.someValuesFrom, {
          case r : Resource => {
            val clazz = OWLDatatype(r,t)
            new OWLNAryDataSomeValuesFrom(r,t,propList,clazz)
          }
          case x => throw new OWLFormatException(x + " was indicated as a some values from class but is not a resource")
        }) match {
          case Some(x) => x
          case None => throw new OWLFormatException("Invalid n-ary datatype property restriction")
        }
      }
  }
}
  
//////////////////////////////////////////////////////////////////////////////////////////////////////
// Object Property Restrictions

/** A restriction on a single property */
abstract class OWLUnaryObjectRestriction(resource : Resource, triples : TripleSet, val property : OWLObjectProperty) 
  extends OWLRestriction(resource,triples) {
}

/** A Some Values from restriction on an object property.
 * Created by <code>prop some clazz</code>
 */
class OWLObjectSomeValuesFrom private[restrictions] (resource : Resource, triples : TripleSet, 
  property : OWLObjectProperty, val clazz : OWLClass)
  extends OWLUnaryObjectRestriction(resource,triples,property) {
     
    override def equals(obj : Any) = obj match {
      case OWLObjectSomeValuesFrom(p2,c2) => (property == p2) && (clazz == c2)
      case _ => false
    }
    
    override def hashCode = property.hashCode * 7 + clazz.hashCode * 1
    
    override def toString = "OWLObjectSomeValuesFrom("+property+ "," + clazz+")"
}
  
object OWLObjectSomeValuesFrom {
  /** Create a new restriction */
  def apply(property : OWLObjectProperty, clazz : OWLClass) = {
    val n = AnonymousNode
    new OWLObjectSomeValuesFrom(n,clazz.triples++property.triples+
      (n %> RDF._type %> OWL.Restriction) +
      (n %> OWL.onProperty %> property.resource) +
      (n %> OWL.someValuesFrom %> clazz.resource),
      property,clazz)
  }
  
  /** Get the restriction with a given URI
   * @throws OWLNoSuchEntityException If there is no restriction at the given URI
   */
   def apply(resource : Resource, triples : TripleSet) = {
     triples.get(Some(resource),Some(RDF._type),Some(OWL.Restriction)) headOption match {
       case None => throw new OWLNoSuchEntityException
       case _ => 
     }
     val prop = triples.get(Some(resource),Some(OWL.onProperty),None) headOption match {
       case Some(_ %> _ %> (r : NamedNode)) => OWLObjectProperty(r,triples)
       case Some(_ %> _ %> r) => throw new OWLFormatException(r + " was stated as the property in a restriction but is not a named node")
       case None => throw new OWLNoSuchEntityException
     }
     val clazz = triples.get(Some(resource),Some(OWL.someValuesFrom),None) headOption match {
       case Some(_ %> _ %> (r : Resource)) => OWLClass(r,triples)
       case Some(_ %> _ %> r) => throw new OWLFormatException(r + " was stated as the class in a restriction but is not a resource")
       case None => throw new OWLNoSuchEntityException
     }
     new OWLObjectSomeValuesFrom(resource,triples,prop,clazz)
   }
   
   def unapply(entity : OWLEntity) : Option[Tuple2[OWLObjectProperty,OWLClass]] = entity match {
     case x : OWLObjectSomeValuesFrom => Some((x.property,x.clazz))
     case _ => None
   }
}
     
/** An All Values from restriction on an object property.
 * Created by <code>prop only clazz</code>
 */
class OWLObjectAllValuesFrom private[restrictions] (resource : Resource, triples : TripleSet, 
  property : OWLObjectProperty, val clazz : OWLClass)
  extends OWLUnaryObjectRestriction(resource,triples,property) {
     
    override def equals(obj : Any) = obj match {
      case OWLObjectAllValuesFrom(p2,c2) => (property == p2) && (clazz == c2)
      case _ => false
    }
    
    override def hashCode = property.hashCode * 7 + clazz.hashCode * 2
    
    override def toString = "OWLObjectAllValuesFrom("+property+ "," + clazz+")"
}
  
object OWLObjectAllValuesFrom {
  /** Create a new restriction */
  def apply(property : OWLObjectProperty, clazz : OWLClass) = {
    val n = AnonymousNode
    new OWLObjectAllValuesFrom(n,clazz.triples++property.triples+
      (n %> RDF._type %> OWL.Restriction) +
      (n %> OWL.onProperty %> property.resource) +
      (n %> OWL.allValuesFrom %> clazz.resource),
      property,clazz)
  }
  
  /** Get the restriction with a given URI
   * @throws OWLNoSuchEntityException If there is no restriction at the given URI
   */
   def apply(resource : Resource, triples : TripleSet) = {
     triples.get(Some(resource),Some(RDF._type),Some(OWL.Restriction)) headOption match {
       case None => throw new OWLNoSuchEntityException
       case _ => 
     }
     val prop = triples.get(Some(resource),Some(OWL.onProperty),None) headOption match {
       case Some(_ %> _ %> (r : NamedNode)) => OWLObjectProperty(r,triples)
       case Some(_ %> _ %> r) => throw new OWLFormatException(r + " was stated as the property in a restriction but is not a named node")
       case None => throw new OWLNoSuchEntityException
     }
     val clazz = triples.get(Some(resource),Some(OWL.allValuesFrom),None) headOption match {
       case Some(_ %> _ %> (r : Resource)) => OWLClass(r,triples)
       case Some(_ %> _ %> r) => throw new OWLFormatException(r + " was stated as the class in a restriction but is not a resource")
       case None => throw new OWLNoSuchEntityException
     }
     new OWLObjectAllValuesFrom(resource,triples,prop,clazz)
   }
   
   def unapply(entity : OWLEntity) : Option[Tuple2[OWLObjectProperty,OWLClass]] = entity match {
     case x : OWLObjectAllValuesFrom => Some((x.property,x.clazz))
     case _ => None
   }
}

     
/** A Has Value restriction on an object property.
 * Created by <code>prop value indiv</code>
 */
class OWLObjectHasValue private[restrictions] (resource : Resource, triples : TripleSet, 
  property : OWLObjectProperty, val value : OWLIndividual)
  extends OWLUnaryObjectRestriction(resource,triples,property) {
     
    override def equals(obj : Any) = obj match {
      case OWLObjectHasValue(p2,v2) => (property == p2) && (value == v2)
      case _ => false
    }
    
    override def hashCode = property.hashCode * 7 + value.hashCode
    
    override def toString = "OWLObjectHasValue("+property+ "," + value+")"
}
  
object OWLObjectHasValue {
  /** Create a new restriction */
  def apply(property : OWLObjectProperty, value : OWLIndividual) = {
    val n = AnonymousNode
    new OWLObjectHasValue(n,value.triples++property.triples+
      (n %> RDF._type %> OWL.Restriction) +
      (n %> OWL.onProperty %> property.resource) +
      (n %> OWL.hasValue %> value.resource),
      property,value)
  }
  
  /** Get the restriction with a given URI
   * @throws OWLNoSuchEntityException If there is no restriction at the given URI
   */
   def apply(resource : Resource, triples : TripleSet) = {
     triples.get(Some(resource),Some(RDF._type),Some(OWL.Restriction)) headOption match {
       case None => throw new OWLNoSuchEntityException
       case _ => 
     }
     val prop = triples.get(Some(resource),Some(OWL.onProperty),None) headOption match {
       case Some(_ %> _ %> (r : NamedNode)) => OWLObjectProperty(r,triples)
       case Some(_ %> _ %> r) => throw new OWLFormatException(r + " was stated as the property in a restriction but is not a named node")
       case None => throw new OWLNoSuchEntityException
     }
     val indiv = triples.get(Some(resource),Some(OWL.hasValue),None) headOption match {
       case Some(_ %> _ %> (r : Resource)) => OWLIndividual(r,triples)
       case Some(_ %> _ %> r) => throw new OWLFormatException(r + " was stated as the class in a restriction but is not a resource")
       case None => throw new OWLNoSuchEntityException
     }
     new OWLObjectHasValue(resource,triples,prop,indiv)
   }
   
   def unapply(entity : OWLEntity) : Option[Tuple2[OWLObjectProperty,OWLIndividual]] = entity match {
     case x : OWLObjectHasValue => Some((x.property,x.value))
     case _ => None
   }
}

/** An All Values from restriction on an object property.
 * Created by <code>prop value indiv</code>
 */
abstract class OWLObjectCardinalityRestriction private[restrictions] (resource : Resource, triples : TripleSet, 
  property : OWLObjectProperty, protected val n : Int)
  extends OWLUnaryObjectRestriction(resource,triples,property) {
     
    override def equals(obj : Any) = obj match {
      case x : OWLObjectCardinalityRestriction => this.getClass == x.getClass && this.property == x.property && this.n == n
      case _ => false
    }
    
    override def hashCode = property.hashCode * 7 + n * 255 + this.getClass.hashCode
    
}
  
/** An Exact Cardinality restriction on an object property.
 * Created by <code>prop exactly n</code>
 */
class OWLObjectExactCardinality private[restrictions] (resource : Resource, triples : TripleSet,
  property : OWLObjectProperty, n : Int) 
  extends OWLObjectCardinalityRestriction(resource,triples,property,n) {
  def cardinality = n
  override def toString = "OWLObjectExactCardinality("+property+","+n+")"
}

object OWLObjectExactCardinality {
  /** Create a new restriction */
  def apply(property : OWLObjectProperty, n : Int) = {
    val n2 = AnonymousNode
    new OWLObjectExactCardinality(n2,property.triples+
      (n2 %> RDF._type %> OWL.Restriction) +
      (n2 %> OWL.onProperty %> property.resource) +
      (n2 %> OWL.cardinality %> (n.toString^^XSD.nonNegativeInteger)),
      property,n)
  }
  
  /** Get the restriction with a given URI
   * @throws OWLNoSuchEntityException If there is no restriction at the given URI
   */
   def apply(resource : Resource, triples : TripleSet) = {
     triples.get(Some(resource),Some(RDF._type),Some(OWL.Restriction)) headOption match {
       case None => throw new OWLNoSuchEntityException
       case _ => 
     }
     val prop = triples.get(Some(resource),Some(OWL.onProperty),None) headOption match {
       case Some(_ %> _ %> (r : NamedNode)) => OWLObjectProperty(r,triples)
       case Some(_ %> _ %> r) => throw new OWLFormatException(r + " was stated as the property in a restriction but is not a named node")
       case None => throw new OWLNoSuchEntityException
     }
     val n = triples.get(Some(resource),Some(OWL.cardinality),None) headOption match {
       case Some(_ %> _ %> (l : Literal)) => Integer.parseInt(l.stringValue)
       case Some(_ %> _ %> r) => throw new OWLFormatException(r + " was stated as the value of a cardinality restrictions but was not a literal")
       case None => throw new OWLNoSuchEntityException
     }
     new OWLObjectExactCardinality(resource,triples,prop,n)
   }
   
   def unapply(entity : OWLEntity) : Option[Tuple2[OWLObjectProperty,Int]] = entity match {
     case x : OWLObjectExactCardinality => Some((x.property,x.n))
     case _ => None
   }
}

/** A Qualified Exact Cardinality restriction on a data property relative to a class
 * Created by <code>prop exact (n,clazz)</code>
 */
class OWLObjectQualifiedExactCardinality private[restrictions] (resource : Resource, triples : TripleSet,
  property : OWLObjectProperty, n : Int, val clazz : OWLClass) 
  extends OWLObjectCardinalityRestriction(resource,triples,property,n) {
  def cardinality = n
  override def equals(obj : Any) = obj match {
    case OWLObjectQualifiedExactCardinality(p2,n2,c2) => property == p2 && n == n2 && clazz == c2
    case _ => false
  }
  override def hashCode = super.hashCode + clazz.hashCode
  override def toString = "OWLObjectQualifiedExactCardinality("+property+","+n+","+clazz+")"
}

object OWLObjectQualifiedExactCardinality {
  /** Create a new restriction */
  def apply(property : OWLObjectProperty, n : Int, clazz : OWLClass) = {
    val n2 = AnonymousNode
    new OWLObjectQualifiedExactCardinality(n2,clazz.triples++property.triples+
      (n2 %> RDF._type %> OWL.Restriction) +
      (n2 %> OWL.onProperty %> property.resource) +
      (n2 %> OWL.cardinality %> (n.toString^^XSD.nonNegativeInteger)) +
      (n2 %> OWL.onClass %> clazz.resource),
      property,n,clazz)
  }
  
  /** Get the restriction with a given URI
   * @throws OWLNoSuchEntityException If there is no restriction at the given URI
   */
   def apply(resource : Resource, triples : TripleSet) = {
     triples.get(Some(resource),Some(RDF._type),Some(OWL.Restriction)) headOption match {
       case None => throw new OWLNoSuchEntityException
       case _ => 
     }
     val prop = triples.get(Some(resource),Some(OWL.onProperty),None) headOption match {
       case Some(_ %> _ %> (r : NamedNode)) => OWLObjectProperty(r,triples)
       case Some(_ %> _ %> r) => throw new OWLFormatException(r + " was stated as the property in a restriction but is not a named node")
       case None => throw new OWLNoSuchEntityException
     }
     val n = triples.get(Some(resource),Some(OWL.cardinality),None) headOption match {
       case Some(_ %> _ %> (l : Literal)) => Integer.parseInt(l.stringValue)
       case Some(_ %> _ %> r) => throw new OWLFormatException(r + " was stated as the value of a cardinality restrictions but was not a literal")
       case None => throw new OWLNoSuchEntityException
     }
     val clazz = triples.get(Some(resource),Some(OWL.allValuesFrom),None) headOption match {
       case Some(_ %> _ %> (r : Resource)) => OWLClass(r,triples)
       case Some(_ %> _ %> r) => throw new OWLFormatException(r + " was stated as the class in a restriction but is not a resource")
       case None => throw new OWLNoSuchEntityException
     }
     new OWLObjectQualifiedExactCardinality(resource,triples,prop,n,clazz)
   }
   
   def unapply(entity : OWLEntity) : Option[Tuple3[OWLObjectProperty,Int,OWLClass]] = entity match {
     case x : OWLObjectQualifiedExactCardinality => Some((x.property,x.n,x.clazz))
     case _ => None
   }
}

  
/** An Exact Cardinality restriction on an object property.
 * Created by <code>prop min n</code>
 */
class OWLObjectMinCardinality private[restrictions] (resource : Resource, triples : TripleSet,
  property : OWLObjectProperty, n : Int) 
  extends OWLObjectCardinalityRestriction(resource,triples,property,n) {
  def minCardinality = n
  override def toString = "OWLObjectMinCardinality("+property+","+n+")"
}

object OWLObjectMinCardinality {
  /** Create a new restriction */
  def apply(property : OWLObjectProperty, n : Int) = {
    val n2 = AnonymousNode
    new OWLObjectMinCardinality(n2,property.triples+
      (n2 %> RDF._type %> OWL.Restriction) +
      (n2 %> OWL.onProperty %> property.resource) +
      (n2 %> OWL.minCardinality %> (n.toString^^XSD.nonNegativeInteger)),
      property,n)
  }
  
  /** Get the restriction with a given URI
   * @throws OWLNoSuchEntityException If there is no restriction at the given URI
   */
   def apply(resource : Resource, triples : TripleSet) = {
     triples.get(Some(resource),Some(RDF._type),Some(OWL.Restriction)) headOption match {
       case None => throw new OWLNoSuchEntityException
       case _ => 
     }
     val prop = triples.get(Some(resource),Some(OWL.onProperty),None) headOption match {
       case Some(_ %> _ %> (r : NamedNode)) => OWLObjectProperty(r,triples)
       case Some(_ %> _ %> r) => throw new OWLFormatException(r + " was stated as the property in a restriction but is not a named node")
       case None => throw new OWLNoSuchEntityException
     }
     val n = triples.get(Some(resource),Some(OWL.minCardinality),None) headOption match {
       case Some(_ %> _ %> (l : Literal)) => Integer.parseInt(l.stringValue)
       case Some(_ %> _ %> r) => throw new OWLFormatException(r + " was stated as the value of a cardinality restrictions but was not a literal")
       case None => throw new OWLNoSuchEntityException
     }
     new OWLObjectMinCardinality(resource,triples,prop,n)
   }
   
   def unapply(entity : OWLEntity) : Option[Tuple2[OWLObjectProperty,Int]] = entity match {
     case x : OWLObjectMinCardinality => Some((x.property,x.n))
     case _ => None
   }
}

/** A Qualified Exact Cardinality restriction on a data property relative to a class
 * Created by <code>prop exact (n,clazz)</code>
 */
class OWLObjectQualifiedMinCardinality private[restrictions] (resource : Resource, triples : TripleSet,
  property : OWLObjectProperty, n : Int, val clazz : OWLClass) 
  extends OWLObjectCardinalityRestriction(resource,triples,property,n) {
  def cardinality = n
  override def equals(obj : Any) = obj match {
    case OWLObjectQualifiedMinCardinality(p2,n2,c2) => property == p2 && n == n2 && clazz == c2
    case _ => false
  }
  override def hashCode = super.hashCode + clazz.hashCode
  override def toString = "OWLObjectQualifiedMinCardinality("+property+","+n+","+clazz+")"
}

object OWLObjectQualifiedMinCardinality {
  /** Create a new restriction */
  def apply(property : OWLObjectProperty, n : Int, clazz : OWLClass) = {
    val n2 = AnonymousNode
    new OWLObjectQualifiedMinCardinality(n2,clazz.triples++property.triples+
      (n2 %> RDF._type %> OWL.Restriction) +
      (n2 %> OWL.onProperty %> property.resource) +
      (n2 %> OWL.minCardinality %> (n.toString^^XSD.nonNegativeInteger)) +
      (n2 %> OWL.onClass %> clazz.resource),
      property,n,clazz)
  }
  
  /** Get the restriction with a given URI
   * @throws OWLNoSuchEntityException If there is no restriction at the given URI
   */
   def apply(resource : Resource, triples : TripleSet) = {
     triples.get(Some(resource),Some(RDF._type),Some(OWL.Restriction)) headOption match {
       case None => throw new OWLNoSuchEntityException
       case _ => 
     }
     val prop = triples.get(Some(resource),Some(OWL.onProperty),None) headOption match {
       case Some(_ %> _ %> (r : NamedNode)) => OWLObjectProperty(r,triples)
       case Some(_ %> _ %> r) => throw new OWLFormatException(r + " was stated as the property in a restriction but is not a named node")
       case None => throw new OWLNoSuchEntityException
     }
     val n = triples.get(Some(resource),Some(OWL.minCardinality),None) headOption match {
       case Some(_ %> _ %> (l : Literal)) => Integer.parseInt(l.stringValue)
       case Some(_ %> _ %> r) => throw new OWLFormatException(r + " was stated as the value of a cardinality restrictions but was not a literal")
       case None => throw new OWLNoSuchEntityException
     }
     val clazz = triples.get(Some(resource),Some(OWL.allValuesFrom),None) headOption match {
       case Some(_ %> _ %> (r : Resource)) => OWLClass(r,triples)
       case Some(_ %> _ %> r) => throw new OWLFormatException(r + " was stated as the class in a restriction but is not a resource")
       case None => throw new OWLNoSuchEntityException
     }
     new OWLObjectQualifiedMinCardinality(resource,triples,prop,n,clazz)
   }
   
   def unapply(entity : OWLEntity) : Option[Tuple3[OWLObjectProperty,Int,OWLClass]] = entity match {
     case x : OWLObjectQualifiedMinCardinality => Some((x.property,x.n,x.clazz))
     case _ => None
   }
}
  
/** An Exact Cardinality restriction on an object property.
 * Created by <code>prop exactly n</code>
 */
class OWLObjectMaxCardinality private[restrictions] (resource : Resource, triples : TripleSet,
  property : OWLObjectProperty, n : Int) 
  extends OWLObjectCardinalityRestriction(resource,triples,property,n) {
  def maxCardinality = n
  override def toString = "OWLObjectMaxCardinality("+property+","+n+")"
}

object OWLObjectMaxCardinality {
  /** Create a new restriction */
  def apply(property : OWLObjectProperty, n : Int) = {
    val n2 = AnonymousNode
    new OWLObjectMaxCardinality(n2,property.triples+
      (n2 %> RDF._type %> OWL.Restriction) +
      (n2 %> OWL.onProperty %> property.resource) +
      (n2 %> OWL.maxCardinality %> (n.toString^^XSD.nonNegativeInteger)),
      property,n)
  }
  
  /** Get the restriction with a given URI
   * @throws OWLNoSuchEntityException If there is no restriction at the given URI
   */
   def apply(resource : Resource, triples : TripleSet) = {
     triples.get(Some(resource),Some(RDF._type),Some(OWL.Restriction)) headOption match {
       case None => throw new OWLNoSuchEntityException
       case _ => 
     }
     val prop = triples.get(Some(resource),Some(OWL.onProperty),None) headOption match {
       case Some(_ %> _ %> (r : NamedNode)) => OWLObjectProperty(r,triples)
       case Some(_ %> _ %> r) => throw new OWLFormatException(r + " was stated as the property in a restriction but is not a named node")
       case None => throw new OWLNoSuchEntityException
     }
     val n = triples.get(Some(resource),Some(OWL.maxCardinality),None) headOption match {
       case Some(_ %> _ %> (l : Literal)) => Integer.parseInt(l.stringValue)
       case Some(_ %> _ %> r) => throw new OWLFormatException(r + " was stated as the value of a cardinality restrictions but was not a literal")
       case None => throw new OWLNoSuchEntityException
     }
     new OWLObjectMaxCardinality(resource,triples,prop,n)
   }
   
   def unapply(entity : OWLEntity) : Option[Tuple2[OWLObjectProperty,Int]] = entity match {
     case x : OWLObjectMaxCardinality => Some((x.property,x.n))
     case _ => None
   }
}

/** A Qualified Max Cardinality restriction on a data property relative to a class
 * Created by <code>prop max (n,clazz)</code>
 */
class OWLObjectQualifiedMaxCardinality private[restrictions] (resource : Resource, triples : TripleSet,
  property : OWLObjectProperty, n : Int, val clazz : OWLClass) 
  extends OWLObjectCardinalityRestriction(resource,triples,property,n) {
  def cardinality = n
  override def equals(obj : Any) = obj match {
    case OWLObjectQualifiedMaxCardinality(p2,n2,c2) => property == p2 && n == n2 && clazz == c2
    case _ => false
  }
  override def hashCode = super.hashCode + clazz.hashCode
  override def toString = "OWLObjectQualifiedMaxCardinality("+property+","+n+","+clazz+")"
}

object OWLObjectQualifiedMaxCardinality {
  /** Create a new restriction */
  def apply(property : OWLObjectProperty, n : Int, clazz : OWLClass) = {
    val n2 = AnonymousNode
    new OWLObjectQualifiedMaxCardinality(n2,clazz.triples++property.triples+
      (n2 %> RDF._type %> OWL.Restriction) +
      (n2 %> OWL.onProperty %> property.resource) +
      (n2 %> OWL.maxCardinality %> (n.toString^^XSD.nonNegativeInteger)) +
      (n2 %> OWL.onClass %> clazz.resource),
      property,n,clazz)
  }
  
  /** Get the restriction with a given URI
   * @throws OWLNoSuchEntityException If there is no restriction at the given URI
   */
   def apply(resource : Resource, triples : TripleSet) = {
     triples.get(Some(resource),Some(RDF._type),Some(OWL.Restriction)) headOption match {
       case None => throw new OWLNoSuchEntityException
       case _ => 
     }
     val prop = triples.get(Some(resource),Some(OWL.onProperty),None) headOption match {
       case Some(_ %> _ %> (r : NamedNode)) => OWLObjectProperty(r,triples)
       case Some(_ %> _ %> r) => throw new OWLFormatException(r + " was stated as the property in a restriction but is not a named node")
       case None => throw new OWLNoSuchEntityException
     }
     val n = triples.get(Some(resource),Some(OWL.maxCardinality),None) headOption match {
       case Some(_ %> _ %> (l : Literal)) => Integer.parseInt(l.stringValue)
       case Some(_ %> _ %> r) => throw new OWLFormatException(r + " was stated as the value of a cardinality restrictions but was not a literal")
       case None => throw new OWLNoSuchEntityException
     }
     val clazz = triples.get(Some(resource),Some(OWL.allValuesFrom),None) headOption match {
       case Some(_ %> _ %> (r : Resource)) => OWLClass(r,triples)
       case Some(_ %> _ %> r) => throw new OWLFormatException(r + " was stated as the class in a restriction but is not a resource")
       case None => throw new OWLNoSuchEntityException
     }
     new OWLObjectQualifiedMaxCardinality(resource,triples,prop,n,clazz)
   }
   
   def unapply(entity : OWLEntity) : Option[Tuple3[OWLObjectProperty,Int,OWLClass]] = entity match {
     case x : OWLObjectQualifiedMaxCardinality => Some((x.property,x.n,x.clazz))
     case _ => None
   }
}

//////////////////////////////////////////////////////////////////////////////////////////////////////
// Data Property Restrictions

/** A restriction on a single property */
abstract class OWLUnaryDataRestriction(resource : Resource, triples : TripleSet, val property : OWLDatatypeProperty) 
  extends OWLRestriction(resource,triples) {
}

/** A Some Values from restriction on an data property.
 * Created by <code>prop some datatype</code>
 */
class OWLDataSomeValuesFrom private[restrictions] (resource : Resource, triples : TripleSet, 
  property : OWLDatatypeProperty, val datatype : OWLDatatype)
  extends OWLUnaryDataRestriction(resource,triples,property) {
     
    override def equals(obj : Any) = obj match {
      case OWLDataSomeValuesFrom(p2,c2) => (property == p2) && (datatype == c2)
      case _ => false
    }
    
    override def hashCode = property.hashCode * 7 + datatype.hashCode * 1
    
    override def toString = "OWLDataSomeValuesFrom("+property+ "," + datatype+")"
}
  
object OWLDataSomeValuesFrom {
  /** Create a new restriction */
  def apply(property : OWLDatatypeProperty, datatype : OWLDatatype) = {
    val n = AnonymousNode
    new OWLDataSomeValuesFrom(n,datatype.triples++property.triples+
      (n %> RDF._type %> OWL.Restriction) +
      (n %> OWL.onProperty %> property.resource) +
      (n %> OWL.someValuesFrom %> datatype.resource),
      property,datatype)
  }
  
  /** Get the restriction with a given URI
   * @throws OWLNoSuchEntityException If there is no restriction at the given URI
   */
   def apply(resource : Resource, triples : TripleSet) = {
     triples.get(Some(resource),Some(RDF._type),Some(OWL.Restriction)) headOption match {
       case None => throw new OWLNoSuchEntityException
       case _ => 
     }
     val prop = triples.get(Some(resource),Some(OWL.onProperty),None) headOption match {
       case Some(_ %> _ %> (r : NamedNode)) => OWLDatatypeProperty(r,triples)
       case Some(_ %> _ %> r) => throw new OWLFormatException(r + " was stated as the property in a restriction but is not a named node")
       case None => throw new OWLNoSuchEntityException
     }
     val datatype = triples.get(Some(resource),Some(OWL.someValuesFrom),None) headOption match {
       case Some(_ %> _ %> (r : Resource)) => OWLDatatype(r,triples)
       case Some(_ %> _ %> r) => throw new OWLFormatException(r + " was stated as the datatype in a restriction but is not a resource")
       case None => throw new OWLNoSuchEntityException
     }
     new OWLDataSomeValuesFrom(resource,triples,prop,datatype)
   }
   
   def unapply(entity : OWLEntity) : Option[Tuple2[OWLDatatypeProperty,OWLDatatype]] = entity match {
     case x : OWLDataSomeValuesFrom => Some((x.property,x.datatype))
     case _ => None
   }
}
     
/** An All Values from restriction on an data property.
 * Created by <code>prop only datatype</code>
 */
class OWLDataAllValuesFrom private[restrictions] (resource : Resource, triples : TripleSet, 
  property : OWLDatatypeProperty, val datatype : OWLDatatype)
  extends OWLUnaryDataRestriction(resource,triples,property) {
     
    override def equals(obj : Any) = obj match {
      case OWLDataAllValuesFrom(p2,c2) => (property == p2) && (datatype == c2)
      case _ => false
    }
    
    override def hashCode = property.hashCode * 7 + datatype.hashCode * 2
    
    override def toString = "OWLDataAllValuesFrom("+property+ "," + datatype+")"
}
  
object OWLDataAllValuesFrom {
  /** Create a new restriction */
  def apply(property : OWLDatatypeProperty, datatype : OWLDatatype) = {
    val n = AnonymousNode
    new OWLDataAllValuesFrom(n,datatype.triples++property.triples+
      (n %> RDF._type %> OWL.Restriction) +
      (n %> OWL.onProperty %> property.resource) +
      (n %> OWL.allValuesFrom %> datatype.resource),
      property,datatype)
  }
  
  /** Get the restriction with a given URI
   * @throws OWLNoSuchEntityException If there is no restriction at the given URI
   */
   def apply(resource : Resource, triples : TripleSet) = {
     triples.get(Some(resource),Some(RDF._type),Some(OWL.Restriction)) headOption match {
       case None => throw new OWLNoSuchEntityException
       case _ => 
     }
     val prop = triples.get(Some(resource),Some(OWL.onProperty),None) headOption match {
       case Some(_ %> _ %> (r : NamedNode)) => OWLDatatypeProperty(r,triples)
       case Some(_ %> _ %> r) => throw new OWLFormatException(r + " was stated as the property in a restriction but is not a named node")
       case None => throw new OWLNoSuchEntityException
     }
     val datatype = triples.get(Some(resource),Some(OWL.allValuesFrom),None) headOption match {
       case Some(_ %> _ %> (r : Resource)) => OWLDatatype(r,triples)
       case Some(_ %> _ %> r) => throw new OWLFormatException(r + " was stated as the datatype in a restriction but is not a resource")
       case None => throw new OWLNoSuchEntityException
     }
     new OWLDataAllValuesFrom(resource,triples,prop,datatype)
   }
   
   def unapply(entity : OWLEntity) : Option[Tuple2[OWLDatatypeProperty,OWLDatatype]] = entity match {
     case x : OWLDataAllValuesFrom => Some((x.property,x.datatype))
     case _ => None
   }
}

     
/** A Has Value restriction on an data property.
 * Created by <code>prop value indiv</code>
 */
class OWLDataHasValue private[restrictions] (resource : Resource, triples : TripleSet, 
  property : OWLDatatypeProperty, val value : Literal)
  extends OWLUnaryDataRestriction(resource,triples,property) {
     
    override def equals(obj : Any) = obj match {
      case OWLDataHasValue(p2,v2) => (property == p2) && (value == v2)
      case _ => false
    }
    
    override def hashCode = property.hashCode * 7 + value.hashCode
    
    override def toString = "OWLDataHasValue("+property+ "," + value+")"
}
  
object OWLDataHasValue {
  /** Create a new restriction */
  def apply(property : OWLDatatypeProperty, value : Literal) = {
    val n = AnonymousNode
    new OWLDataHasValue(n,property.triples+
      (n %> RDF._type %> OWL.Restriction) +
      (n %> OWL.onProperty %> property.resource) +
      (n %> OWL.hasValue %> value),
      property,value)
  }
  
  /** Get the restriction with a given URI
   * @throws OWLNoSuchEntityException If there is no restriction at the given URI
   */
   def apply(resource : Resource, triples : TripleSet) = {
     triples.get(Some(resource),Some(RDF._type),Some(OWL.Restriction)) headOption match {
       case None => throw new OWLNoSuchEntityException
       case _ => 
     }
     val prop = triples.get(Some(resource),Some(OWL.onProperty),None) headOption match {
       case Some(_ %> _ %> (r : NamedNode)) => OWLDatatypeProperty(r,triples)
       case Some(_ %> _ %> r) => throw new OWLFormatException(r + " was stated as the property in a restriction but is not a named node")
       case None => throw new OWLNoSuchEntityException
     }
     val indiv = triples.get(Some(resource),Some(OWL.hasValue),None) headOption match {
       case Some(_ %> _ %> (l : Literal)) => l
       case Some(_ %> _ %> r) => throw new OWLFormatException(r + " was stated as the value in a datatype restriction but is not a literal")
       case None => throw new OWLNoSuchEntityException
     }
     new OWLDataHasValue(resource,triples,prop,indiv)
   }
   
   def unapply(entity : OWLEntity) : Option[Tuple2[OWLDatatypeProperty,Literal]] = entity match {
     case x : OWLDataHasValue => Some((x.property,x.value))
     case _ => None
   }
}

/** An All Values from restriction on an data property.
 * Created by <code>prop value indiv</code>
 */
abstract class OWLDataCardinalityRestriction private[restrictions] (resource : Resource, triples : TripleSet, 
  property : OWLDatatypeProperty, protected val n : Int)
  extends OWLUnaryDataRestriction(resource,triples,property) {
     
    override def equals(obj : Any) = obj match {
      case x : OWLDataCardinalityRestriction => this.getClass == x.getClass && this.property == x.property && this.n == n
      case _ => false
    }
    
    override def hashCode = property.hashCode * 7 + n * 255 + this.getClass.hashCode
    
}
  
/** An Exact Cardinality restriction on an data property.
 * Created by <code>prop exactly n</code>
 */
class OWLDataExactCardinality private[restrictions] (resource : Resource, triples : TripleSet,
  property : OWLDatatypeProperty, n : Int) 
  extends OWLDataCardinalityRestriction(resource,triples,property,n) {
  def cardinality = n
  override def toString = "OWLDataExactCardinality("+property+","+n+")"
}

object OWLDataExactCardinality {
  /** Create a new restriction */
  def apply(property : OWLDatatypeProperty, n : Int) = {
    val n2 = AnonymousNode
    new OWLDataExactCardinality(n2,property.triples+
      (n2 %> RDF._type %> OWL.Restriction) +
      (n2 %> OWL.onProperty %> property.resource) +
      (n2 %> OWL.cardinality %> (n.toString^^XSD.nonNegativeInteger)),
      property,n)
  }
  
  /** Get the restriction with a given URI
   * @throws OWLNoSuchEntityException If there is no restriction at the given URI
   */
   def apply(resource : Resource, triples : TripleSet) = {
     triples.get(Some(resource),Some(RDF._type),Some(OWL.Restriction)) headOption match {
       case None => throw new OWLNoSuchEntityException
       case _ => 
     }
     val prop = triples.get(Some(resource),Some(OWL.onProperty),None) headOption match {
       case Some(_ %> _ %> (r : NamedNode)) => OWLDatatypeProperty(r,triples)
       case Some(_ %> _ %> r) => throw new OWLFormatException(r + " was stated as the property in a restriction but is not a named node")
       case None => throw new OWLNoSuchEntityException
     }
     val n = triples.get(Some(resource),Some(OWL.cardinality),None) headOption match {
       case Some(_ %> _ %> (l : Literal)) => Integer.parseInt(l.stringValue)
       case Some(_ %> _ %> r) => throw new OWLFormatException(r + " was stated as the value of a cardinality restrictions but was not a literal")
       case None => throw new OWLNoSuchEntityException
     }
     new OWLDataExactCardinality(resource,triples,prop,n)
   }
   
   def unapply(entity : OWLEntity) : Option[Tuple2[OWLDatatypeProperty,Int]] = entity match {
     case x : OWLDataExactCardinality => Some((x.property,x.n))
     case _ => None
   }
}

/** A Qualified Exact Cardinality restriction on a data property relative to a datatype
 * Created by <code>prop exact (n,datatype)</code>
 */
class OWLDataQualifiedExactCardinality private[restrictions] (resource : Resource, triples : TripleSet,
  property : OWLDatatypeProperty, n : Int, val datatype : OWLDatatype) 
  extends OWLDataCardinalityRestriction(resource,triples,property,n) {
  def cardinality = n
  override def equals(obj : Any) = obj match {
    case OWLDataQualifiedExactCardinality(p2,n2,c2) => property == p2 && n == n2 && datatype == c2
    case _ => false
  }
  override def hashCode = super.hashCode + datatype.hashCode
  override def toString = "OWLDataQualifiedExactCardinality("+property+","+n+","+datatype+")"
}

object OWLDataQualifiedExactCardinality {
  /** Create a new restriction */
  def apply(property : OWLDatatypeProperty, n : Int, datatype : OWLDatatype) = {
    val n2 = AnonymousNode
    new OWLDataQualifiedExactCardinality(n2,datatype.triples++property.triples+
      (n2 %> RDF._type %> OWL.Restriction) +
      (n2 %> OWL.onProperty %> property.resource) +
      (n2 %> OWL.cardinality %> (n.toString^^XSD.nonNegativeInteger)) +
      (n2 %> OWL.onDatatype %> datatype.resource),
      property,n,datatype)
  }
  
  /** Get the restriction with a given URI
   * @throws OWLNoSuchEntityException If there is no restriction at the given URI
   */
   def apply(resource : Resource, triples : TripleSet) = {
     triples.get(Some(resource),Some(RDF._type),Some(OWL.Restriction)) headOption match {
       case None => throw new OWLNoSuchEntityException
       case _ => 
     }
     val prop = triples.get(Some(resource),Some(OWL.onProperty),None) headOption match {
       case Some(_ %> _ %> (r : NamedNode)) => OWLDatatypeProperty(r,triples)
       case Some(_ %> _ %> r) => throw new OWLFormatException(r + " was stated as the property in a restriction but is not a named node")
       case None => throw new OWLNoSuchEntityException
     }
     val n = triples.get(Some(resource),Some(OWL.cardinality),None) headOption match {
       case Some(_ %> _ %> (l : Literal)) => Integer.parseInt(l.stringValue)
       case Some(_ %> _ %> r) => throw new OWLFormatException(r + " was stated as the value of a cardinality restrictions but was not a literal")
       case None => throw new OWLNoSuchEntityException
     }
     val datatype = triples.get(Some(resource),Some(OWL.allValuesFrom),None) headOption match {
       case Some(_ %> _ %> (r : Resource)) => OWLDatatype(r,triples)
       case Some(_ %> _ %> r) => throw new OWLFormatException(r + " was stated as the datatype in a restriction but is not a resource")
       case None => throw new OWLNoSuchEntityException
     }
     new OWLDataQualifiedExactCardinality(resource,triples,prop,n,datatype)
   }
   
   def unapply(entity : OWLEntity) : Option[Tuple3[OWLDatatypeProperty,Int,OWLDatatype]] = entity match {
     case x : OWLDataQualifiedExactCardinality => Some((x.property,x.n,x.datatype))
     case _ => None
   }
}

  
/** An Exact Cardinality restriction on an data property.
 * Created by <code>prop min n</code>
 */
class OWLDataMinCardinality private[restrictions] (resource : Resource, triples : TripleSet,
  property : OWLDatatypeProperty, n : Int) 
  extends OWLDataCardinalityRestriction(resource,triples,property,n) {
  def minCardinality = n
  override def toString = "OWLDataMinCardinality("+property+","+n+")"
}

object OWLDataMinCardinality {
  /** Create a new restriction */
  def apply(property : OWLDatatypeProperty, n : Int) = {
    val n2 = AnonymousNode
    new OWLDataMinCardinality(n2,property.triples+
      (n2 %> RDF._type %> OWL.Restriction) +
      (n2 %> OWL.onProperty %> property.resource) +
      (n2 %> OWL.minCardinality %> (n.toString^^XSD.nonNegativeInteger)),
      property,n)
  }
  
  /** Get the restriction with a given URI
   * @throws OWLNoSuchEntityException If there is no restriction at the given URI
   */
   def apply(resource : Resource, triples : TripleSet) = {
     triples.get(Some(resource),Some(RDF._type),Some(OWL.Restriction)) headOption match {
       case None => throw new OWLNoSuchEntityException
       case _ => 
     }
     val prop = triples.get(Some(resource),Some(OWL.onProperty),None) headOption match {
       case Some(_ %> _ %> (r : NamedNode)) => OWLDatatypeProperty(r,triples)
       case Some(_ %> _ %> r) => throw new OWLFormatException(r + " was stated as the property in a restriction but is not a named node")
       case None => throw new OWLNoSuchEntityException
     }
     val n = triples.get(Some(resource),Some(OWL.minCardinality),None) headOption match {
       case Some(_ %> _ %> (l : Literal)) => Integer.parseInt(l.stringValue)
       case Some(_ %> _ %> r) => throw new OWLFormatException(r + " was stated as the value of a cardinality restrictions but was not a literal")
       case None => throw new OWLNoSuchEntityException
     }
     new OWLDataMinCardinality(resource,triples,prop,n)
   }
   
   def unapply(entity : OWLEntity) : Option[Tuple2[OWLDatatypeProperty,Int]] = entity match {
     case x : OWLDataMinCardinality => Some((x.property,x.n))
     case _ => None
   }
}

/** A Qualified Exact Cardinality restriction on a data property relative to a datatype
 * Created by <code>prop exact (n,datatype)</code>
 */
class OWLDataQualifiedMinCardinality private[restrictions] (resource : Resource, triples : TripleSet,
  property : OWLDatatypeProperty, n : Int, val datatype : OWLDatatype) 
  extends OWLDataCardinalityRestriction(resource,triples,property,n) {
  def cardinality = n
  override def equals(obj : Any) = obj match {
    case OWLDataQualifiedMinCardinality(p2,n2,c2) => property == p2 && n == n2 && datatype == c2
    case _ => false
  }
  override def hashCode = super.hashCode + datatype.hashCode
  override def toString = "OWLDataQualifiedMinCardinality("+property+","+n+","+datatype+")"
}

object OWLDataQualifiedMinCardinality {
  /** Create a new restriction */
  def apply(property : OWLDatatypeProperty, n : Int, datatype : OWLDatatype) = {
    val n2 = AnonymousNode
    new OWLDataQualifiedMinCardinality(n2,datatype.triples++property.triples+
      (n2 %> RDF._type %> OWL.Restriction) +
      (n2 %> OWL.onProperty %> property.resource) +
      (n2 %> OWL.minCardinality %> (n.toString^^XSD.nonNegativeInteger)) +
      (n2 %> OWL.onDatatype %> datatype.resource),
      property,n,datatype)
  }
  
  /** Get the restriction with a given URI
   * @throws OWLNoSuchEntityException If there is no restriction at the given URI
   */
   def apply(resource : Resource, triples : TripleSet) = {
     triples.get(Some(resource),Some(RDF._type),Some(OWL.Restriction)) headOption match {
       case None => throw new OWLNoSuchEntityException
       case _ => 
     }
     val prop = triples.get(Some(resource),Some(OWL.onProperty),None) headOption match {
       case Some(_ %> _ %> (r : NamedNode)) => OWLDatatypeProperty(r,triples)
       case Some(_ %> _ %> r) => throw new OWLFormatException(r + " was stated as the property in a restriction but is not a named node")
       case None => throw new OWLNoSuchEntityException
     }
     val n = triples.get(Some(resource),Some(OWL.minCardinality),None) headOption match {
       case Some(_ %> _ %> (l : Literal)) => Integer.parseInt(l.stringValue)
       case Some(_ %> _ %> r) => throw new OWLFormatException(r + " was stated as the value of a cardinality restrictions but was not a literal")
       case None => throw new OWLNoSuchEntityException
     }
     val datatype = triples.get(Some(resource),Some(OWL.allValuesFrom),None) headOption match {
       case Some(_ %> _ %> (r : Resource)) => OWLDatatype(r,triples)
       case Some(_ %> _ %> r) => throw new OWLFormatException(r + " was stated as the datatype in a restriction but is not a resource")
       case None => throw new OWLNoSuchEntityException
     }
     new OWLDataQualifiedMinCardinality(resource,triples,prop,n,datatype)
   }
   
   def unapply(entity : OWLEntity) : Option[Tuple3[OWLDatatypeProperty,Int,OWLDatatype]] = entity match {
     case x : OWLDataQualifiedMinCardinality => Some((x.property,x.n,x.datatype))
     case _ => None
   }
}
  
/** An Exact Cardinality restriction on an data property.
 * Created by <code>prop exactly n</code>
 */
class OWLDataMaxCardinality private[restrictions] (resource : Resource, triples : TripleSet,
  property : OWLDatatypeProperty, n : Int) 
  extends OWLDataCardinalityRestriction(resource,triples,property,n) {
  def maxCardinality = n
  override def toString = "OWLDataMaxCardinality("+property+","+n+")"
}

object OWLDataMaxCardinality {
  /** Create a new restriction */
  def apply(property : OWLDatatypeProperty, n : Int) = {
    val n2 = AnonymousNode
    new OWLDataMaxCardinality(n2,property.triples+
      (n2 %> RDF._type %> OWL.Restriction) +
      (n2 %> OWL.onProperty %> property.resource) +
      (n2 %> OWL.maxCardinality %> (n.toString^^XSD.nonNegativeInteger)),
      property,n)
  }
  
  /** Get the restriction with a given URI
   * @throws OWLNoSuchEntityException If there is no restriction at the given URI
   */
   def apply(resource : Resource, triples : TripleSet) = {
     triples.get(Some(resource),Some(RDF._type),Some(OWL.Restriction)) headOption match {
       case None => throw new OWLNoSuchEntityException
       case _ => 
     }
     val prop = triples.get(Some(resource),Some(OWL.onProperty),None) headOption match {
       case Some(_ %> _ %> (r : NamedNode)) => OWLDatatypeProperty(r,triples)
       case Some(_ %> _ %> r) => throw new OWLFormatException(r + " was stated as the property in a restriction but is not a named node")
       case None => throw new OWLNoSuchEntityException
     }
     val n = triples.get(Some(resource),Some(OWL.maxCardinality),None) headOption match {
       case Some(_ %> _ %> (l : Literal)) => Integer.parseInt(l.stringValue)
       case Some(_ %> _ %> r) => throw new OWLFormatException(r + " was stated as the value of a cardinality restrictions but was not a literal")
       case None => throw new OWLNoSuchEntityException
     }
     new OWLDataMaxCardinality(resource,triples,prop,n)
   }
   
   def unapply(entity : OWLEntity) : Option[Tuple2[OWLDatatypeProperty,Int]] = entity match {
     case x : OWLDataMaxCardinality => Some((x.property,x.n))
     case _ => None
   }
}

/** A Qualified Max Cardinality restriction on a data property relative to a datatype
 * Created by <code>prop max (n,datatype)</code>
 */
class OWLDataQualifiedMaxCardinality private[restrictions] (resource : Resource, triples : TripleSet,
  property : OWLDatatypeProperty, n : Int, val datatype : OWLDatatype) 
  extends OWLDataCardinalityRestriction(resource,triples,property,n) {
  def cardinality = n
  override def equals(obj : Any) = obj match {
    case OWLDataQualifiedMaxCardinality(p2,n2,c2) => property == p2 && n == n2 && datatype == c2
    case _ => false
  }
  override def hashCode = super.hashCode + datatype.hashCode
  override def toString = "OWLDataQualifiedMaxCardinality("+property+","+n+","+datatype+")"
}

object OWLDataQualifiedMaxCardinality {
  /** Create a new restriction */
  def apply(property : OWLDatatypeProperty, n : Int, datatype : OWLDatatype) = {
    val n2 = AnonymousNode
    new OWLDataQualifiedMaxCardinality(n2,datatype.triples++property.triples+
      (n2 %> RDF._type %> OWL.Restriction) +
      (n2 %> OWL.onProperty %> property.resource) +
      (n2 %> OWL.maxCardinality %> (n.toString^^XSD.nonNegativeInteger)) +
      (n2 %> OWL.onDatatype %> datatype.resource),
      property,n,datatype)
  }
  
  /** Get the restriction with a given URI
   * @throws OWLNoSuchEntityException If there is no restriction at the given URI
   */
   def apply(resource : Resource, triples : TripleSet) = {
     triples.get(Some(resource),Some(RDF._type),Some(OWL.Restriction)) headOption match {
       case None => throw new OWLNoSuchEntityException
       case _ => 
     }
     val prop = triples.get(Some(resource),Some(OWL.onProperty),None) headOption match {
       case Some(_ %> _ %> (r : NamedNode)) => OWLDatatypeProperty(r,triples)
       case Some(_ %> _ %> r) => throw new OWLFormatException(r + " was stated as the property in a restriction but is not a named node")
       case None => throw new OWLNoSuchEntityException
     }
     val n = triples.get(Some(resource),Some(OWL.maxCardinality),None) headOption match {
       case Some(_ %> _ %> (l : Literal)) => Integer.parseInt(l.stringValue)
       case Some(_ %> _ %> r) => throw new OWLFormatException(r + " was stated as the value of a cardinality restrictions but was not a literal")
       case None => throw new OWLNoSuchEntityException
     }
     val datatype = triples.get(Some(resource),Some(OWL.allValuesFrom),None) headOption match {
       case Some(_ %> _ %> (r : Resource)) => OWLDatatype(r,triples)
       case Some(_ %> _ %> r) => throw new OWLFormatException(r + " was stated as the datatype in a restriction but is not a resource")
       case None => throw new OWLNoSuchEntityException
     }
     new OWLDataQualifiedMaxCardinality(resource,triples,prop,n,datatype)
   }
   
   def unapply(entity : OWLEntity) : Option[Tuple3[OWLDatatypeProperty,Int,OWLDatatype]] = entity match {
     case x : OWLDataQualifiedMaxCardinality => Some((x.property,x.n,x.datatype))
     case _ => None
   }
}


/** A restriction on a single property */
abstract class OWLNAryDataRestriction(resource : Resource, triples : TripleSet, propList : RDFList) 
  extends OWLRestriction(resource,triples) {
    def properties : Set[OWLDatatypeProperty] = (propList map { 
      case x : NamedNode => OWLDatatypeProperty(x,triples)
      case r => throw new OWLFormatException(r + " was stated as part of property list in an n-ary data restriction but is not a named node")
    }).toSet
}

/** A Some Values from restriction on an data property.
 * Created by <code>prop some datatype</code>
 */
class OWLNAryDataSomeValuesFrom private[restrictions] (resource : Resource, triples : TripleSet, 
  propList : RDFList, val datatype : OWLDatatype)
  extends OWLNAryDataRestriction(resource,triples,propList) {
     
    override def equals(obj : Any) = obj match {
      case OWLDataSomeValuesFrom(p2,c2) => (properties == p2) && (datatype == c2)
      case _ => false
    }
    
    override def hashCode = properties.hashCode * 7 + datatype.hashCode * 1
    
    override def toString = "OWLNAryDataSomeValuesFrom("+properties.mkString(",")+ "," + datatype+")"
}
  
object OWLNAryDataSomeValuesFrom {
  /** Create a new restriction */
  def apply(properties : Set[OWLDatatypeProperty], datatype : OWLDatatype) = {
    val n = AnonymousNode
    val l = RDFList((properties map (_.resource)).asInstanceOf[Set[Value]].toSeq:_*)
    new OWLNAryDataSomeValuesFrom(n,mergeProperties(properties)++datatype.triples++l.triples+
      (n %> RDF._type %> OWL.Restriction) +
      (n %> OWL.onProperties %> l.node) +
      (n %> OWL.someValuesFrom %> datatype.resource),
      l,datatype)
  }
  
  private def mergeProperties(clazz : Iterable[OWLDatatypeProperty]) : TripleSet = {
    if(clazz.isEmpty) {
      TripleSet()
    } else {
      var ts = clazz.head.triples
      for(c <- clazz.tail) {
        ts ++= c.triples
      }
      ts
    }
  }
  
  /** Get the restriction with a given URI
   * @throws OWLNoSuchEntityException If there is no restriction at the given URI
   */
   def apply(resource : Resource, triples : TripleSet) = {
     triples.get(Some(resource),Some(RDF._type),Some(OWL.Restriction)) headOption match {
       case None => throw new OWLNoSuchEntityException
       case _ => 
     }
     val propList = triples.get(Some(resource),Some(OWL.onProperty),None) headOption match {
       case Some(_ %> _ %> (r : Resource)) => RDFList(r,triples)
       case Some(_ %> _ %> r) => throw new OWLFormatException(r + " was stated as the property list in an n-ary restriction but is not a resource")
       case None => throw new OWLNoSuchEntityException
     }
     val datatype = triples.get(Some(resource),Some(OWL.someValuesFrom),None) headOption match {
       case Some(_ %> _ %> (r : Resource)) => OWLDatatype(r,triples)
       case Some(_ %> _ %> r) => throw new OWLFormatException(r + " was stated as the datatype in a restriction but is not a resource")
       case None => throw new OWLNoSuchEntityException
     }
     new OWLNAryDataSomeValuesFrom(resource,triples,propList,datatype)
   }
   
   def unapply(entity : OWLEntity) : Option[Tuple2[Set[OWLDatatypeProperty],OWLDatatype]] = entity match {
     case x : OWLNAryDataSomeValuesFrom => Some((x.properties,x.datatype))
     case _ => None
   }
}


/** A All Values from restriction on an data property.
 * Created by <code>prop all datatype</code>
 */
class OWLNAryDataAllValuesFrom private[restrictions] (resource : Resource, triples : TripleSet, 
  propList : RDFList, val datatype : OWLDatatype)
  extends OWLNAryDataRestriction(resource,triples,propList) {
     
    override def equals(obj : Any) = obj match {
      case OWLDataAllValuesFrom(p2,c2) => (properties == p2) && (datatype == c2)
      case _ => false
    }
    
    override def hashCode = properties.hashCode * 7 + datatype.hashCode * 1
    
    override def toString = "OWLNAryDataAllValuesFrom("+properties.mkString(",")+ "," + datatype+")"
}
  
object OWLNAryDataAllValuesFrom {
  /** Create a new restriction */
  def apply(properties : Set[OWLDatatypeProperty], datatype : OWLDatatype) = {
    val n = AnonymousNode
    val l = RDFList((properties map (_.resource)).asInstanceOf[Set[Value]].toSeq:_*)
    new OWLNAryDataAllValuesFrom(n,mergeProperties(properties)++datatype.triples++l.triples+
      (n %> RDF._type %> OWL.Restriction) +
      (n %> OWL.onProperties %> l.node) +
      (n %> OWL.allValuesFrom %> datatype.resource),
      l,datatype)
  }
  
  private def mergeProperties(clazz : Iterable[OWLDatatypeProperty]) : TripleSet = {
    if(clazz.isEmpty) {
      TripleSet()
    } else {
      var ts = clazz.head.triples
      for(c <- clazz.tail) {
        ts ++= c.triples
      }
      ts
    }
  }
  
  /** Get the restriction with a given URI
   * @throws OWLNoSuchEntityException If there is no restriction at the given URI
   */
   def apply(resource : Resource, triples : TripleSet) = {
     triples.get(Some(resource),Some(RDF._type),Some(OWL.Restriction)) headOption match {
       case None => throw new OWLNoSuchEntityException
       case _ => 
     }
     val propList = triples.get(Some(resource),Some(OWL.onProperty),None) headOption match {
       case Some(_ %> _ %> (r : Resource)) => RDFList(r,triples)
       case Some(_ %> _ %> r) => throw new OWLFormatException(r + " was stated as the property list in an n-ary restriction but is not a resource")
       case None => throw new OWLNoSuchEntityException
     }
     val datatype = triples.get(Some(resource),Some(OWL.allValuesFrom),None) headOption match {
       case Some(_ %> _ %> (r : Resource)) => OWLDatatype(r,triples)
       case Some(_ %> _ %> r) => throw new OWLFormatException(r + " was stated as the datatype in a restriction but is not a resource")
       case None => throw new OWLNoSuchEntityException
     }
     new OWLNAryDataAllValuesFrom(resource,triples,propList,datatype)
   }
   
   def unapply(entity : OWLEntity) : Option[Tuple2[Set[OWLDatatypeProperty],OWLDatatype]] = entity match {
     case x : OWLNAryDataAllValuesFrom => Some((x.properties,x.datatype))
     case _ => None
   }
}
