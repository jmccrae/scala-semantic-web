package scalasemweb.owl

import scalasemweb.rdf.collection._
import scala.collection._
import scalasemweb.owl.restrictions._
import scalasemweb.rdf.model._


/////////////////////////////////////////////////////////////////////////////////
// OWL properties

/** An OWL property. That is either an object, datatype or annotation property */
trait OWLProperty[+Type <: OWLType] extends OWLEntity {
  protected def typeCompanion : OWLCompanion[Type]
  implicit def companion : OWLCompanion[OWLProperty[OWLType]]
  protected type Prop <: OWLProperty[Type]
  def resource : NamedNode
  def triples : TripleSet
  
  /** Gets a copy of this property with domain axioms */
  def domain(clazzes : OWLClass*) : OWLProperty[Type] 
  
  /** Gets a copy of this property with sub property axioms */
  def subPropertyOf(clazzes : OWLClass*) : OWLProperty[Type]
    
  ///** Gets a copy of this property with range axioms */
  //def range(clazzes : Type*) : OWLProperty[Type] 
  
  /** Create an OWL property that is indentical to this but is deprecated */
  def deprecated(value : Boolean) = if(value) {
    make(resource, triples + (resource %> RDF._type %> OWL.DeprecatedProperty))
  } else {
    make(resource, triples - (resource %> RDF._type %> OWL.DeprecatedProperty))
  } 
    
  /** Gets all domains of this property */
  def domains : Set[OWLClass]
  
  ///** Gets all ranges of this property */
  //def ranges : Set[Type] 
  
  /** Get all sub propeties of this property */
  def subProperties : Set[Prop] 
  
  /** Get if this property is deprecated */
  def isDeprecated : Boolean = (triples get(Some(resource),Some(RDF._type),Some(OWL.DeprecatedProperty))) != None
}

object OWLProperty {
  def apply(resource : Resource, triples : TripleSet) : OWLProperty[OWLType] = resource match {
    case resource : NamedNode => {
      triples.get(Some(resource),Some(RDF._type),None).view.map {
        case _ %> _ %> OWL.ObjectProperty => Some(new OWLObjectProperty(resource,triples))
        case _ %> _ %> OWL.DatatypeProperty => Some(new OWLDatatypeProperty(resource,triples))
        case _ %> _ %> OWL.AnnotationProperty => Some(new OWLAnnotationProperty(resource,triples))
        //case _ %> _ %> OWL.OntologyProperty => new OWLOntologyProperty(resource,triples)
        case _ %> _ %> RDF.Property => rdfProperty(resource,triples) 
        case _ => None
      } find (_ != None) match {
        case Some(prop) => prop.get
        case None => if(triples.has(Some(resource),Some(RDF._type),Some(RDF.Property))) {
          new OWLObjectProperty(resource,triples)
        } else {
          throw new OWLNoSuchEntityException()
        }
      }
    }
    case _ => throw new OWLNoSuchEntityException()
  }
  
  private def rdfProperty(resource : NamedNode, triples : TripleSet) : Option[OWLProperty[OWLType]] = {
    triples get(Some(resource),Some(RDFS.range),None) headOption match {
      case Some(_ %> _ %> (range : Resource)) => try {
          OWLClass(range,triples)
          Some(new OWLObjectProperty(resource,triples))
        } catch {
          case x : Exception => try {
            OWLDatatype(range,triples)
            Some(new OWLDatatypeProperty(resource,triples))
          } catch {
            case x2 : OWLNoSuchEntityException => None
          } 
        }
      case _ => None
    }
  }
}            

sealed trait OWLPropertyCharacteristic {
  private[owl] def clazz : Resource
}

trait OWLObjectPropertyCharacteristic extends OWLPropertyCharacteristic 

trait OWLDatatypePropertyCharacteristic extends OWLPropertyCharacteristic

object Symmetric extends OWLObjectPropertyCharacteristic {
  private[owl] def clazz = OWL.SymmetricProperty
}

object Transitive extends OWLObjectPropertyCharacteristic {
  private[owl] def clazz = OWL.TransitiveProperty
} 

object Reflexive extends OWLObjectPropertyCharacteristic {
  private[owl] def clazz = OWL.ReflexiveProperty
}

object InverseFunctional extends OWLObjectPropertyCharacteristic {
  private[owl] def clazz = OWL.InverseFunctionalProperty
}

object Irreflexive extends OWLObjectPropertyCharacteristic {
  private[owl] def clazz = OWL.IrreflexiveProperty
}

object Functional extends OWLObjectPropertyCharacteristic with OWLDatatypePropertyCharacteristic {
  private[owl] def clazz = OWL.SymmetricProperty
}

object Asymmetric extends OWLObjectPropertyCharacteristic {
  private[owl] def clazz = OWL.AsymmetricProperty
}

/////////////////////////////////////////////////////////////////////////////////
// OWL object properties

/** An OWL object property. That is a property that has an individual as its object */
class OWLObjectProperty (val resource : NamedNode, val triples : TripleSet) extends 
  OWLProperty[OWLClass] {
  implicit def companion : OWLCompanion[OWLObjectProperty] = OWLObjectProperty
  protected def typeCompanion = OWLClass
  protected type Prop = OWLObjectProperty
  
  /** Get a copy of this property with equivalent property axioms */
  def equivalentProperty(props : OWLObjectProperty*) = make(resource, triples ++ 
    (props map (prop => resource %> OWL.equivalentProperty %> prop.resource)))
  
  /** Get a copy of this property with disjoint property axioms */
  def disjointWith(props : OWLObjectProperty*) = make(resource, triples ++
    (props map (prop => resource %> OWL.propertyDisjointWith %> prop.resource)))
  
  /** Get a copy of this property with inverse property axioms */
  def inverseOf(props : OWLObjectProperty*) = make(resource, triples ++
    (props map (prop => resource %> OWL.inverseOf %> prop.resource)))
    
  /** Get a copy of this property with a property chain axiom */    
  def propertyChain(prop : OWLPropertyChain) = make(resource, triples + 
    (resource %> OWL.propertyChainAxiom %> prop.node) ++ prop.frame)
    
  /** Create a property chain staring with this property */
  def o(prop : OWLObjectProperty) = new OWLPropertyChain(RDFList(resource,prop.resource),triples)
    
  /** Get a copy of this property with the given characterstic(s) */
  def withCharacteristic(chars : OWLObjectPropertyCharacteristic*) = make(resource, triples ++
    (chars map (c => resource %> RDF._type %> c.clazz)))
    
  /** Get all equivalent properties to this property */
  def equivalentProperties = triples get(Some(resource),Some(OWL.equivalentProperty),None) map {
    case _ %> _ %> (p : Resource) => make(p,triples)
    case stat => throw new OWLFormatException(stat.obj + " stated as an equivalent property but not a resource")
  }
  
  /** Get all disjoint properties to this property */
  def disjointProperties = triples get(Some(resource),Some(OWL.propertyDisjointWith),None) map {
    case _ %> _ %> (p : Resource) => make(p,triples)
    case stat => throw new OWLFormatException(stat.obj + " stated as a disjoint property but not a resource")
  }
  
  /** Get all inverse properties to this property */
  def inverseProperties = triples get(Some(resource),Some(OWL.inverseOf),None) map {
    case _%> _ %> (p : Resource) => make(p,triples)
    case stat => throw new OWLFormatException(stat.obj + " stated as an inverse property but not a resource")
  }
  
  /** Get all property chains equalt to this property */
  def propertyChains = triples get(Some(resource),Some(OWL.propertyChainAxiom),None) map {
    case _%> _ %> (p : Resource) => new OWLPropertyChain(RDFList(p,triples),triples)
    case stat => throw new OWLFormatException(stat.obj + " stated as an inverse property but not a resource")
  }
  
  /** Get all the characteristics of this property */
  def characteristics = triples get(Some(resource),Some(RDF._type),None) flatMap {
    case _ %> _ %> OWL.SymmetricProperty => Some(Symmetric)
    case _ %> _ %> OWL.TransitiveProperty => Some(Transitive)
    case _ %> _ %> OWL.InverseFunctionalProperty => Some(InverseFunctional)
    case _ %> _ %> OWL.IrreflexiveProperty => Some(Irreflexive)
    case _ %> _ %> OWL.FunctionalProperty => Some(Functional)
    case _ %> _ %> OWL.AsymmetricProperty => Some(Asymmetric)
    case _ => None
  } 
  
  ////////////////
  // Restrictions
  
  /** Create an all values from restriction class on this property */
  def only(clazz : OWLClass) = OWLObjectAllValuesFrom(this,clazz)
  
  /** Create a some values from restriction class on this property */
  def some(clazz : OWLClass) = OWLObjectSomeValuesFrom(this,clazz)
  
  /** Create a has value restriction class on this property */
  def value(indiv : OWLIndividual) = OWLObjectHasValue(this,indiv)
  
  /** Create an exact cardinality restriction class on this property */
  def exact(n : Int) = OWLObjectExactCardinality(this,n)
  
  /** Create a qualified exact cardinality restriction class on this property */
  def exact(n : Int, clazz : OWLClass) = OWLObjectQualifiedExactCardinality(this,n,clazz)
  
  /** Create a minimum cardinality restriction class on this property */
  def min(n : Int) = OWLObjectMinCardinality(this,n)
  
  /** Create a qualified minimum cardinality restriction class on this property */
  def min(n : Int, clazz : OWLClass) = OWLObjectQualifiedMinCardinality(this,n,clazz)
  
  /** Create a maximum cardinality restriction class on this property */
  def max(n : Int) = OWLObjectMaxCardinality(this,n)
  
  /** Create a qualified maximum cardinality restriction class on this property */
  def max(n : Int, clazz : OWLClass) = OWLObjectQualifiedMaxCardinality(this,n,clazz)
  
  // END
  
  private lazy val _frame = (triples get(Some(resource),None,None)) ++ 
  (triples get(Some(resource),Some(OWL.propertyChainAxiom),None) flatMap { 
    case _ %> _ %> (p : Resource) => RDFList(p,triples).frame
    case stat => throw new OWLFormatException(stat.obj + " stated as an inverse property but not a resource")
  })
  def frame = _frame
  
  override def equals(o : Any) = o match {
    case prop : OWLObjectProperty => resource == prop.resource
    case _ => false
  }
  
  override def hashCode = resource.hashCode + 3
  
  override def toString = "OWLObjectProperty("+resource+")"
  
  def domain(clazzes : OWLClass*) = { 
    make(resource,triples ++ ( clazzes map (clazz => resource %> RDFS.domain %> clazz.resource)))
  }
  
  def subPropertyOf(clazzes : OWLClass*)= { 
    make(resource,triples ++ (clazzes map (clazz => resource %> RDFS.subPropertyOf %> clazz.resource)))
  }
    
  def range(clazzes : OWLClass*) = { 
    make(resource,triples ++ (clazzes map (clazz => resource %> RDFS.range %> clazz.resource)))
  }
    
  def domains : Set[OWLClass] = triples get(Some(resource),Some(RDFS.domain),None) map {
    case _ %> _ %> (res : Resource) => OWLClass(res,triples)
    case stat => throw new OWLFormatException(stat.obj + " stated as property domain but not a resource")
  }
  
  def ranges : Set[OWLClass] = triples get(Some(resource),Some(RDFS.range),None) map {
    case _ %> _ %> (res : Resource) => typeCompanion(res,triples)
    case stat => throw new OWLFormatException(stat.obj + " stated as property range but not a resource")
  }
  
  
  def subProperties : Set[OWLObjectProperty] = {
    triples get(Some(resource),Some(RDFS.subPropertyOf),None) map {
      case _ %> _ %> (res : Resource) => make(res,triples)
      case stat => throw new OWLFormatException(stat.obj + " stated as a sub property but not a resource")
    }
  }
}

object OWLObjectProperty extends OWLCompanion[OWLObjectProperty] {
  val predefined = Set[Resource](OWL.topObjectProperty,OWL.bottomObjectProperty)
  
  def apply(resource : NamedNode) = new OWLObjectProperty(resource,TripleSet(resource %> RDF._type %> OWL.ObjectProperty))
  def apply(resource : Resource,triples : TripleSet) = resource match {
    case resource : NamedNode => if(triples.has(Some(resource),Some(RDF._type),Some(OWL.ObjectProperty)) ||
      predefined.contains(resource)) {
        new OWLObjectProperty(resource,triples)
      } else {
        throw new OWLNoSuchEntityException
      }
    case _ => throw new OWLFormatException
  }
  def unapply(entity : OWLEntity) : Option[Resource] = {
    if(entity.triples has(Some(entity.resource),Some(RDF._type),Some(OWL.ObjectProperty))) {
      Some(entity.resource)
    } else {
      None
    }
  }
}

/** A chain of OWL object properties */
class OWLPropertyChain private[owl] (list : RDFList,val triples : TripleSet) extends LinearSeq[OWLObjectProperty] 
with View {
  override def isEmpty = list.isEmpty
  
  override def head = list.head match {
    case res : NamedNode => new OWLObjectProperty(res,triples)
    case lit => throw new OWLFormatException(lit + " stated as part of property chain but not a resource")
  }
  
  override def tail = new OWLPropertyChain(list.tail,triples)
  
  def o(prop : OWLObjectProperty) = new OWLPropertyChain(list :+ prop.resource,triples)
  
  def apply(i : Int) = list(i) match {
    case res : NamedNode => new OWLObjectProperty(res,triples)
    case lit => throw new OWLFormatException(lit + " stated as part of property chain but not a resource")
  }
  
  def length = list.length
  
  def frame = list.frame
  
  def node = list.node
}

/////////////////////////////////////////////////////////////////////////////////
// OWL data type properties

/** An OWL datatype property. That is a property that takes literals as values */
class OWLDatatypeProperty (val resource : NamedNode, val triples : TripleSet) 
extends OWLProperty[OWLDatatype] {
  implicit def companion = OWLDatatypeProperty
  protected def typeCompanion = OWLDatatype
  protected type Prop = OWLDatatypeProperty
    
  /** Get a copy of this property with equivalent property axioms */
  def equivalentProperty(props : OWLObjectProperty*) = make(resource, triples ++ 
      (props map (prop => resource %> OWL.equivalentProperty %> prop.resource)))
      
  /** Get a copy of this property with disjoint property axioms */
  def disjointWith(props : OWLObjectProperty*) = make(resource, triples ++
      (props map (prop => resource %> OWL.propertyDisjointWith %> prop.resource)))
      
  /** Get all equivelant properties to this property */
  def equivalentProperties = triples get(Some(resource),Some(OWL.equivalentProperty),None) map {
    case _ %> _ %> (p : Resource) => make(p,triples)
    case stat => throw new OWLFormatException(stat.obj + " stated as an equivalent property but not a resource")
  }
  
  /** Get all disjoint properties to this property */
  def disjointProperties = triples get(Some(resource),Some(OWL.propertyDisjointWith),None) map {
    case _ %> _ %> (p : Resource) => make(p,triples)
    case stat => throw new OWLFormatException(stat.obj + " stated as a disjoint property but not a resource")
  }
  
  /** Get a copy of this property with the given characterstic(s) */
  def withCharacteristic(chars : OWLObjectPropertyCharacteristic*) = make(resource, triples ++
    (chars map (c => resource %> RDF._type %> c.clazz)))
  
  ////////////////
  // Restrictions
  
  /** Create an all values from restriction class on this property */
  def only(clazz : OWLDatatype) = OWLDataAllValuesFrom(this,clazz)
  
  /** Create a some values from restriction class on this property */
  def some(clazz : OWLDatatype) = OWLDataSomeValuesFrom(this,clazz)
  
  /** Create a has value restriction class on this property */
  def value(literal : Literal) = OWLDataHasValue(this,literal)
  
  /** Create an exact cardinality restriction class on this property */
  def exact(n : Int) = OWLDataExactCardinality(this,n)
  
  /** Create a qualified exact cardinality restriction class on this property */
  def exact(n : Int, clazz : OWLDatatype) = OWLDataQualifiedExactCardinality(this,n,clazz)
  
  /** Create a minimum cardinality restriction class on this property */
  def min(n : Int) = OWLDataMinCardinality(this,n)
  
  /** Create a qualified minimum cardinality restriction class on this property */
  def min(n : Int, clazz : OWLDatatype) = OWLDataQualifiedMinCardinality(this,n,clazz)
  
  /** Create a maximum cardinality restriction class on this property */
  def max(n : Int) = OWLDataMaxCardinality(this,n)
  
  /** Create a qualified maximum cardinality restriction class on this property */
  def max(n : Int, clazz : OWLDatatype) = OWLDataQualifiedMaxCardinality(this,n,clazz)
  
  // END
    
  private lazy val _frame = (triples get(Some(resource),None,None)) 
  def frame = _frame 
    
  override def equals(o : Any) = o match {
    case prop : OWLDatatypeProperty => resource == prop.resource
    case _ => false
  }
    
  override def hashCode = resource.hashCode + 4
    
  override def toString = "OWLDatatypeProperty("+resource+")"
  def domain(clazzes : OWLClass*) = { 
    make(resource,triples ++ ( clazzes map (clazz => resource %> RDFS.domain %> clazz.resource)))
  }
  
  def subPropertyOf(clazzes : OWLClass*)= { 
    make(resource,triples ++ (clazzes map (clazz => resource %> RDFS.subPropertyOf %> clazz.resource)))
  }
    
  def range(clazzes : OWLDatatype*) = { 
    make(resource,triples ++ (clazzes map (clazz => resource %> RDFS.range %> clazz.resource)))
  }
    
  def domains : Set[OWLClass] = triples get(Some(resource),Some(RDFS.domain),None) map {
    case _ %> _ %> (res : Resource) => OWLClass(res,triples)
    case stat => throw new OWLFormatException(stat.obj + " stated as property domain but not a resource")
  }
  
  def ranges : Set[OWLDatatype] = triples get(Some(resource),Some(RDFS.range),None) map {
    case _ %> _ %> (res : Resource) => typeCompanion(res,triples)
    case stat => throw new OWLFormatException(stat.obj + " stated as property range but not a resource")
  }
  
  def subProperties : Set[OWLDatatypeProperty] = {
    triples get(Some(resource),Some(RDFS.subPropertyOf),None) map {
      case _ %> _ %> (res : Resource) => make(res,triples)
      case stat => throw new OWLFormatException(stat.obj + " stated as a sub property but not a resource")
    }
  }
  
  /** Get all the characteristics of this property */
  def characteristics = triples get(Some(resource),Some(RDF._type),None) flatMap {
    case _ %> _ %> OWL.FunctionalProperty => Some(Functional)
    case _ => None
  } 
  
}

object OWLDatatypeProperty extends OWLCompanion[OWLDatatypeProperty] {
  val predefined = Set[Resource](OWL.topDataProperty,OWL.bottomDataProperty)
  
  def apply(resource : NamedNode) = new OWLDatatypeProperty(resource,TripleSet(resource %> RDF._type %> OWL.DatatypeProperty))
  def apply(resource : Resource,triples : TripleSet) = resource match {
    case resource : NamedNode => if(triples.has(Some(resource),Some(RDF._type),Some(OWL.DatatypeProperty)) ||
      predefined.contains(resource)) {
        new OWLDatatypeProperty(resource,triples)
      } else {
        throw new OWLNoSuchEntityException
      }
    case _ => throw new OWLFormatException
  }
  def unapply(entity : OWLEntity) : Option[Resource] = {
    if(entity.triples has(Some(entity.resource),Some(RDF._type),Some(OWL.DatatypeProperty))) {
      Some(entity.resource)
    } else {
      None
    }
  }
}

/////////////////////////////////////////////////////////////////////////////////
// OWL annotation properties

/** An OWL annotation property. That is a property that is not used for reasoning but
 * to provide non-ontological information */
class OWLAnnotationProperty (val resource : NamedNode, val triples : TripleSet) 
extends OWLProperty[OWLType] {
  implicit def companion = OWLAnnotationProperty
  protected def typeCompanion = new OWLCompanion[OWLType] {
    def apply(resource : Resource, triples : TripleSet) = new OWLEntityInstance(resource,triples) match {
      case OWLClass(resource) => new OWLClass(resource,triples)
      case OWLDatatype(resource) => new OWLDatatype(resource,triples)
      case _ => throw new OWLFormatException(resource + " was stated as a range of an annotation property but not asserted as a class or data type")
    }
  }
  protected type Prop = OWLAnnotationProperty
  
  def frame = triples get(Some(resource),None,None)
  
  override def equals(o : Any) = o match {
    case prop : OWLAnnotationProperty => resource == prop.resource
    case _ => false
  }
    
  override def hashCode = resource.hashCode + 5
    
  override def toString = "OWLAnnotationProperty("+resource+")"
  def domain(clazzes : OWLClass*) = { 
    make(resource,triples ++ ( clazzes map (clazz => resource %> RDFS.domain %> clazz.resource)))
  }
  
  def subPropertyOf(clazzes : OWLClass*)= { 
    make(resource,triples ++ (clazzes map (clazz => resource %> RDFS.subPropertyOf %> clazz.resource)))
  }
    
  def range(clazzes : OWLType*) = { 
    make(resource,triples ++ (clazzes map (clazz => resource %> RDFS.range %> clazz.resource)))
  }
    
  def domains : Set[OWLClass] = triples get(Some(resource),Some(RDFS.domain),None) map {
    case _ %> _ %> (res : Resource) => OWLClass(res,triples)
    case stat => throw new OWLFormatException(stat.obj + " stated as property domain but not a resource")
  }
  
  def ranges : Set[OWLType] = triples get(Some(resource),Some(RDFS.range),None) map {
    case _ %> _ %> (res : Resource) => typeCompanion(res,triples)
    case stat => throw new OWLFormatException(stat.obj + " stated as property range but not a resource")
  }
  
  def subProperties : Set[OWLAnnotationProperty] = {
    triples get(Some(resource),Some(RDFS.subPropertyOf),None) map {
      case _ %> _ %> (res : Resource) => make(res,triples)
      case stat => throw new OWLFormatException(stat.obj + " stated as a sub property but not a resource")
    }
  }
}
 
object OWLAnnotationProperty extends OWLCompanion[OWLAnnotationProperty] {
  val predefined = Set[Resource](
    RDFS.comment,
    RDFS.isDefinedBy,
    RDFS.label,
    RDFS.seeAlso,
    OWL.deprecated,             
    OWL.versionIRI,            
    OWL.priorVersion,           
    OWL.backwardCompatibleWith, 
    OWL.incompatibleWith,
    OWL.versionInfo)      
  
  def apply(resource : NamedNode) = new OWLAnnotationProperty(resource,TripleSet(resource %> RDF._type %> OWL.AnnotationProperty))
  def apply(resource : Resource,triples : TripleSet) = resource match {
    case resource : NamedNode => if(triples.has(Some(resource),Some(RDF._type),Some(OWL.AnnotationProperty)) ||
      predefined.contains(resource)) {
        new OWLAnnotationProperty(resource,triples)
      } else {
        throw new OWLNoSuchEntityException
      }
    case _ => throw new OWLFormatException
  }
  def unapply(entity : OWLEntity) : Option[Resource] = {
    if(entity.triples has(Some(entity.resource),Some(RDF._type),Some(OWL.AnnotationProperty))) {
      Some(entity.resource)
    } else {
      None
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////
// Pattern matching support

object some {
  def unapply(entity : OWLEntity) : Option[Tuple2[OWLProperty[OWLType],OWLType]] = entity match {
    case OWLObjectSomeValuesFrom(property,clazz) => Some((property,clazz))
    case OWLDataSomeValuesFrom(property,clazz) => Some((property,clazz))
  }
}

