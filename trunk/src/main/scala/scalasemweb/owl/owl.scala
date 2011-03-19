package scalasemweb.owl

import scala.collection._
import scalasemweb.rdf.model._
import scalasemweb.rdf.collection._

/////////////////////////////////////////////////////////////////////////////////
// OWL name space definition

/** The OWL name space */
object OWL extends NameSpace("owl","""http://www.w3.org/2002/07/owl#""") {
 val Thing                        = this&"Thing"
 val AllDisjointProperties        = this&"AllDisjointProperties"
 val Restriction                  = this&"Restriction"
 val SymmetricProperty            = this&"SymmetricProperty"
 val targetIndividual             = this&"targetIndividual"
 val NegativePropertyAssertion    = this&"NegativePropertyAssertion"
 val Ontology                     = this&"Ontology"
 val annotatedSource              = this&"annotatedSource"
 val AnnotationProperty           = this&"AnnotationProperty"
 val topObjectProperty            = this&"topObjectProperty"
 val TransitiveProperty           = this&"TransitiveProperty"
 val maxQualifiedCardinality      = this&"maxQualifiedCardinality"
 val hasValue                     = this&"hasValue"
 val DataRange                    = this&"DataRange"
 val ReflexiveProperty            = this&"ReflexiveProperty"
 val InverseFunctionalProperty    = this&"InverseFunctionalProperty"
 val DeprecatedProperty           = this&"DeprecatedProperty"
 val equivalentProperty           = this&"equivalentProperty"
 val disjointWith                 = this&"disjointWith"
 val maxCardinality               = this&"maxCardinality"
 val hasSelf                      = this&"hasSelf"
 val onProperty                   = this&"onProperty"
 val IrreflexiveProperty          = this&"IrreflexiveProperty"
 val AllDifferent                 = this&"AllDifferent"
 val equivalentClass              = this&"equivalentClass"
 val cardinality                  = this&"cardinality"
 val DatatypeProperty             = this&"DatatypeProperty"
 val bottomDataProperty           = this&"bottomDataProperty"
 val Annotation                   = this&"Annotation"
 val sameAs                       = this&"sameAs"
 val Nothing                      = this&"Nothing"
 val unionOf                      = this&"unionOf"
 val minCardinality               = this&"minCardinality"
 val deprecated                   = this&"deprecated"
 val sourceIndividual             = this&"sourceIndividual"
 val withRestrictions             = this&"withRestrictions"
 val topDataProperty              = this&"topDataProperty"
 val annotatedProperty            = this&"annotatedProperty"
 val qualifiedCardinality         = this&"qualifiedCardinality"
 val allValuesFrom                = this&"allValuesFrom"
 val onDataRange                  = this&"onDataRange"
 val distinctMembers              = this&"distinctMembers"
 val FunctionalProperty           = this&"FunctionalProperty"
 val OntologyProperty             = this&"OntologyProperty"
 val ObjectProperty               = this&"ObjectProperty"
 val DeprecatedClass              = this&"DeprecatedClass"
 val onProperties                 = this&"onProperties"
 val onDatatype                   = this&"onDatatype"
 val NamedIndividual              = this&"NamedIndividual"
 val annotatedTarget              = this&"annotatedTarget"
 val versionIRI                   = this&"versionIRI"
 val differentFrom                = this&"differentFrom"
 val intersectionOf               = this&"intersectionOf"
 val hasKey                       = this&"hasKey"
 val disjointUnionOf              = this&"disjointUnionOf"
 val complementOf                 = this&"complementOf"
 val someValuesFrom               = this&"someValuesFrom"
 val priorVersion                 = this&"priorVersion"
 val backwardCompatibleWith       = this&"backwardCompatibleWith"
 val members                      = this&"members"
 val inverseOf                    = this&"inverseOf"
 val _Class                       = this&"Class"
 val datatypeComplementOf         = this&"datatypeComplementOf"
 val assertionProperty            = this&"assertionProperty"
 val bottomObjectProperty         = this&"bottomObjectProperty"
 val incompatibleWith             = this&"incompatibleWith"
 val minQualifiedCardinality      = this&"minQualifiedCardinality"
 val targetValue                  = this&"targetValue"
 val oneOf                        = this&"oneOf"
 val propertyChainAxiom           = this&"propertyChainAxiom"
 val propertyDisjointWith         = this&"propertyDisjointWith"
 val AllDisjointClasses           = this&"AllDisjointClasses"
 val AsymmetricProperty           = this&"AsymmetricProperty"
 val Axiom                        = this&"Axiom"
 val onClass                      = this&"onClass"
 val imports                      = this&"imports"
 val versionInfo                  = this&"versionInfo"
}


/////////////////////////////////////////////////////////////////////////////////
// OWL ontology

/** An instance of an OWL ontology. The SSW OWL package is designed as a set of views on
 * triple sets, the OWL ontology object views all triples in the ontology. It is also
 * accessible as an immutable set so changes can be made
 * @param triples The underlying set of triples
 */
class OWLOntology (val triples : TripleSet) extends View with Set[OWLEntity] 
with SetLike[OWLEntity,OWLOntology] {
  /** Create a copy of this ontology with the entity added */
  def +(entity : OWLEntity) = new OWLOntology(triples ++ entity.frame)
  /** Create a copy of this ontology with the entity removed */
  def -(entity : OWLEntity) = new OWLOntology(triples -- entity.frame)
  /** Does this contain the given entity */
  def contains(entity : OWLEntity) = !(triples get(Some(entity.resource),Some(RDF._type),None) isEmpty)
  private lazy val entSet : Set[OWLEntity] = triples get(None,Some(RDF._type),None) map {
    case res %> _ %> typ => typ match {
      case OWL._Class => new OWLClass(res,triples)
      case RDFS.Datatype => new OWLDatatype(res,triples)
      case OWL.ObjectProperty => new OWLObjectProperty(res,triples)
      case OWL.DatatypeProperty => new OWLDatatypeProperty(res,triples)
      case OWL.AnnotationProperty => new OWLAnnotationProperty(res,triples)
      case _ => new OWLIndividual(res,triples)
    }
  }
  /** Get an iterator over the set of elements */
  def iterator = entSet.iterator
  /** Get a new empty ontology */
  override def empty = new OWLOntology(TripleSet())
  /** The frame of the ontology is always the set of triples */
  def frame = triples
  
// def entities = classes ++ properties ++ individuals
// 
// def classes = triples get(None,Some(RDF._type),Some(OWL._Class)) map {
//   case res %> _ %> _ => Some(new OWLClass(res,triples))
//   case _ => None
// } ++ triples.get(None,Some(RDF._type),Some(RDFS._Class)) map {
//   case res %> _ %> _ => Some(new OWLClass(res,triples))
//   case _ => None
// } 
// 
// def properties = objectProperties ++ datatypeProperties ++ annotationProperties/* ++ 
// triples.get(None,Some(RDF._type),Some(RDF.Property)) {
//   case res %> _ %> _ => triples.get(Some(res),Some(RDFS.range),None) match {
//     case TripleSet((res : NamedNode) %> _ %> _) => if(res.uri.toString.matches(XSD.prefix+".*")) {
//       new OWLDatatypeProperty(res,triples)
//       
//     case TripleSet(res : ) => new OWLObjectProperty(res,triples)*/
//     
// def objectProperties = triples get(None,Some(RDF._type),Some(OWL.ObjectProperty)) map {
//   case res %> _ %> _ => Some(new OWLObjectProperty(res,triples))
//   case _ => None
// }
// 
// def datatypeProperties = triples get(None,Some(RDF._type),Some(OWL.DatatypeProperty)) map {
//   case res %> _ %> _ => Some(new OWLDatatypeProperty(res,triples))
//   case _ => None
// }
// 
// def annotationProperties = triples get(None,Some(RDF._type),Some(OWL.AnnotationProperty)) map {
//   case res %> _ %> _ => Some(new OWLAnnotationProperty(res,triples))
//   case _ => None
// }
// 
// def individual = triples get(None,Some(RDF._type),None) flatMap {
//   case res %> _ %> typ => if(typ == OWL._Class || typ == RDFS._Class || typ == OWL.ObjectProperty ||
//   typ == OWL.DatatypeProperty || typ == OWL.AnnotationProperty || typ == RDF.Property) {
//     None
//   } else {
//     Some(new OWLIndividual(res,triples))
//   }
// }
}

object OWLOntology {
  /** Create a new ontology, identified by a resource */
  def apply(resource : Resource) = new OWLOntology(TripleSet(resource %> RDF._type %> OWL.Ontology))
  /** Create a new ontology from a triple set */
  def apply(triples : TripleSet) = new OWLOntology(triples)
}

/////////////////////////////////////////////////////////////////////////////////
// OWL entities and types

/** An OWL entity. This like the rest of the SSW OWL support is a view on a triple set */
trait OWLEntity extends View {
  /** Get the resource this entity is based */
  def resource : Resource
  
  /** Create a new instance of this type */
  protected def make[Ent <: OWLEntity](resource : Resource, triples : TripleSet)
    (implicit companion : OWLCompanion[Ent]) : Ent = companion(resource,triples)
  
  /** The companion is used for creating ontology objects */
  implicit def companion : OWLCompanion[OWLEntity]
  /*def annotations(annos : Tuple2[NamedNode,Value]*) : this.type = make(resource,triples ++
     (*/
}

private class OWLEntityInstance(val resource : Resource, val triples : TripleSet) extends OWLEntity {
  implicit def companion = new OWLCompanion[OWLEntity] {
    def apply(resource : Resource, triples : TripleSet) = new OWLEntityInstance(resource,triples)
  }
  def frame = triples.get(Some(resource),None,None)
    
}

/** The companion object used to create new objects */
trait OWLCompanion[+Ent <: OWLEntity] {
  /** Create an entity on the resource over the set of triples */
  def apply(resource : Resource, triples : TripleSet) : Ent
}
  
/** An OWL type, this is a class or a datatype */
trait OWLType extends OWLEntity


/////////////////////////////////////////////////////////////////////////////////
// OWL classes

/** An OWL class */
class OWLClass (val resource : Resource, val triples : TripleSet) extends OWLType {
  implicit def companion = OWLClass
  
  /** Create an OWL class that is identical this but has a set of subclass axioms */
  def subClassOf(clazzes : OWLClass*) = make(resource, triples ++
     (clazzes flatMap  
      (clazz => clazz.frame + (resource %> RDFS.subClassOf %> clazz.resource))
    )
    )
  
    /** Create an OWL class that is identical this but has a set of equivalent class axioms */
  def equivalentClass(clazzes : OWLClass*) = make(resource, triples ++ 
    (clazzes flatMap  
       (clazz => clazz.frame + (resource %> OWL.equivalentClass %> clazz.resource))
    ))
    
  /** Create an OWL class that is identical this but has a set of disjoint class axioms */
  def disjointWith(clazzes : OWLClass*) = make(resource, triples ++
    (clazzes flatMap  
      (clazz => clazz.frame + (resource %> OWL.disjointWith %> clazz.resource))
    ))
    
  /** Create an OWL class that is identical this but has a set of disjoint union class axioms */
  def disjointUnionOf(clazzList : Iterable[OWLClass]) = {
    val list = RDFList.fromSeq(clazzList map (_.resource))
    make(resource, triples + (resource %> OWL.disjointUnionOf %> list.node) ++ list.frame)
  }
    
  /** Get all subclasses of this class */
  def subClasses : Set[OWLClass] = triples get(Some(resource), Some(RDFS.subClassOf),None) map {
    case _ %> _ %> (res :Resource) => make(res,triples)
    case x => throw new OWLException(x + " was stated as a sub class but is not a resource")
  }
  
  /** Get all equivalent classes of this class */
  def equivalentClasses : Set[OWLClass] = triples get(Some(resource), Some(OWL.equivalentClass),None) map {
    case _ %> _ %> (res :Resource) => make(res,triples)
    case x => throw new OWLException(x + " was stated as an equivalent class but is not a resource")
  }
  
  /** Get all disjoint classes of this class */
  def disjointClasses : Set[OWLClass] = triples get(Some(resource), Some(OWL.disjointWith),None) map {
    case _ %> _ %> (res :Resource) => make(res,triples)
    case x => throw new OWLException(x + " was stated as an equivalent class but is not a resource")
  }
  
  /** Get all disjoint unions with this class */
  def disjointUnions : Set[LinearSeq[OWLClass]] = triples get(Some(resource),Some(OWL.disjointUnionOf),None) map {
    case _ %> _ %> (res : Resource) => RDFList(res,triples) map {
      case res2 : Resource => make(res2,triples)
      case _ => throw new OWLException(res + " was stated as part of a disjoint union of classes but is not a resource")
    }
    case x => throw new OWLException(x + " was stated as a list of disjoint classes but is not a resource")
  }   
  
  def and(clazz : OWLClass) = {
    val n = AnonymousNode
    val l = RDFList(this.resource,clazz.resource)
    new OWLClassIntersection(n,triples + (n %> OWL.intersectionOf %> l.node) ++ clazz.triples,l)
  }
  
  private lazy val _frame = triples.get(Some(resource),None,None) ++
    (triples.get(Some(resource),Some(OWL.disjointUnionOf),None) flatMap {
      case triple : Triple => { triple.obj match {
        case res : Resource => RDFList(res,triples).frame 
        case res => throw new OWLException(res + " was stated as part of a disjoint union of classes but is not a resource")
        }
      }
    })
    
  def frame = _frame
    
  override def equals(o : Any) = o match {
    case clazz : OWLClass => resource == clazz.resource
    case _ => false
  }
  
  override def hashCode = resource.hashCode + 1
  
  override def toString = "OWLClass("+resource+")"
}

object OWLClass extends OWLCompanion[OWLClass] {
  def apply(res : Resource) = new OWLClass(res, TripleSet(res %> RDF._type %> OWL._Class))
  def apply(res : Resource, triples : TripleSet) = new OWLClass(res,triples)
  def unapply(entity : OWLEntity) : Option[Resource] = {
    if(entity.triples has(Some(entity.resource),Some(RDF._type),Some(OWL._Class))) {
      Some(entity.resource)
    } else {
      None
    }
  }
}

/////////////////////////////////////////////////////////////////////////////////
// OWL complex classes

class OWLClassIntersection(resource :Resource, triples : TripleSet, val list : RDFList) extends OWLClass(resource,triples++list) {
  override def frame = list.frame

  override def and(clazz : OWLClass) = new OWLClassIntersection(resource,triples,list :+ clazz.resource)
  
  def intersections : Set[OWLClass] = (list map {
    case res : Resource => make(res,triples)
    case lit => throw new OWLException(lit + " was stated as part of an OWL class intersection but is not a resource")
  }).toSet
  
  override def toString = "OWLClassIntersection("+ intersections.mkString(",") + ")"
}

object and {
  def unapply(entity : OWLEntity) : Option[Tuple2[OWLClass,OWLClass]] = entity match {
    case intersection : OWLClassIntersection => if(intersection.list.tail.isEmpty) {
      None
    } else if(intersection.list.tail.tail.isEmpty) {
      Some((new OWLClass(intersection.list.head.asInstanceOf[Resource],intersection.triples),
        new OWLClass(intersection.list.tail.head.asInstanceOf[Resource],intersection.triples)))
    } else {
      Some((new OWLClassIntersection(intersection.resource,intersection.triples,intersection.list.tail),
        new OWLClass(intersection.list.head.asInstanceOf[Resource],intersection.triples)))
    }
  }
}
      

/////////////////////////////////////////////////////////////////////////////////
// OWL datatypes

/** An OWL datatype. That is a value used to identify a range of literals */
class OWLDatatype private[owl] (val resource : Resource, val triples : TripleSet) extends OWLType {
  implicit def companion = OWLDatatype
  
  def frame = triples get(Some(resource),None,None)
  
  override def equals(o : Any) = o match {
      case clazz : OWLClass => resource == clazz.resource
      case _ => false
    }
    
  override def hashCode = resource.hashCode + 2
    
  override def toString = "OWLClass("+resource+")"
}

object OWLDatatype extends OWLCompanion[OWLDatatype] {
  def apply(resource : Resource) = new OWLDatatype(resource,TripleSet(resource %> RDF._type %> RDFS.Datatype))
  def apply(resource : Resource,triples : TripleSet) = new OWLDatatype(resource,triples)
  def unapply(entity : OWLEntity) : Option[Resource] = {
    if(entity.triples has(Some(entity.resource),Some(RDF._type),Some(RDFS.Datatype))) {
      Some(entity.resource)
    } else {
      None
    }
  }
}

/////////////////////////////////////////////////////////////////////////////////
// OWL properties

/** An OWL property. That is either an object, datatype or annotation property */
trait OWLProperty[Type <: OWLType] extends OWLEntity {
  protected def typeCompanion : OWLCompanion[Type]
  implicit def companion : OWLCompanion[OWLProperty[_]]
  protected type Prop <: OWLProperty[Type]
  
  /** Gets a copy of this property with domain axioms */
  def domain(clazzes : OWLClass*) : OWLProperty[Type] 
  
  /** Gets a copy of this property with sub property axioms */
  def subPropertyOf(clazzes : OWLClass*) : OWLProperty[Type]
    
  /** Gets a copy of this property with range axioms */
  def range(clazzes : Type*) : OWLProperty[Type] 
    
  /** Gets all domains of this property */
  def domains : Set[OWLClass]
  
  /** Gets all ranges of this property */
  def ranges : Set[Type] 
  
  /** Get all sub propeties of this property */
  def subProperties : Set[Prop] 
}


/////////////////////////////////////////////////////////////////////////////////
// OWL object properties

/** An OWL object property. That is a property that has an individual as its object */
class OWLObjectProperty (val resource : Resource, val triples : TripleSet) extends 
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
  
  /** Get all equivalent properties to this property */
  def equivalentPropertys = triples get(Some(resource),Some(OWL.equivalentProperty),None) map {
    case _ %> _ %> (p : Resource) => make(p,triples)
    case stat => throw new OWLException(stat.obj + " stated as an equivalent property but not a resource")
  }
  
  /** Get all disjoint properties to this property */
  def disjointPropertys = triples get(Some(resource),Some(OWL.propertyDisjointWith),None) map {
    case _ %> _ %> (p : Resource) => make(p,triples)
    case stat => throw new OWLException(stat.obj + " stated as a disjoint property but not a resource")
  }
  
  /** Get all inverse properties to this property */
  def inversePropertys = triples get(Some(resource),Some(OWL.inverseOf),None) map {
    case _%> _ %> (p : Resource) => make(p,triples)
    case stat => throw new OWLException(stat.obj + " stated as an inverse property but not a resource")
  }
  
  /** Get all property chains equalt to this property */
  def propertyChains = triples get(Some(resource),Some(OWL.propertyChainAxiom),None) map {
    case _%> _ %> (p : Resource) => new OWLPropertyChain(RDFList(p,triples),triples)
    case stat => throw new OWLException(stat.obj + " stated as an inverse property but not a resource")
  }
  
  private lazy val _frame = (triples get(Some(resource),None,None)) ++ 
  (triples get(Some(resource),Some(OWL.propertyChainAxiom),None) flatMap { 
    case _ %> _ %> (p : Resource) => RDFList(p,triples).frame
    case stat => throw new OWLException(stat.obj + " stated as an inverse property but not a resource")
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
    case stat => throw new OWLException(stat.obj + " stated as property domain but not a resource")
  }
  
  def ranges : Set[OWLClass] = triples get(Some(resource),Some(RDFS.range),None) map {
    case _ %> _ %> (res : Resource) => typeCompanion(res,triples)
    case stat => throw new OWLException(stat.obj + " stated as property range but not a resource")
  }
  
  
  def subProperties : Set[OWLObjectProperty] = {
    triples get(Some(resource),Some(RDFS.subPropertyOf),None) map {
      case _ %> _ %> (res : Resource) => make(res,triples)
      case stat => throw new OWLException(stat.obj + " stated as a sub property but not a resource")
    }
  }
}

object OWLObjectProperty extends OWLCompanion[OWLObjectProperty] {
  def apply(resource : Resource) = new OWLObjectProperty(resource,TripleSet(resource %> RDF._type %> OWL.ObjectProperty))
  def apply(resource : Resource,triples : TripleSet) = new OWLObjectProperty(resource,triples)
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
    case res : Resource => new OWLObjectProperty(res,triples)
    case lit => throw new OWLException(lit + " stated as part of property chain but not a resource")
  }
  
  override def tail = new OWLPropertyChain(list.tail,triples)
  
  def o(prop : OWLObjectProperty) = new OWLPropertyChain(list :+ prop.resource,triples)
  
  def apply(i : Int) = list(i) match {
    case res : Resource => new OWLObjectProperty(res,triples)
    case lit => throw new OWLException(lit + " stated as part of property chain but not a resource")
  }
  
  def length = list.length
  
  def frame = list.frame
  
  def node = list.node
}

/////////////////////////////////////////////////////////////////////////////////
// OWL data type properties

/** An OWL datatype property. That is a property that takes literals as values */
class OWLDatatypeProperty (val resource : Resource, val triples : TripleSet) 
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
  def equivalentPropertys = triples get(Some(resource),Some(OWL.equivalentProperty),None) map {
    case _ %> _ %> (p : Resource) => make(p,triples)
    case stat => throw new OWLException(stat.obj + " stated as an equivalent property but not a resource")
  }
  
  /** Get all disjoint properties to this property */
  def disjointPropertys = triples get(Some(resource),Some(OWL.propertyDisjointWith),None) map {
    case _ %> _ %> (p : Resource) => make(p,triples)
    case stat => throw new OWLException(stat.obj + " stated as a disjoint property but not a resource")
  }
    
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
    case stat => throw new OWLException(stat.obj + " stated as property domain but not a resource")
  }
  
  def ranges : Set[OWLDatatype] = triples get(Some(resource),Some(RDFS.range),None) map {
    case _ %> _ %> (res : Resource) => typeCompanion(res,triples)
    case stat => throw new OWLException(stat.obj + " stated as property range but not a resource")
  }
  
  def subProperties : Set[OWLDatatypeProperty] = {
    triples get(Some(resource),Some(RDFS.subPropertyOf),None) map {
      case _ %> _ %> (res : Resource) => make(res,triples)
      case stat => throw new OWLException(stat.obj + " stated as a sub property but not a resource")
    }
  }
}

object OWLDatatypeProperty extends OWLCompanion[OWLDatatypeProperty] {
  def apply(resource : Resource) = new OWLDatatypeProperty(resource,TripleSet(resource %> RDF._type %> OWL.DatatypeProperty))
  def apply(resource : Resource,triples : TripleSet) = new OWLDatatypeProperty(resource,triples)
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
class OWLAnnotationProperty (val resource : Resource, val triples : TripleSet) 
extends OWLProperty[OWLType] {
  implicit def companion = OWLAnnotationProperty
  protected def typeCompanion = new OWLCompanion[OWLType] {
    def apply(resource : Resource, triples : TripleSet) = new OWLEntityInstance(resource,triples) match {
      case OWLClass(resource) => new OWLClass(resource,triples)
      case OWLDatatype(resource) => new OWLDatatype(resource,triples)
      case _ => throw new OWLException(resource + " was stated as a range of an annotation property but not asserted as a class or data type")
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
    case stat => throw new OWLException(stat.obj + " stated as property domain but not a resource")
  }
  
  def ranges : Set[OWLType] = triples get(Some(resource),Some(RDFS.range),None) map {
    case _ %> _ %> (res : Resource) => typeCompanion(res,triples)
    case stat => throw new OWLException(stat.obj + " stated as property range but not a resource")
  }
  
  def subProperties : Set[OWLAnnotationProperty] = {
    triples get(Some(resource),Some(RDFS.subPropertyOf),None) map {
      case _ %> _ %> (res : Resource) => make(res,triples)
      case stat => throw new OWLException(stat.obj + " stated as a sub property but not a resource")
    }
  }
}
 
object OWLAnnotationProperty extends OWLCompanion[OWLAnnotationProperty] {
  def apply(resource : Resource) = new OWLAnnotationProperty(resource,TripleSet(resource %> RDF._type %> OWL.AnnotationProperty))
  def apply(resource : Resource,triples : TripleSet) = new OWLAnnotationProperty(resource,triples)
  def unapply(entity : OWLEntity) : Option[Resource] = {
    if(entity.triples has(Some(entity.resource),Some(RDF._type),Some(OWL.AnnotationProperty))) {
      Some(entity.resource)
    } else {
      None
    }
  }
}

/////////////////////////////////////////////////////////////////////////////////
// OWL individuals

/** An OWL individual. A single instance of an object in the ontology */
class OWLIndividual(val resource : Resource, val triples : TripleSet) extends OWLEntity {
  implicit def companion = OWLIndividual
      
  /** Get a copy of this individual with a type axiom */
  def _type(clazzes : OWLClass*) = new OWLIndividual(resource,triples ++ (clazzes map {
    clazz => resource %> RDF._type %> clazz.resource
  }))
  
  /** Get a copy of this individual with a same as axiom */
  def sameAs(indivs : OWLIndividual*) = new OWLIndividual(resource,triples ++ (indivs map {
    indiv => resource %> OWL.sameAs %> indiv.resource
  }))
  
  /** Get a copy of this individual with a different from axiom */
  def differentFrom(indivs : OWLIndividual*) = new OWLIndividual(resource,triples ++ (indivs map {
    indiv => resource %> OWL.differentFrom %> indiv.resource
  }))
  
  /** Get all types for this individual */
  def _types = triples get(Some(resource),Some(RDF._type),None) map {
    case _ %> _ %> (res : Resource) =>  new OWLClass(res,triples)
    case stat => throw new OWLException(stat.obj + " was stated as a type of an individual but is not a resource")
  }
  
  /** Get all individuals that are equivalent to this individual */ 
  def sameIndividuals = triples get(Some(resource),Some(OWL.sameAs),None) map {
    case _ %> _ %> (res : Resource) =>  new OWLIndividual(res,triples)
    case stat => throw new OWLException(stat.obj + " was stated as the same as an individual but is not a resource")
  }
  
  /** Get all individuals that are different from this individual */
  def differentIndividuals = triples get(Some(resource),Some(OWL.differentFrom),None) map {
    case _ %> _ %> (res : Resource) =>  new OWLClass(res,triples)
    case stat => throw new OWLException(stat.obj + " was stated as different from an individual but is not a resource")
  }
  
  def frame = triples get(Some(resource),None,None)
  
  override def equals(o : Any) = o match {
    case prop : OWLAnnotationProperty => resource == prop.resource
    case _ => false
  }
  
  override def hashCode = resource.hashCode + 6
    
  override def toString = "OWLAnnotationProperty("+resource+")"
}

object OWLIndividual extends OWLCompanion[OWLIndividual] {
  def apply(resource : Resource) = new OWLIndividual(resource,TripleSet(resource %> RDF._type %> OWL.Thing))
  def apply(resource : Resource,triples : TripleSet) = new OWLIndividual(resource,triples)
  def unapply(entity : OWLEntity) : Option[Resource] = {
    entity.triples get(Some(entity.resource),Some(RDF._type),None) find {
      case _ %> _ %> OWL._Class => false
      case _ %> _ %> OWL.ObjectProperty => false
      case _ %> _ %> OWL.DatatypeProperty => false
      case _ %> _ %> OWL.AnnotationProperty => false
      case _ %> _ %> RDFS.Datatype => false
      case _ => true
    } match {
      case Some(_) => Some(entity.resource)
      case None => None
    }
  }
}   

class OWLException(message : String = "", cause : Throwable = null) extends RuntimeException(message,cause)
