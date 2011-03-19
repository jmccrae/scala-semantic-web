package scalasemweb.owl

import scala.collection._
import scalasemweb.rdf.model._
import scalasemweb.rdf.collection._

/////////////////////////////////////////////////////////////////////////////////
// OWL name space definition

/** The OWL name space */
object OWL extends NameSpace("owl","""http://www.w3.org/2002/07/owl#""") {  
  // Primary Types
  val Ontology                     = this&"Ontology"
  val _Class                       = this&"Class"
  val AnnotationProperty           = this&"AnnotationProperty"
  val DatatypeProperty             = this&"DatatypeProperty"
  val OntologyProperty             = this&"OntologyProperty"
  val ObjectProperty               = this&"ObjectProperty"
  val NamedIndividual              = this&"NamedIndividual"
  val Axiom                        = this&"Axiom"
    
  // Deprecating types
  val DeprecatedProperty           = this&"DeprecatedProperty"
  val DeprecatedClass              = this&"DeprecatedClass"
  
  // Fixed elements
  val Thing                        = this&"Thing"
  val Nothing                      = this&"Nothing"
  val topObjectProperty            = this&"topObjectProperty"
  val topDataProperty              = this&"topDataProperty"
  val bottomObjectProperty         = this&"bottomObjectProperty"
  val bottomDataProperty           = this&"bottomDataProperty"
    
  // Class relation
  val disjointWith                 = this&"disjointWith"
  val equivalentClass              = this&"equivalentClass"
  val unionOf                      = this&"unionOf"
  val intersectionOf               = this&"intersectionOf"
  val disjointUnionOf              = this&"disjointUnionOf"
  val complementOf                 = this&"complementOf"
    
  // Class complexes: Restictions
  val Restriction                  = this&"Restriction"
  val maxQualifiedCardinality      = this&"maxQualifiedCardinality"
  val maxCardinality               = this&"maxCardinality"
  val hasValue                     = this&"hasValue"
  val hasSelf                      = this&"hasSelf"
  val cardinality                  = this&"cardinality"
  val minCardinality               = this&"minCardinality"
  val minQualifiedCardinality      = this&"minQualifiedCardinality"
  val qualifiedCardinality         = this&"qualifiedCardinality"
  val allValuesFrom                = this&"allValuesFrom"
  val someValuesFrom               = this&"someValuesFrom"
  val onDataRange                  = this&"onDataRange"
  val onProperties                 = this&"onProperties"
  val onDatatype                   = this&"onDatatype"
  val onProperty                   = this&"onProperty"
  val onClass                      = this&"onClass"
    
  // Data ranges
  val DataRange                    = this&"DataRange"
  val withRestrictions             = this&"withRestrictions"
  val datatypeComplementOf         = this&"datatypeComplementOf"
  val oneOf                        = this&"oneOf"
  
  // Property relations
  val equivalentProperty           = this&"equivalentProperty"
  val inverseOf                    = this&"inverseOf"
  val propertyDisjointWith         = this&"propertyDisjointWith"
  
  // Property sub-types
  val SymmetricProperty            = this&"SymmetricProperty"
  val TransitiveProperty           = this&"TransitiveProperty"
  val ReflexiveProperty            = this&"ReflexiveProperty"
  val InverseFunctionalProperty    = this&"InverseFunctionalProperty"
  val IrreflexiveProperty          = this&"IrreflexiveProperty"
  val FunctionalProperty           = this&"FunctionalProperty"
  val AsymmetricProperty           = this&"AsymmetricProperty"
  
  // Individual relations
  val sameAs                       = this&"sameAs"
  val differentFrom                = this&"differentFrom"
  
  // Negative property assertion vocabularies
  val NegativePropertyAssertion    = this&"NegativePropertyAssertion"
  val targetIndividual             = this&"targetIndividual"
  val assertionProperty            = this&"assertionProperty"
  val sourceIndividual             = this&"sourceIndividual"
  val targetValue                  = this&"targetValue"
  
  // Axiom annotations
  val Annotation                   = this&"Annotation"
  val annotatedSource              = this&"annotatedSource"
  val annotatedProperty            = this&"annotatedProperty"
  val annotatedTarget              = this&"annotatedTarget"
    
  // Disjunctions
  val AllDisjointProperties        = this&"AllDisjointProperties"
  val AllDifferent                 = this&"AllDifferent"
  val AllDisjointClasses           = this&"AllDisjointClasses"
  val distinctMembers              = this&"distinctMembers"
  val members                      = this&"members"
  
  // Keys
  val hasKey                       = this&"hasKey"
  
  // Annotation properties
  val deprecated                   = this&"deprecated"
  val versionIRI                   = this&"versionIRI"
  val priorVersion                 = this&"priorVersion"
  val backwardCompatibleWith       = this&"backwardCompatibleWith"
  val incompatibleWith             = this&"incompatibleWith"
  val versionInfo                  = this&"versionInfo"
  
  // Property Chains
  val propertyChainAxiom           = this&"propertyChainAxiom"
  
  // Ontology Properties
  val imports                      = this&"imports"
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
  
  /** Create a new version of this entity with the given annotation */
  def annotation(annos : Tuple2[OWLAnnotationProperty,OWLAnnotationValue]*) = {
    make(resource,annos.foldLeft(triples)((x,y) => x + (resource %> y._1.resource %> y._2.value)))
  }
  
  /** Get all annotations attached to this element */
  def annotations = {
    triples.get(Some(resource),None,None) flatMap {
      case _ %> p %> o => try {
        val ap = OWLAnnotationProperty(p,triples)
        val av = o match {
          case av : Literal => new OWLAnnotationValue {  def value = av }
          case av : Resource => {
            try {
              OWLIndividual(av,triples)
            } catch {
              case x : OWLNoSuchEntityException => new OWLAnnotationValue { def value = av }
            }
          }
        }
        Some(ap -> av)
      } catch {
        case x : OWLNoSuchEntityException => None
      }
    }
  }
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

/** Anything that may act as an owl annotation value */
trait OWLAnnotationValue {
  def value : Value 
}


/////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Exceptions

/** An error within the OWL api */
class OWLException(message : String = "", cause : Throwable = null) extends RuntimeException(message,cause)

/** An invalid triple was encoutered in the ontology */
class OWLFormatException(message : String = "", cause : Throwable = null) extends OWLException(message,cause)

/** An attempt was made to create an object, that was not supported by the triples used to describe it */
class OWLNoSuchEntityException(message : String = "", cause : Throwable = null) extends OWLException(message,cause)
