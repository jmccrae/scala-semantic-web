package scalasemweb.owl

import scalasemweb.rdf.model._
import scalasemweb.rdf.model.collections._

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

trait OWLEntity extends StatementSet{
  def resource : Resource
  def annotations(annos : Tuple2[NamedNode,Value]*) = {
    new StdStatementSet(this ++ (annos map {
      anno => resource %> anno._1 %> anno._2
    }))
  }
}

trait OWLClass extends OWLEntity {
  def subClassOf(clazzes : OWLClass*) : StatementSet = {
    new StdStatementSet(this ++ (clazzes flatMap { 
      clazz => clazz + (resource %> RDFS.subClassOf %> clazz.resource) 
    }))
  }
  def equivalentClass(clazzes : OWLClass*) : StatementSet = {
    new StdStatementSet(this ++ (clazzes flatMap { 
      clazz => clazz + (resource %> OWL.equivalentClass %> clazz.resource)
    }))
  }
  def disjointWith(clazzes : OWLClass*) : StatementSet = {
    new StdStatementSet(this ++ (clazzes flatMap { 
      clazz => clazz + (resource %> OWL.disjointWith %> clazz.resource)
    }))
  }
  def disjointUnionOf(clazzList : Iterable[OWLClass]) : StatementSet = {
    val list = RDFList(clazzList map (_.resource))
    new StdStatementSet(this + (resource %> OWL.disjointUnionOf %> list.node) ++ list ++ (clazzList flatMap (x=>x)))
  }
}

trait OWLClassExpr extends OWLClass {
 /* def subClasses : Set[OWLClassExpr] = {
    get(Some(resource), Some(RDFS.subClassOf), None) map {
      case res : Resource => new StdStatementSet(this) with OWLClassExpr {
        def resource = res
      }
      case x => throw new OWLException(x + " was stated as a sub class but is not a resource")
    }
  }
  def equivalentClasses : Set[OWLClassExpr] = {
    get(Some(resource), Some(OWL.equivalentClass), None) map {
      case res :Resource => new StdStatementSet(this) with OWLClassExpr {
        def resource = res
      }
      case x => throw new OWLException(x + " was stated as an equivalent class but is not a resource")
    }
  }
  def disjointClasses : Set[OWLClassExpr] = {
    get(Some(resource), Some(OWL.equivalentClass), None) map {
      case res : Resource => new StdStatementSet(this) with OWLClassExpr {
        def resource = res
      }
      case x => throw new OWLException(x + " was stated as a disjoint class but is not a resource")
    }
  }
  def disjointUnions : Set[List[OWLClassExpr]] = {
    get(Some(resource), Some(OWL.disjointUnionOf), None) flatMap {
      case res : Resource => RDFList.getList(res, this) match {
        case Some(l) => List(l.toValueList map {
          case r : Resource => new StdStatementSet(this) with OWLClassExpr {
            def resource = r
          }
          case v => throw new OWLException(v + " was stated as a disjoint union of a class but is not a resource")
        })
        case None => Nil
      }
      case x => throw new OWLException(x + " was stated as a disjoint union list but is not a resource")
    }
  }*/
}

object OWLClass {
  def apply(res : Resource) : OWLClassExpr = new StdStatementSet(Set(res %> RDF._type %> OWL._Class)) with OWLClassExpr {
    def resource = res
  }
  def unapply(ss : StatementSet) : Option[Resource] = {
    if(ss.size == 1) {
      ss.head match {
        case res %> RDF._type %> OWL._Class => Some(res)
        case _ => None
      }
    } else {
      None
    }
  }
}

trait ObjectProperty extends OWLEntity {
  def domain(clazzes : OWLClass*) : StatementSet = {
    new StdStatementSet(this ++ (clazzes flatMap {
      clazz => clazz + (resource %> RDFS.domain %> clazz.resource)
    }))
  }
  def range(clazzes : OWLClass*) : StatementSet = {
    new StdStatementSet(this ++ (clazzes flatMap {
      clazz => clazz + (resource %> RDFS.range %> clazz.resource)
    }))
  }
  def subPropertyOf(objProps : ObjectProperty*) : StatementSet = {
    new StdStatementSet(this ++ (objProps flatMap {
      objProp => objProp + (resource %> RDFS.subPropertyOf %> objProp.resource)
    }))
  }
}

class OWLException(message : String = "", cause : Throwable = null) extends RuntimeException(message,cause)
