package scalasemweb.rdf.model

import scalasemweb.rdf.model.aux._
import java.net.URI

/////////////////////////////////////////////////////////////////////////////////////////////
// Values

/**
 * Any RDF value 
 */
trait Value

/**
 * A Resource (non-literal) RDF value
 */
trait Resource extends Value {
  /** Construct a RDF pair */
  def %>(pred:NamedNode) = new %>(this,pred)
  def %>*(predObjs:PredObj*) : StatementSet = {
  	new StdStatementSet((predObjs map { 
			predObj => Statement(this, predObj._1,predObj._2)
    }).toSet)
  }
}

/**
 * A literal RDF value
 */
trait Literal extends Value {
  /** The string value of the literal */
  def stringValue:String
}

/**
 * A RDF Resource with a name (URI)
 */
trait NamedNode extends Resource {
  /** The unqualified URI of the resource */
  def uri:URI
  /** Construct a RDF pair */
  def %>(lit:Literal) = PredObj(this,lit)
  /** Construct a RDF pair */
  def %>(bn:BlankNode) = PredObj(this,bn)
}

/**
 * A blank RDF resource. That is a resource without a URI
 * @param id The id denoting the blank node in the document
 */
case class BlankNode(val id:String) extends Resource {
  override def toString = "_:" + id
}


/**
 * A single URI name
 * @param uri The URI value of the node
 */
case class URIRef(val uri:URI) extends NamedNode {
  override def toString = "<" + uri.toString + ">"
  override def equals(obj : Any) = obj match {
    case URIRef(uri2) => uri == uri2
    case QName(ns2,s2) => (ns2.prefix + s2) == (uri.toString)
    case _ => false
  }
  override def hashCode = uri.toString.hashCode 
}

/**
 * A URI generated from a name space
 * @param nameSpace The name space prefix
 * @param suffix The suffix for this URI
 */
case class QName(val nameSpace:NameSpace,val suffix:String) extends NamedNode {
  def uri = new URI(nameSpace.prefix + suffix)
  override def toString = nameSpace.id + ":" + suffix
  override def equals(obj : Any) = obj match {
    case URIRef(uri) => (nameSpace.prefix + suffix) == (uri.toString)
    case QName(ns2,s2) => nameSpace == ns2 && suffix == s2
    case _ => false
  }
  
  override def hashCode = (nameSpace.prefix + suffix).hashCode 
}

/**
 * A plain RDF literal
 * @param value The value of the literal
 */ 
case class SimpleLiteral(val value:String) extends Literal {
  def stringValue = value
  override def toString = "\"" + value + "\""
}

/**
 * A RDF literal qualified with a language tag
 * @param value The value of the literal
 * @param lang The language tag
 */
case class LangLiteral(val value:String, val lang:String) extends Literal {
  def stringValue = value
  override def toString = "\"" + value + "\"@" + lang
}

/**
 * A RDF literal with a type annotation
 * @param value The value of the literal
 * @param typ The type of the literal
 */
case class TypedLiteral(val value:String, val typ:NamedNode) extends Literal {
  def stringValue = value
  override def toString = "\"" + value + "\"^^" + typ
}

/////////////////////////////////////////////////////////////////////////////////////////////
// Statements

/**
 * Used to create objects and provide case matching
 */
class %>(val subject : Resource, val predicate : NamedNode) extends RDFPair[Resource,NamedNode] {
	def %>(obj : Value) = new Statement(this,obj)
  def %>*(vals:Value*) : StatementSet = new StdStatementSet((vals map { 
  	(value:Value) => new Statement(this,value) 
  }).toSet)
  def _1 = subject
  def _2 = predicate
  override def hashCode = {
  	subject.hashCode * 31 + predicate.hashCode
  }
}

object %> {
	def apply(subject : Resource, predicate : NamedNode) = new %>(subject,predicate)
	def unapply(stat : Statement) : Option[Tuple2[%>,Value]] = Some((stat.subjPred,stat.obj))
	def unapply(subjPred : %>) : Option[Tuple2[Resource,NamedNode]] = Some((subjPred.subject, subjPred.predicate))
}


/**
 * An RDF statement (triple).
 * @param subj The subject of the triple
 * @param prop The predicate (property) of the triple
 * @param obj The object of the triple
 */
class Statement(val subjPred : %>, val obj : Value) {
	override def toString = subj + " " + pred + " " + obj
  def subj = subjPred.subject
  def pred = subjPred.predicate  
  override def equals(o : Any) = {
  	o match {
  		case Statement(s,p,o) => subjPred.subject == s && subjPred.predicate == p && obj == o
  		case _ => false
  	}
  }
  override def hashCode = {
  	subjPred.hashCode * 31 + obj.hashCode
  }
}

object Statement {
	def apply(subject : Resource, predicate : NamedNode, obj : Value) = new Statement(new %>(subject,predicate),obj)
	def unapply(statement : Statement) : Option[Tuple3[Resource,NamedNode,Value]] = { 
		Some(statement.subj,statement.pred,statement.obj)
	}
}

/////////////////////////////////////////////////////////////////////////////////////////////
// Name space

/**
 * A name space for use with RDF
 * @param id The short identifier for the namespace
 * @param prefix The full URI prefix
 */
case class NameSpace(val id:String, val prefix:String) {
  /** Generate a new named resource with this name space and given suffix */
  def &(suffix:String) = QName(this,suffix)
}


/////////////////////////////////////////////////////////////////////////////////////////////
// StatementSet

import scala.collection._
import scala.collection.mutable.Builder

trait StatementSet extends Set[Statement] with SetLike[Statement,StatementSet] {
	def has(subject : Option[Resource], predicate : Option[NamedNode], obj : Option[Value]) : Boolean
	def get(subject : Option[Resource], predicate : Option[NamedNode], obj : Option[Value]) : StatementSet
	override def empty : StatementSet = new StdStatementSet(Set())
}

object StatementSet {
	def apply(statements : Statement*) = new StdStatementSet(statements.toSet)
	
	private def fromSet(statements : Set[Statement]) = new StdStatementSet(statements)
	
	def newBuilder : Builder[Statement, StatementSet] = {
	  new scala.collection.mutable.HashSet[Statement] mapResult {
	    x => fromSet(x) 
	  }
	}
}

class StdStatementSet(statements : Set[Statement]) extends StatementSet {
	def -(statement : Statement) = new StdStatementSet(statements - statement)
	def +(statement : Statement) = new StdStatementSet(statements + statement) 
	def contains(statement : Statement) = statements.contains(statement)
	def iterator = statements.iterator
	def has(subject : Option[Resource], predicate : Option[NamedNode], obj : Option[Value]) : Boolean = {
		subject match {
			case Some(subj) => {
				predicate match {
					case Some(pred) => {
						obj match {
							case Some(ob) => {
								statements exists { case Statement(s,p,o) => s == subj && p == pred && o == obj }
							}
							case None => {
								statements exists { case Statement(s,p,o) => s == subj && p == pred }
							}
						}
					}
					case None => {
						obj match {
							case Some(ob) => {
								statements exists { case Statement(s,p,o) => s == subj && o == ob }
							}
							case None => {
								statements exists { case Statement(s,p,o) => s == subj }
							}
						}
					}
				}
			}
			case None => {
				predicate match {
					case Some(pred) => {
						obj match {
							case Some(ob) => {
								statements exists { case Statement(s,p,o) => p == pred && o == ob }
							}
							case None => {
								statements exists { case Statement(s,p,o) => p == pred }
							}
						}
					}
					case None => {
						obj match {
							case Some(ob) => {
								statements exists { case Statement(s,p,o) => o == ob }
							}
							case None => {
								!statements.isEmpty
							}
						}
					}
				}
			}
		}
	}
	def get(subject : Option[Resource], predicate : Option[NamedNode], obj : Option[Value]) : StdStatementSet = {
		new StdStatementSet (subject match {
			case Some(subj) => {
				predicate match {
					case Some(pred) => {
						obj match {
							case Some(ob) => {
								statements filter ( _ == Statement(subj,pred,ob) )
							}
							case None => {
								statements filter { case Statement(s,p,o) => s == subj && p == pred }
							}
						}
					}
					case None => {
						obj match {
							case Some(ob) => {
								statements filter { case Statement(s,p,o) => s == subj && o == ob }
							}
							case None => {
								statements filter { case Statement(s,p,o) => s == subj }
							}
						}
					}
				}
			}
			case None => {
				predicate match {
					case Some(pred) => {
						obj match {
							case Some(ob) => {
								statements filter { case Statement(s,p,o) => p == pred && o == ob }
							}
							case None => {
								statements filter { case Statement(s,p,o) => p == pred }
							}
						}
					}
					case None => {
						obj match {
							case Some(ob) => {
								statements filter { case Statement(s,p,o) => o == ob }
							}
							case None => {
								statements
							}
						}
					}
				}
			}
		})
	}
}

/////////////////////////////////////////////////////////////////////////////////////////////
// Default Vocabulary

/** The XML Schema Description name space. Here are all the URIs defined for defining RDF data types */
object XSD extends NameSpace("xsd","http://www.w3.org/2001/XMLSchema") {
	val string = this&"string"
	val _boolean = this&"boolean"
	val _float = this&"float"
	val _double = this&"double"
	val decimal = this&"decimal"
	val duration = this&"duration"
	val dateTime = this&"dateTime"
	val time = this&"time"
	val date = this&"date"
	val gYearMonth = this&"gYearMonth"
	val gYear = this&"gYear"
	val gMonthDay = this&"gMonthDay"
	val gDay = this&"gDay"
	val gMonth = this&"gMonth"
	val hexBinary = this&"hexBinary"
	val base64Binary = this&"base64Binary"
	val anyURI = this&"anyURI"
	val _QName = this&"QName"
	val NOTATION = this&"NOTATION"
	val	normalizedString = this&"normalizedString"
	val token = this&"token"
	val language = this&"language"
	val IDREFS = this&"IDREFS"
	val ENTITIES = this&"ENTITIES"
	val NMTOKEN = this&"NMTOKEN"
	val NMTOKENS = this&"NMTOKENS"
	val Name = this&"Name"
	val NCName = this&"NCName"
	val ID = this&"ID"
	val IDREF = this&"IDREF"
	val ENTITY = this&"ENTITY"
	val integer = this&"integer" 
	val nonPositiveInteger = this&"nonPositiveInteger"
	val negativeInteger = this&"negativeInteger"
	val _long = this&"long"
	val _int = this&"int"
	val _short = this&"short"
	val _byte = this&"byte"
	val nonNegativeInteger = this&"nonNegativeInteger"
	val unsignedLong = this&"unsignedLong"
	val unsignedInt = this&"unsignedInt"
	val unsignedShort = this&"unsignedShort"
	val unsignedByte = this&"unsignedByte"
	val positiveInteger = this&"positiveInteger"
}

/** The Resource Description Framework name space. All the RDF URIs are defined here, also implicit conversions for common types are included here */


object RDF extends NameSpace("rdf", """http://www.w3.org/1999/02/22-rdf-syntax-ns#""") {
  private var _base = NameSpace("","")
  
  /** Get the (globally defined) base name space */
  def base : NameSpace = _base
  /** Set the (gloablly defined) base name space */
  def base_=(prefix:String) = _base = NameSpace("",prefix)

  val _type = this&"type"
  val Property = this&"Property"
  val _Statement = this&"Statement"
  val subject = this&"subject"
  val predicate = this&"predicate"
  val _object = this&"object" 
  val _Bag = this&"Bag"
  val _Seq = this&"Seq"
  val Alt = this&"Alt"
  val value = this&"value"
  val _List = this&"List"
  val nil = this&"nil"
  val first = this&"first"
  val rest = this&"rest"
  val XMLLiteral = this&"XMLLiteral"
}

/** The Resource Description Framework Schema name space */
object RDFS extends NameSpace("rdfs","http://www.w3.org/2000/01/rdf-schema#") {
  val _Resource = this&"Resource"
  val _Class = this&"Class"
  val subClassOf = this&"subClassOf"
  val subPropertyOf = this&"subPropertyOf"
  val comment = this&"comment"
  val label = this&"label"
  val domain = this&"domain"
  val range = this&"range"
  val seeAlso = this&"seeAlso"
  val isDefinedBy = this&"isDefinedBy"
  val _Literal = this&"Literal"
  val Container = this&"Container"
  val ContainerMembershipProperty = this&"ContainerMembershipProperty"
  val member = this&"member"
  val Datatype = this&"Datatype"
}
