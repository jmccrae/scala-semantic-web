package scala.rdf
import java.net.URI

/**
 * Any RDF value 
 */
trait Value

/**
 * A Resource (non-literal) RDF value
 */
trait Resource extends Value {
  /** Construct a RDF pair */
  def ~>(pred:NamedNode) = SubjPred[Resource](this,pred)
  def ~>(predObjs:RDFPair[NamedNode,Value]*) = (predObjs map { 
      predObj => Statement(this, predObj._1,predObj._2)
    }).toList
    
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
  def ~>(lit:Literal) = PredObj(this,lit)
  /** Construct a RDF pair */
  override def ~>(res:NamedNode) = SubjPred[NamedNode](this,res)
  /** Construct a RDF pair */
  def ~>(bn:BlankNode) = PredObj(this,bn)
  
  def ~>*(vals:Value*) = vals map { value => PredObj(this,value) }
}

/**
 * A blank RDF resource. That is a resource without a URI
 * @param id The id denoting the blank node in the document
 */
case class BlankNode(val id:String) extends Resource {
  override def toString = "_:" + id
}

/**
 * An anonymous blank RDF resource.
 */
class AnonymousNode extends BlankNode(Math.abs((new java.util.Random()).nextLong()).toString()) 

/**
 * An RDF statement (triple).
 * @param subj The subject of the triple
 * @param prop The predicate (property) of the triple
 * @param obj The object of the triple
 */
case class Statement(val subj:Resource, val pred:NamedNode, val obj:Value) {
  override def toString = subj + " " + pred + " " + obj
  /** Create a list of statements including this statement, and a new statement whose subject is this statements's subject and the new predicate + object values */
  def &&(predObj : RDFPair[NamedNode,Value]) = StatList(this :: List(Statement(subj,predObj._1,predObj._2)),subj)
  
  /** A list of statements for a given value. (Used for constructing triples) */
  case class StatList(val stats : List[Statement], val subj : Resource) {
    def &&(predObj : RDFPair[NamedNode,Value]) = StatList(stats ::: List(Statement(subj,predObj._1,predObj._2)),subj)
    def $ = stats
  }
}

/**
 * A name space for use with RDF
 * @param id The short identifier for the namespace
 * @param prefix The full URI prefix
 */
case class NameSpace(val id:String, val prefix:String) {
  /** Generate a new named resource with this name space and given suffix */
  def &(suffix:String) = QName(this,suffix)
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

/** Any pair of RDF values. (Used for constructing triples) */
trait RDFPair[+T <: Value,+U <: Value] {
  def _1 : T
  def _2 : U
}

/** Any pair of RDF values. (Used for constructing triples) */
case class SubjPred[+T <: Resource](val subj : T , val pred : NamedNode) extends RDFPair[T,NamedNode] {
  /** Construct a statement using this subject and predicate and given value */
  def ~>(value:Value) = Statement(subj,pred,value)
  /** Construct a statement for each given value, using this subject and predicate and the value */
  def ~>*(values:Value*) = (values map { value => Statement(subj,pred,value)}).toList 
  def _1 = subj
  def _2 = pred
}

/** Any pair of RDF values. (Used for constructing triples) */
case class PredObj(val pred : NamedNode, val obj : Value) extends RDFPair[NamedNode,Value] {
  def _1 = pred
  def _2 = obj
}

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
object sugar {
  /** Convert a string to a plain literal */
  implicit def str2lit(str:String) = SimpleLiteral(str)
  /** Convert a string to typed literal, lang literal or URI ref */
  implicit def str2vb(str:String) = new  {
    def ^^(typ:NamedNode) = TypedLiteral(str,typ)
    def @@(lang:String) = LangLiteral(str,lang)
    def uri = URIRef(URI.create(str))
  }
  /** Convert a symbol to a QName in the RDF base namespace */
  implicit def sym2qn(sym:Symbol) = RDF.base&(sym.name)
  /** Convert a triple to a statement */
  implicit def trip2stat(trip:Tuple3[Any,Any,Any]) = trip match { 
    case (subj,prop,obj) => new { def rdf = new Statement(toResource(subj),
                                                      toNamedNode(prop),
                                                      toValue(obj)) } }
  
  /** Convert a URI to a URIref */
  implicit def uri2ref(uri : URI) = URIRef(uri)
  /** Convert a named node to a URI */
  implicit def ref2uri(ref : NamedNode) = ref.uri    
  
  private def toResource(r:Any) = r match {
    case x : Resource => x
    case x : Symbol => sym2qn(x)
    case x : URI => uri2ref(x)
    case _ => throw new IllegalArgumentException(r.toString + " is not a resource")
  }

  private def toNamedNode(r : Any) = r match {
    case x : NamedNode => x
    case x : Symbol => sym2qn(x)
    case x : URI => uri2ref(x)
    case _ => throw new IllegalArgumentException(r.toString + " is not a named resource")
  }

  private def toValue(r : Any) = r match {
    case x : Value => x
    case x : String => str2lit(x)
    case x : Symbol => sym2qn(x)
    case x : URI => uri2ref(x)
    case _ => throw new IllegalArgumentException(r.toString + " is not a value")
  }
}

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
