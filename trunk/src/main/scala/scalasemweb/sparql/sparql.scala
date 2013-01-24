package scalasemweb.sparql

import scala.util.parsing.combinator._
import scalasemweb.rdf.model.{Value=>RDFValue,Triple=>RDFTriple,_}
import java.net.URI

trait SPARQLElement

// Todo: This only supports SPARQL 1.0
// Need to update to SPARQL 1.1

/**
 * A SPARQL query
 * @param decls Any namespace headers
 * @param body The query body
 */
case class SPARQLQuery(val decls : List[NameSpace], val body : QueryBody) extends SPARQLElement{
  private def nsToString(ns:NameSpace) = ns match {
    case NameSpace("",x) => "BASE <" + x + ">"
    case NameSpace(x,y) => "PREFIX " + x + ": <" + y + ">"
  }
  
  def filter(p : (RDFValue) => Boolean) : List[RDFValue] = body.filter(p)
  
  def vars : Set[Variable] = filter(_.isInstanceOf[Variable]).map{
    case v : Variable => v
    case _ => throw new RuntimeException("unreachable") 
  }.toSet
    
  override def toString() = (decls.map(nsToString(_))).mkString("\n") + 
  (if(!decls.isEmpty) { "\n" } else { "" }) + body
}

//object SPARQLQuery {
  /**
   * The body of the query
   */
  sealed trait QueryBody {
    def filter(p : (RDFValue) => Boolean) : List[RDFValue]
  }
  
  /**
   * A modifier to a select query
   */
  class SelectModifier(str : String) {
    override def toString() = str
  }
  
  object SelectModifier {
    /**
     * The query returns distinct values
     */
    val distinct = new SelectModifier("DISTINCT")
    /**
     * Permits the query to remove duplicate values
     */
    val reduced = new SelectModifier("REDUCED")
  }
  
  /**
   * A select query
   * @param modifier The duplicate removal option
   * @param selection The list of variables to be selected or an empty list for all variables
   * @param datasetClauses The datasets to use for the query
   * @param whereClause The main search clause
   * @param solutionModifier Modifiers to the result (limit, offset, order)
   */
  case class SelectQuery(val modifier : Option[SelectModifier], val selection : List[VariableOrAggregate], 
    val datasetClauses : List[DatasetClause], val whereClause : List[GraphExpression], 
    val solutionModifier : SolutionModifier) 
    extends QueryBody{
      def filter(p : (RDFValue) => Boolean) : List[RDFValue] = 
        selection.filter(p) ::: datasetClauses.flatMap(_.filter(p)) ::: 
        whereClause.flatMap(_.filter(p)) ::: solutionModifier.filter(p)
        
      override def toString() = "SELECT " + (if(modifier.isEmpty) { "" } else { modifier.get + " " }) +
    (if(selection.isEmpty) {"*"} else { selection.mkString(" ") }) + datasetClauses.mkString(" ") +
    "\nWHERE{ " + whereClause.mkString(" .\n") + " }\n" + solutionModifier
  }
  
  /**
   * A construct query
   * @param constructTemplate The result to be constructed
   * @param datasetClauses The datasets to use for the query
   * @param whereClause The main search clause
   * @param solutionModifier Modifiers to the result (limit, offset, order)
   */
  case class ConstructQuery(val constructTemplate : ConstructTemplate, val datasetClauses : List[DatasetClause],
    val whereClause : List[GraphExpression], val solutionModifier : SolutionModifier) extends QueryBody {
    def filter(p : (RDFValue) => Boolean) : List[RDFValue] = constructTemplate.filter(p) :::
      datasetClauses.flatMap(_.filter(p)) :::
      whereClause.flatMap(_.filter(p)) ::: solutionModifier.filter(p)
    
    
      override def toString = "CONSTRUCT " + datasetClauses.mkString(" ") + "\nWHERE{ " + whereClause.mkString(" .\n") + " }\n" + solutionModifier
    }
    
  /**
   * A descibe query
   * @param selection The list of variables or URIs to be described
   * @param datasetClauses The datasets to use for the query
   * @param whereClause The main search clause
   * @param solutionModifier Modifiers to the result (limit, offset, order)
   */
  case class DescribeQuery(val variables : List[NamedNode], val datasetClauses : List[DatasetClause],
    val whereClause : List[GraphExpression], val solutionModifier : SolutionModifier) extends QueryBody {
      def filter(p : (RDFValue) => Boolean) : List[RDFValue] = variables.filter(p) :::
        datasetClauses.flatMap(_.filter(p)) :::
        whereClause.flatMap(_.filter(p)) ::: solutionModifier.filter(p)
      
      override def toString = "DESCRIBE " + (if(variables.isEmpty) { "*" } else { variables.mkString(" ") }) +
      " " + datasetClauses.mkString(" ") + "\n" + (if(whereClause.isEmpty) { "" } else { "\nWHERE{ " + whereClause.mkString(" .\n") + " }\n" }) + solutionModifier
  }
  
  /**
   * An ask query
   * @param datasetClauses The datasets to use for the query
   * @param whereClause The main search clause
   */
  case class AskQuery(val datasetClauses : List[DatasetClause], val whereClause : List[GraphExpression]) 
  extends QueryBody {
    def filter(p : (RDFValue) => Boolean) : List[RDFValue] = datasetClauses.flatMap(_.filter(p)) ::: 
    whereClause.flatMap(_.filter(p))
    
    override def toString = "ASK "+ datasetClauses.mkString(" ") + 
    " WHERE{ " + whereClause.mkString(" .\n") + " }"
  }
  
  /**
   * A dataset selection clause
   */
  trait DatasetClause {
   def filter(p : (RDFValue) => Boolean) : List[RDFValue]
  }
  
  /**
   * Select data from a named graph
   * @param ref The named graph
   */
  case class NamedGraphClause(val ref : NamedNode) extends DatasetClause {
    def filter(p : (RDFValue) => Boolean) : List[RDFValue] = if(p(ref)) { List(ref) } else { Nil }
    
    override def toString = "FROM NAMED " + ref
  }
  
  /**
   * Select data form a default graph
   * @param ref The default graph
   */
  case class DefaultGraphClause(val ref : NamedNode) extends DatasetClause {
    def filter(p : (RDFValue) => Boolean) : List[RDFValue] = if(p(ref)) { List(ref) } else { Nil }
    
    override def toString = "FROM " + ref.toString
  }
  
  /**
   * Modifiers to a search
   * @param limit The maximum number of resutls
   * @param offset The offset of the first result
   * @param order A method for ordering results
   */
  case class SolutionModifier(val limit : Option[Int] = None, 
    val offset : Option[Int] = None, 
    val order : Option[OrderCondition] = None) {
    override def toString = (limit match {
      case Some(x) => "LIMIT " + x + "\n"
      case None => ""
    }) + (offset match {
      case Some(x) => "OFFSET " + x + "\n"
      case None => ""
    }) + (order match {
      case Some(x) => "ORDER BY " + x + "\n"
      case None => ""
    })
    
    def filter(p : (RDFValue) => Boolean) : List[RDFValue] = order match {
      case Some(x) => x.filter(p)
      case None => Nil
    }
    
    private def join[T](opt1 : Option[T], opt2 : Option[T]) : Option[T] = opt1 match {
      case Some(x) => opt2 match {
        case Some(_) => throw new IllegalArgumentException("Both options set")
        case None => Some(x)
      }
      case None => opt2 match {
        case Some(x) => Some(x)
        case None => None
      }
    }
    
    def ++(m2 : SolutionModifier) = SolutionModifier(join(limit,m2.limit), 
    join(offset,m2.offset), join(order,m2.order))
  }
  
  /**
   * A condition for ordering results
   */
  trait OrderCondition {
    def filter(p : (RDFValue) => Boolean) : List[RDFValue]
  }
  
  /**
   * Order results by the value of an expression
   * @param ascending <code>true</code> for ascending sort
   * @param by The expression to evaluate for sorting
   */
  case class AscDesc(val ascending : Boolean, val by : BrackettedExpression) extends OrderCondition {
    def filter(p : (RDFValue) => Boolean) : List[RDFValue] = by.filter(p)
    
    override def toString = (if(ascending) { "ASC " } else { "DESC " }) + by
  }
  
  /**
   * A graph expression. A list of these is a graph pattern
   */
  trait GraphExpression {
    def filter(p : (RDFValue) => Boolean) : List[RDFValue]
  }
  
  /**
   * Indicates an optional graph pattern
   * @param graphPattern The list of patterns unified
   */
  case class OptionalGraphPattern(val graphPattern : List[GraphExpression]) extends GraphExpression {
    def filter(p : (RDFValue) => Boolean) : List[RDFValue] = graphPattern.flatMap(_.filter(p))
    
    override def toString = "OPTIONAL { " + graphPattern.mkString(" .\n") + " }"
  }
  
  /**
   * Indicates a named sub graph
   * @param group The name of the sub graph
   * @param graphPattern The pattern for the graph
   */
  case class GraphGraphPattern(val group : NamedNode, val graphPattern : List[GraphExpression]) 
    extends GraphExpression {
      def filter(p : (RDFValue) => Boolean) : List[RDFValue] = if(p(group)) { 
        group :: graphPattern.flatMap(_.filter(p))
      } else {
        graphPattern.flatMap(_.filter(p))
      }
      
      override def toString = "GRAPH " + group + " { " + graphPattern.mkString(" .\n") + " }"
  }
  
  /**
   * Indicates a union of several graph patterns. 
   * @param graphPatterns The graph patterns to be unified, must not be empty
   */
  case class UnionGraphPattern(val graphPatterns : List[List[GraphExpression]]) extends GraphExpression {
    require(!graphPatterns.isEmpty)
    def filter(p : (RDFValue) => Boolean) : List[RDFValue] = graphPatterns.flatMap(_.flatMap(_.filter(p)))
    
    override def toString = (graphPatterns map 
      { pattern => "{ " + pattern.mkString(" .\n") + " }" }).mkString(" UNION ")
  }
  
  /**
   * Indicate a filter on some values
   * @param constraint The constraint used by this filter
   */
  case class Filter(val constraint : Constraint) extends GraphExpression {
    def filter(p : (RDFValue) => Boolean) : List[RDFValue] = constraint.filter(p)
    
    override def toString = "FILTER " + constraint
  }
  
  /**
   * A constraint used by a filter or an ordering
   */
  trait Constraint extends OrderCondition {
    def filter(p : (RDFValue) => Boolean) : List[RDFValue]
  }
  
  /**
   * A call to a named function
   * @param function The name of the function
   * @param args The arguments to be passed to the function
   */
  case class FunctionCall(val function : NamedNode, args : List[LogicalExpression]) 
  extends Constraint with PrimaryExpression {
    def filter(p : (RDFValue) => Boolean) : List[RDFValue] = args.flatMap(_.filter(p))
    
    override def toString = function + "(" + args.mkString(",") + ")"
  }
  
  /**
   * The template used by a construct
   * @param body The body of the template. Must not be empty
   */
  case class ConstructTemplate(val body : List[Triple]) {
    require(!body.isEmpty)
    def filter(p : (RDFValue) => Boolean) : List[RDFValue] = body.flatMap(_.filter(p))
    
    override def toString = "{ " + body.mkString(" .\n") + " }"
  }
  
  /**
   * A triple. This preserves the form of the original query
   * @param head The head of the triple
   * @param body The associated predicates and objects. May be empty only if the head is a BNodeList or Collection
   */
  case class Triple(val head : Resource, val body : List[PredicateObject]) extends GraphExpression {
    require(!body.isEmpty || head.isInstanceOf[BNodeList] || head.isInstanceOf[Collection])
    def filter(p : (RDFValue) => Boolean) : List[RDFValue] = (head match {
      case c : Collection => c.filter(p)
      case b : BNodeList => b.filter(p)
      case _ => Nil
    }) :::  (if(p(head)) {
      head :: body.flatMap(_.filter(p))
    } else {
      body.flatMap(_.filter(p))
    })
    
    override def toString = head + " " + body.mkString(" ;\n")
    /** Convert to a list of statements */
    def toStats : List[RDFTriple] = (head match {
      case x : BNodeList => x.toStats
      case x : Collection => x.toStats
      case x => Nil
    }) ::: (body flatMap { case PredicateObject(pred,objs) => (objs flatMap {
      obj => (obj match {
        case x : BNodeList => x.toStats
        case x : Collection => x.toStats
        case x => Nil
    }) ::: List(head %> pred %> obj)})})
  }
      
  
  /**
   * The right hand side of a SPARQL triple
   * @param pred The predicate
   * @param objs The List of objects, may not be empty
   */
  case class PredicateObject(val pred : NamedNode, val objs : List[RDFValue]) {
    require(!objs.isEmpty)
    def filter(p : (RDFValue) => Boolean) : List[RDFValue] = if(p(pred)) {
      pred :: objs.filter(p)
    } else {
      objs.filter(p)
    }
    
    override def toString = (if(pred == RDF._type) {
      "a " 
    } else {
      pred + " " 
    }) + objs.mkString(" ,")
  }
  
  /**
   * An RDF Collection. Note this object is only the first blank node in the collection
   * @param elems The elements in the collection
   */
  class Collection(val elems : List[RDFValue], id : String) extends BlankNode(id) {
    import scalasemweb.rdf.model.RDF._
    
    def filter(p : (RDFValue) => Boolean) : List[RDFValue] = elems.flatMap{ x => x match {
      case c : Collection => c.filter(p) ::: (if(p(c)) { List(c) } else { Nil })
      case b : BNodeList => b.filter(p) ::: (if(p(b)) { List(b) } else { Nil })
      case _ => if(p(x)) { List(x) } else { Nil }
      }
    }
    
    override def toString = "( " + elems.mkString(" ") + " )"
    /** Convert to a list of statements */
    def toStats : List[RDFTriple] = {
      def _toStats(last : Resource, els : List[RDFValue]) : List[RDFTriple] = els match {
        case x :: xs => {
          val node = AnonymousNode()
          (x match {
            case y : BNodeList => y.toStats
            case y : Collection => y.toStats
          case _ => Nil}) ::: (         
          (last %> RDF.rest %> node) ::
          (node %> RDF.first %> x) :: _toStats(node,xs))
        }
        case Nil => List(last %> RDF.rest %> RDF.nil)
      }
      if(elems.isEmpty) {
        Nil
      } else {
        (this %> RDF.first %> elems.head) ::
        _toStats(this, elems.tail)
      }
    }
  }
  
  /**
   * A blank node defined with a set of predicates and objects
   * @param body The body of the blank node declaration
   */
  class BNodeList(val body : List[PredicateObject],id : String) extends BlankNode(id) {
    override def toString = "[ " + body.mkString(" ;\n") + " ]"
    def filter(p : (RDFValue) => Boolean) : List[RDFValue] = body.flatMap(_.filter(p))
    
    /** Convert to a list of statements */
    def toStats : List[RDFTriple] = {
      body flatMap { case PredicateObject(pred,objs) => objs flatMap {
        obj => (obj match {
            case x : BNodeList => x.toStats
            case x : Collection => x.toStats
            case x => Nil
          }) ::: List(this %> pred %> obj)
        }
      }
    }
  }
  
  trait VariableOrAggregate extends Constraint with RDFValue
  
  /**
   * A variable in a SPARQL expression
   * @param id The id of the variable (no ? or $)
   */
  case class Variable(val id : String) extends NamedNode with VariableOrAggregate {
    def filter(p : (RDFValue) => Boolean) : List[RDFValue] = if(p(this)) { List(this) } else { Nil }
    
    override def toString = "?"+id
    /** The uri is of the form <code>var:x</code> */
    def uri = URI.create("var:"+id)
  }
  
  trait Aggregate extends VariableOrAggregate {
    def filter(p : (RDFValue) => Boolean) : List[RDFValue] = throw new UnsupportedOperationException() 
  }
  
  case class CountAggregate(val distinct : Boolean, val expr : Option[ValueLogical]) extends Aggregate {
    override def toString = "COUNT("+(if(distinct) { "DISTINCT " } else { ""})+expr.getOrElse("*")+")"
  }
  
  case class SumAggregate(val distinct : Boolean, val expr : ValueLogical) extends Aggregate {
    override def toString = "SUM("+(if(distinct) { "DISTINCT " } else { ""})+expr+")"
  }
  
  case class MinAggregate(val distinct : Boolean, val expr : ValueLogical) extends Aggregate {
    override def toString = "MIN("+(if(distinct) { "DISTINCT " } else { ""})+expr+")"
  }
  
  case class MaxAggregate(val distinct : Boolean, val expr : ValueLogical) extends Aggregate {
    override def toString = "MAX("+(if(distinct) { "DISTINCT " } else { ""})+expr+")"
  }
  
  case class AvgAggregate(val distinct : Boolean, val expr : ValueLogical) extends Aggregate {
    override def toString = "AVG("+(if(distinct) { "DISTINCT " } else { ""})+expr+")"
  }
  
  case class SampleAggregate(val distinct : Boolean, val expr : ValueLogical) extends Aggregate {
    override def toString = "SAMPLE("+(if(distinct) { "DISTINCT " } else { ""})+expr+")"
  }
  
  case class GroupConcatAggregate(val distinct : Boolean, val expr : ValueLogical) extends Aggregate {
    override def toString = "GROUP_CONCAT("+(if(distinct) { "DISTINCT " } else { ""})+expr+")"
  }
  
  /**
   * A logical expression, resulting in a value
   */
  trait LogicalExpression {
   def filter(p : (RDFValue) => Boolean) : List[RDFValue]
  }
  
  /**
   * An or combination of one or more expressions
   * @param exprs The expressions
   */
  case class OrExpression(val exprs : List[AndExpression]) extends LogicalExpression {
    require(!exprs.isEmpty)
    def filter(p : (RDFValue) => Boolean) : List[RDFValue] = exprs.flatMap(_.filter(p))
    
    override def toString = exprs.mkString(" || ")
  }
  
  /**
   * An and combination of one or more expressions
   * @param exprs The expressions
   */
  case class AndExpression(val exprs : List[ValueLogical]) {
    def filter(p : (RDFValue) => Boolean) : List[RDFValue] = exprs.flatMap(_.filter(p))
    
    override def toString = exprs.mkString(" && ")
  }
  
  /**
   * A logical value
   */
  trait ValueLogical extends LogicalExpression {
    def filter(p : (RDFValue) => Boolean) : List[RDFValue]
  }
  
  /**
   * A logical operand expression
   * @param left Left operand
   * @param op Operator. One of =, !=, <, >;, <=, >=
   * @param right Right operand
   */
  case class LogicalExpr(val left : NumericExpression, 
  val op : String, val right : NumericExpression) extends ValueLogical {
    require(op == "=" || op == "<" || op == ">" || op == "!=" || op == "<=" || op == ">=")
    
    def filter(p : (RDFValue) => Boolean) : List[RDFValue] = left.filter(p) ::: right.filter(p)
    
    override def toString = left + " " + op + " " + right
  }
  
  /**
   * A numeric expression
   */
  trait NumericExpression extends ValueLogical {
    def filter(p : (RDFValue) => Boolean) : List[RDFValue]
  }
  
  /**
   * A summation operand expression
   * @param left Left operand
   * @param op Operator. One of +, -
   * @param right Right operand
   */
  case class NumericExpr(val left : MultiplicativeExpression, 
  val op : String, val right : NumericExpression) extends NumericExpression {
    require(op == "+" || op == "-")
    def filter(p : (RDFValue) => Boolean) : List[RDFValue] = left.filter(p) ::: right.filter(p)
    
    override def toString = left + " "+op+" " + right
  }
  
  /**
   * A numeric expression
   */
  trait MultiplicativeExpression extends NumericExpression {
    def filter(p : (RDFValue) => Boolean) : List[RDFValue]
  }
  
  /**
   * A multiplicative operand expression
   * @param left Left operand
   * @param op Operator. One of *, /
   * @param right Right operand
   */
  case class MultiplicationExpr(val left : UnaryExpression, 
  val op : String,val right : MultiplicativeExpression) extends MultiplicativeExpression {
    require(op == "*" || op == "/")
    def filter(p : (RDFValue) => Boolean) : List[RDFValue] = left.filter(p) ::: right.filter(p)
    
    override def toString = left + " "+op+" " + right
  }
  
  /**
   * A unary expression
   */
  trait UnaryExpression extends MultiplicativeExpression {
    def filter(p : (RDFValue) => Boolean) : List[RDFValue]
  }
  
  /**
   * A unary expression
   * @param op Operator. One of +, -, !
   * @param value The value operated on
   */
  case class UnaryExpr(val op : String, val value : PrimaryExpression) extends UnaryExpression {
    require(op == "+" || op == "-" || op == "!")
    def filter(p : (RDFValue) => Boolean) : List[RDFValue] = value.filter(p)
    
    override def toString = "-"+value
  }
  
  /**
   * A primary expression
   */
  trait PrimaryExpression extends UnaryExpression {
    def filter(p : (RDFValue) => Boolean) : List[RDFValue]
  }
  
  /**
   * An expression in brackets
   * @param expr The expression within the brackets
   */
  case class BrackettedExpression(val expr : LogicalExpression) extends PrimaryExpression with Constraint {
    def filter(p : (RDFValue) => Boolean) : List[RDFValue] = expr.filter(p)
    
    override def toString = "( " + expr + " )"
  }
  
  /**
   * Enumeration of built in operators. (see companion)
   */
  final class BuiltIn(value : String) {
    override def toString = value
  }
  
  object BuiltIn  {
    val str = new BuiltIn("STR")
    val lang = new BuiltIn("LANG")
    val langmatches = new BuiltIn("LANGMATCHES")
    val datatype = new BuiltIn("DATATYPE")
    val bound = new BuiltIn("BOUND")
    val sameTerm = new BuiltIn("sameTerm")
    val isIRI = new BuiltIn("isIRI")
    val isURI = new BuiltIn("isURI")
    val isBLANK = new BuiltIn("isBLANK")
    val isLITERAL = new BuiltIn("isLITERAL")
    val regex = new BuiltIn("REGEX")
  }
  
  /**
   * A call to a built-in function
   * @param function The function
   * @param arg1 The first argument
   * @param arg2 The second argument (if any)
   * @param arg3 The third argument (if any, regex only)
   */
  case class BuiltInCall(val function : BuiltIn, val arg1 : LogicalExpression, 
    val arg2 : Option[LogicalExpression] = None, val arg3 : Option[LogicalExpression] = None) 
    extends PrimaryExpression with Constraint {
      def filter(p : (RDFValue) => Boolean) : List[RDFValue] = arg1.filter(p) ::: (arg2 match {
        case Some(expr) => expr.filter(p)
        case None => Nil
      }) ::: (arg3 match {
        case Some(expr) => expr.filter(p)
        case None => Nil
      })
    
      override def toString = function + "(" + arg1 + (arg2 match {
        case Some(x) => ","+x
        case None => ""
      }) + (arg3 match {
        case Some(x) => ","+x
        case None => ""
      }) + ")"
  }
  
  /**
   * A RDF value within an expression
   * @param value The RDF value
   */
  case class ValueExpr(val value : RDFValue) extends PrimaryExpression {
    def filter(p : (RDFValue) => Boolean) : List[RDFValue] = value match {
      case c : Collection => c.filter(p) ::: (if(p(c)) { List(c) } else { Nil })
      case b : BNodeList => b.filter(p) ::: (if(p(b)) { List(b) } else { Nil })
      case x => if(p(x)) { List(x) } else { Nil }
    }
    
    override def toString = value.toString 
  }
//}

/**
 * Parser to read SPARQL queries
 */
object SPARQLParser  {
  import SPARQLQuery._
  /**
   * Parse a SPARQL query as a string
   * @param q The query
   * @param prefixes Extra prefixes to include implicitly
   * @throws SPARQLParseException If the query is not syntactically valid
   */
  def parse(q : String, prefixes : Map[String,String] = Map(), strict : Boolean = false) = {
    val parser = new ParserImpl(strict)
    for((prefix,uri) <- prefixes) {
      parser.prefixMap.put(prefix,NameSpace(prefix,uri))
    }
    // Skip all new line characters
    parser.parseAll(parser.query,q) match {
      case parser.Success(res,_) => res
      case x => throw new SPARQLParseException(x.toString)
    }
  }
  
  private[SPARQLParser] class ParserImpl(strict : Boolean) extends JavaTokenParsers {
    import scala.collection.mutable.HashMap    
  
    var prefixMap = new HashMap[String,NameSpace]()
    private var bNodeNo = 0
    
    def query = prologue ~ ( selectQuery | constructQuery | describeQuery | askQuery ) ^^
    { case x ~ y => SPARQLQuery(x,y) }
  
    def prologue = (baseDecl?) ~ (prefixDecl*) ^^ 
    { case Some(x) ~ y => x :: y
      case None ~ y => y
    }
    
    def baseDecl = """(?i)\QBASE\E""".r ~> uriref2 ^^
    { x => prefixMap.put("",NameSpace("",x)); NameSpace("",x) }
    
    def prefixDecl = """(?i)\QPREFIX\E""".r ~> pNameColon ~ uriref2 ^^
    { case x ~ y => prefixMap.put(x,NameSpace(x,y)); NameSpace(x,y) }
    
    def selectQuery = """(?i)\QSELECT\E""".r ~> (( """(?i)\QDISTINCT\E""".r ^^^ SelectModifier.distinct | """(?i)\QREDUCED\E""".r ^^^ SelectModifier.reduced)?) ~ 
    varSelectBlock ~ (datasetClause *) ~  whereClause ~ solutionModifier ^^
    { case a ~ b ~ c ~ d ~ e => SelectQuery(a,b,c,d,e) }
      
    // In non-strict mode, allow variables to be separated by ","
    def varSelectBlock = if(strict) {
      ( (Var+) | "*" ^^^ Nil )
    } else {
      ( rep1sep(varOrAggregate,",".?)  | "*" ^^^ Nil )
    }
    
    def constructQuery = """(?i)\QCONSTRUCT\E""".r ~> constructTemplate ~ (datasetClause*) ~ whereClause ~ solutionModifier ^^
    { case x ~ y ~ z ~ w  => ConstructQuery(x,y,z,w) }
    
    def describeQuery = """(?i)\QDESCRIBE\E""".r ~> ( (varOrNamedNode+) | "*" ^^^ Nil ) ~ (datasetClause*) ~ 
      (whereClause?) ~ solutionModifier ^^
      { case x ~ y ~ z ~ w => DescribeQuery(x,y,z.getOrElse(Nil),w) }
      
    def askQuery = """(?i)\QASK\E""".r ~> (datasetClause*) ~ whereClause ^^ 
    { case x ~ y => AskQuery(x,y) }
    
    def datasetClause : Parser[DatasetClause] = """(?i)\QFROM\E""".r ~> ( namedGraphClause | defaultGraphClause )
    
    def defaultGraphClause = sourceSelector ^^ { DefaultGraphClause(_) }
    
    def namedGraphClause = """(?i)\QNAMED\E""".r ~> sourceSelector ^^ { NamedGraphClause(_) }
    
    def sourceSelector = uriRef
    
    def whereClause = ("""(?i)\QWHERE\E""".r.?) ~> groupGraphPattern
    
    def solutionModifier = (orderClause?) ~ (limitOffsetClause?) ^^
    { case Some(x) ~ Some(y) => x ++ y
      case Some(x) ~ None => x
      case None ~ Some(x) => x
      case None ~ None => SolutionModifier()
    }
    
    def limitOffsetClause = if(strict) {
      limitClause ~ offsetClause ^^ 
      { case x ~ y => SolutionModifier(limit=Some(Integer.parseInt(x)),offset=Some(Integer.parseInt(y)))} | 
      limitClause ^^ { x => SolutionModifier(limit=Some(Integer.parseInt(x))) } | 
      offsetClause ^^ { x => SolutionModifier(offset=Some(Integer.parseInt(x))) }
    } else {
      limitClause ~ offsetClause ^^ 
      { case x ~ y => SolutionModifier(limit=Some(Integer.parseInt(x)),offset=Some(Integer.parseInt(y)))} | 
      offsetClause ~ limitClause ^^ 
      { case y ~ x => SolutionModifier(limit=Some(Integer.parseInt(x)),offset=Some(Integer.parseInt(y)))} | 
      limitClause ^^ { x => SolutionModifier(limit=Some(Integer.parseInt(x))) } | 
      offsetClause ^^ { x => SolutionModifier(offset=Some(Integer.parseInt(x))) }
    }
    
    def limitClause = """(?i)\QLIMIT\E""".r ~> wholeNumber
    
    def offsetClause = """(?i)\QOFFSET\E""".r ~> wholeNumber
    
    def orderClause = """(?i)\QORDER\E""".r ~> """(?i)\QBY\E""".r ~> orderCondition ^^ { case x => SolutionModifier(order=Some(x)) }
    
    def orderCondition = ( """(?i)\QASC\E""".r | """(?i)\QDESC\E""".r ) ~ (if(strict) { brackettedExpression } else { brackettedExpression | sqBrackettedExpression }) ^^
    { case x ~ y => AscDesc(x == "ASC", y) } |
                         constraint |
                         Var
    
    def groupGraphPattern : Parser[List[GraphExpression]] = "{" ~> rep(expr) <~ "}" ^^ { x => x.flatten }
    
    def expr : Parser[List[GraphExpression]] = graphPatternNotTriples ^^ { p => List(p) } | rep1sep(triplesSameSubject,".") <~ ".".? 
    
    def graphPatternNotTriples = (optionalGraphPattern | groupOrUnionGraphPattern | graphGraphPattern | filter) <~ ".".?
    
    def optionalGraphPattern = """(?i)\QOPTIONAL\E""".r ~> groupGraphPattern ^^ { OptionalGraphPattern(_) }
    
    def graphGraphPattern = """(?i)\QGRAPH\E""".r ~> varOrNamedNode ~ groupGraphPattern ^^
    { case x ~ y => GraphGraphPattern(x,y) }
    
    def groupOrUnionGraphPattern = groupGraphPattern ~ (( """(?i)\QUNION\E""".r ~> groupGraphPattern )*) ^^
    { case x ~ y => UnionGraphPattern(x :: y) }
    
    def filter = """(?i)\QFILTER\E""".r ~> constraint ^^ { Filter(_) }
    
    def constraint : Parser[Constraint] = brackettedExpression | builtInCall | functionCall
    
    def functionCall = uriRef ~ argList ^^ { case x ~ y => FunctionCall(x,y) }
  
    def argList : Parser[List[LogicalExpression]] = "(" ~> repsep(expression, ",") <~ ")"
  
    def constructTemplate = "{" ~> rep1sep(triplesSameSubject,".") <~ "}" ^^ { ConstructTemplate(_) }
    
    def triplesSameSubject = graphNode ~ propertyListNotEmpty  ^^ 
    { case x ~ y => Triple(x,y) } | 
    triplesNode ~ propertyList ^^
    { case x ~ y => Triple(x,y) }
    
    def propertyListNotEmpty = rep1sep(verb ~ objectList, ";") ^^
    { case list => list map { case x ~ y => PredicateObject(x,y) } }
    
    def propertyList = repsep(verb ~ objectList, ";") ^^ 
    { case list => list map { case x ~ y => PredicateObject(x,y) } }
    
    def objectList = rep1sep(objct, ",")
    
    def objct = graphTerm
    
    def verb = varOrNamedNode | "a" ^^^ RDF._type
    
    def triplesNode : Parser[Resource] = collection | blankNodePropertyList
    
    def blankNodePropertyList : Parser[Resource] = "[" ~> propertyListNotEmpty <~ "]" ^^ {
      bNodeNo += 1
      new BNodeList(_,bNodeNo.toString()) 
    }
    
    def collection : Parser[Resource] = "(" ~> (graphTerm+) <~ ")" ^^ { 
      bNodeNo += 1 
      new Collection(_,bNodeNo.toString()) 
    }
    
    def graphNode : Parser[Resource] = Var | triplesNode | blankNode | uriRef
    
    def varOrTerm : Parser[RDFValue] = Var | graphTerm
    
    def varOrNamedNode = Var | uriRef
    
    def Var : Parser[Variable] = var1 | var2 
    
    def varOrAggregate = Var | aggregate
    
    def aggregate =  "(?i)\\QCOUNT\\E".r ~> "(" ~> "(?i)\\QDISTINCT\\E".r.? ~ ( "*" | valueLogical ) <~ ')' ^^ {
          case x ~ "*" => CountAggregate(x != None, None)
          case x ~ (y : ValueLogical) => CountAggregate(x != None,Some(y))
        } | 
        "(?i)\\QSUM\\E".r ~> "(" ~> "(?i)\\QDISTINCT\\E".r.? ~ valueLogical <~ ")" ^^ { case x ~ y => SumAggregate(x != None,y) } |
        "(?i)\\QMIN\\E".r ~> "(" ~> "(?i)\\QDISTINCT\\E".r.? ~ valueLogical <~ ")" ^^ { case x ~ y => MinAggregate(x != None,y) } |
        "(?i)\\QMAX\\E".r ~> "(" ~> "(?i)\\QDISTINCT\\E".r.? ~ valueLogical <~ ")"  ^^ { case x ~ y => MaxAggregate(x != None,y) } |
        "(?i)\\QAVG\\E".r ~> "(" ~> "(?i)\\QDISTINCT\\E".r.? ~ valueLogical <~ ")"  ^^ { case x ~ y => SumAggregate(x != None,y) } |
        "(?i)\\QSAMPLE\\E".r ~> "(" ~> "(?i)\\QDISTINCT\\E".r.? ~ valueLogical <~ ")"  ^^ { case x ~ y => SumAggregate(x != None,y) } 
    
    def graphTerm : Parser[RDFValue] = Var | blankNode | uriRef | rdfLiteral | numericLiteral | booleanLiteral
    
    def expression = conditionalOrExpression
    
    def conditionalOrExpression = rep1sep(conditionalAndExpression, orSymbol) ^^ { OrExpression(_) }
    
    def orSymbol : Parser[String] = if(strict) { "||" } else { "||" | "(?i)\\QOR\\E".r }
    
    def conditionalAndExpression = rep1sep(valueLogical, andSymbol)  ^^ { AndExpression(_) }
    
    def andSymbol : Parser[String] = if(strict) { "&&" } else { "&&" | "(?i)\\QAND\\E".r }
    
    def valueLogical = (numericExpression ~ "=" ~ numericExpression |
                        numericExpression ~ "!=" ~ numericExpression |
                        numericExpression ~ "<" ~ numericExpression |
                        numericExpression ~ ">" ~ numericExpression |
                        numericExpression ~ "<=" ~ numericExpression |
                        numericExpression ~ ">=" ~ numericExpression) ^^ 
                      {case x ~ y ~ z => LogicalExpr(x,y,z) } |  
                      numericExpression
    
    def numericExpression = multiplicativeExpression ~ (( "+" ~ multiplicativeExpression |
                                                          "-" ~ multiplicativeExpression)*) ^^ 
      { case x ~ y => buildSummation(x,y) }
      
    def buildSummation(x : MultiplicativeExpression , ys : List[~[String,MultiplicativeExpression]]) 
     : NumericExpression = ys match 
    {
      case (p ~ y) :: ys2 => NumericExpr(x, p,buildSummation(y,ys2))
      case Nil => x
    }
   
    
    def multiplicativeExpression = unaryExpression ~ (( "*" ~ unaryExpression |
                                                        "/" ~ unaryExpression )*) ^^
      { case x ~ y => buildMultiplicative(x,y) }
      
    def buildMultiplicative(x : UnaryExpression , ys : List[~[String,UnaryExpression]]) 
     : MultiplicativeExpression = ys match {
      case (p ~ y) :: ys2 => MultiplicationExpr(x, p,buildMultiplicative(y,ys2))
      case Nil => x
    }
                                                        
    def unaryExpression = ("!" ~ primaryExpression |
                          "+" ~ primaryExpression |
                          "-" ~ primaryExpression) ^^ { case x ~ y => UnaryExpr(x,y) } |
                          primaryExpression
                          
    def primaryExpression : Parser[PrimaryExpression] = 
      brackettedExpression | builtInCall | (( uriRefOrFunction | rdfLiteral | 
      numericLiteral | booleanLiteral | Var ) ^^ {
        case x : RDFValue => ValueExpr(x)
        case x : PrimaryExpression => x
      })
      
    def brackettedExpression : Parser[BrackettedExpression] = "(" ~> expression <~ ")" ^^
    { case expr => BrackettedExpression(expr) }
    
    def sqBrackettedExpression : Parser[BrackettedExpression] = "[" ~> expression <~ "]" ^^
    { case expr => BrackettedExpression(expr) }
    
    
    def builtInCall : Parser[BuiltInCall] = 
                      """(?i)\QSTR\E""".r ~> "(" ~> expression <~ ")"  ^^ { case x => BuiltInCall(BuiltIn.str, x) } |
                      """(?i)\QLANG\E""".r ~> "(" ~> expression <~ ")" ^^ { case x => BuiltInCall(BuiltIn.lang, x) } |
                      """(?i)\QLANGMATCHES\E""".r ~> "(" ~> (expression ~ ("," ~> expression)) <~ ")" ^^ 
                        { case x ~ y => BuiltInCall(BuiltIn.langmatches, x,Some(y)) } |
                      """(?i)\QDATATYPE\E""".r ~> "(" ~> expression <~ ")" ^^ { case x => BuiltInCall(BuiltIn.datatype,x) } |
                      """(?i)\QBOUND\E""".r ~> "(" ~> Var <~ ")" ^^ { case x => BuiltInCall(BuiltIn.bound,ValueExpr(x)) } |
                      "(?i)\\QsameTerm\\E".r ~> "(" ~> (expression ~ ("," ~> expression)) <~ ")"  ^^
                      { case x ~ y => BuiltInCall(BuiltIn.sameTerm,x,Some(y)) } |
                      "(?i)\\QisIRI\\E".r ~> "(" ~> expression <~ ")" ^^ { case x => BuiltInCall(BuiltIn.isIRI, x) } |
                      "(?i)\\QisURI\\E".r ~> "(" ~> expression <~ ")" ^^ { case x => BuiltInCall(BuiltIn.isURI, x) } |
                      "(?i)\\QisBLANK\\E".r ~> "(" ~> expression <~ ")" ^^ { case x => BuiltInCall(BuiltIn.isBLANK, x) } |
                      "(?i)\\QisLITERAL\\E".r ~> "(" ~> expression <~ ")" ^^ 
                      { case x => BuiltInCall(BuiltIn.isLITERAL, x) } |
                      regexExpression
                      
    def regexExpression = """(?i)\QREGEX\E""".r ~> "(" ~> ((expression <~ ",") ~ (expression) ~ 
      (("," ~> expression)?)) <~ ")" ^^
      { case x ~ y ~ z => BuiltInCall(BuiltIn.regex, x, Some(y), z) }
    
    def uriRefOrFunction = uriRef ~ (argList?) ^^
    { case x ~ Some(y) => FunctionCall(x,y) 
      case x ~ None => x }
      
    
    def rdfLiteral = strng ~ ( "^^" ~> uriRef ) ^^ { case x ~ y => TypedLiteral(x,y) } |
      strng ~ langTag ^^ { case x ~ y => LangLiteral(x,y) } |
      strng ^^ { case x => SimpleLiteral(x) }
    
    def numericLiteral = decimalNumber ^^ 
      { case x => TypedLiteral(x,XSD._double) }
    
    def booleanLiteral = "true" ^^^ TypedLiteral("true",XSD._boolean) | 
      "false" ^^^ TypedLiteral("false",XSD._boolean)
    
    def blankNode = anon | blankNodeID
    
    def blankNodeID = regex("""_:[A-Za-z0-9_]+"""r) ^^ 
      { case x => BlankNode(x.substring(2)) }
    
    def anon = "[" ~> "]" ^^ { x => AnonymousNode() }
    
    def langTag = regex("""@[a-z][a-z0-9]*"""r) ^^ 
      { case x => x.substring(1) }
    
    def uriRef = uriref | qname
    
    def qname = pNameColon ~ pName ^^
    { case x ~ y => {
        if(!prefixMap.contains(x)) {
          throw new SPARQLParseException("Prefix " + x + " not declared")
        }
        prefixMap(x)&y
        }
    }
        
    val pnCharsBase = "[A-Za-z\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02FF\u0370-\u037D\u037F-\u1FFF\u200C-\u200D\u2070-\u218F\u2C00-\u2FEF\u3001-\uD7FF\uF900-\uFDCF\uFDF0-\uFFFD]"
    
    val pnChars = "[\\-_0-9\u00B7\u0300-\u036F\u203F-\u2040A-Za-z\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02FF\u0370-\u037D\u037F-\u1FFF\u200C-\u200D\u2070-\u218F\u2C00-\u2FEF\u3001-\uD7FF\uF900-\uFDCF\uFDF0-\uFFFD]"
    
    val varChars =
    "[A-Za-z_0-9\u00B7\u0300-\u036F\u203F-\u2040\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02FF\u0370-\u037D\u037F-\u1FFF\u200C-\u200D\u2070-\u218F\u2C00-\u2FEF\u3001-\uD7FF\uF900-\uFDCF\uFDF0-\uFFFD]"
    
    
    def uriref = ("<" ~> relativeURI <~ ">") ^^ 
      { case x => try { URIRef(URI.create(x)) } catch { case ex : IllegalArgumentException => throw new SPARQLParseException(ex.getMessage,ex) } }
    
    def uriref2 = ("<" ~> relativeURI <~ ">")
      
    def relativeURI = """[^ <>{}\|^`\\]+"""r
        
    def pName = (pnCharsBase + pnChars+"""*""")r 
    
    def pNameColon = regex((pnCharsBase + pnChars+"""*:""")r) ^^ { x => x.substring(0,x.length-1) } |
      ":" ^^^ ""
    
    def prefixName = regex((pnCharsBase + pnChars+"""*""")r)
        
    def strng = longString | string1 | string2
    
    def string1 = regex("'" + """(([^'])|(\'))*?""" + "(?<!\\\\)'"r) ^^ { case x => x.substring(1,x.length-1) }
    
    def string2 = regex("\"" + """(([^"])|(\"))*?""" + "(?<!\\\\)\""r) ^^ { case x => x.substring(1,x.length-1) }
        
    def longString = regex(("\"\"\"" + """(([^"])|(\"))*?""" + "\"\"\"")r) ^^ 
      { case x => x.substring(3,x.length - 3) }
     
      
    def var1 = "?" ~> ((varChars+"+")r) ^^ 
      { case x => Variable(x) }
    
    def var2 = "$" ~> ((varChars+"+")r) ^^ 
      { case x => Variable(x) }
  }
}
  
class SPARQLParseException(message : String, exception : Throwable = null) extends RuntimeException(message,exception) 
