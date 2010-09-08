package scala.rdf.sparql

import scala.util.parsing.combinator._
import scala.rdf.{Value=>RDFValue,_}
import java.net.URI;

/**
 * A SPARQL query
 * @param decls Any namespace headers
 * @param body The query body
 */
case class SPARQLQuery(val decls : List[NameSpace], val body : QueryBody) {
  private def nsToString(ns:NameSpace) = ns match {
    case NameSpace("",x) => "BASE <" + x + ">"
    case NameSpace(x,y) => "PREFIX " + x + ": <" + y + ">"
  }
  
  override def toString() = (decls.map(nsToString(_))).mkString("\n") + body
}

/**
 * The body of the query
 */
sealed trait QueryBody

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
case class SelectQuery(val modifier : Option[SelectModifier], val selection : List[Variable], 
  val datasetClauses : List[DatasetClause], val whereClause : List[GraphExpression], 
  val solutionModifier : SolutionModifier) 
  extends QueryBody{
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
  override def toString = "ASK "+ datasetClauses.mkString(" ") + 
  " WHERE{ " + whereClause.mkString(" .\n") + " }"
}

/**
 * A dataset selection clause
 */
trait DatasetClause

/**
 * Select data from a named graph
 * @param ref The named graph
 */
case class NamedGraphClause(val ref : NamedNode) extends DatasetClause {
  override def toString = "FROM NAMED " + ref
}

/**
 * Select data form a default graph
 * @param ref The default graph
 */
case class DefaultGraphClause(val ref : NamedNode) extends DatasetClause {
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
trait OrderCondition

/**
 * Order results by the value of an expression
 * @param ascending <code>true</code> for ascending sort
 * @param by The expression to evaluate for sorting
 */
case class AscDesc(val ascending : Boolean, val by : BrackettedExpression) extends OrderCondition {
  override def toString = (if(ascending) { "ASC " } else { "DESC " }) + by
}

/**
 * A graph expression. A list of these is a graph pattern
 */
trait GraphExpression

/**
 * Indicates an optional graph pattern
 * @param graphPattern The list of patterns unified
 */
case class OptionalGraphPattern(val graphPattern : List[GraphExpression]) extends GraphExpression {
  override def toString = "OPTIONAL { " + graphPattern.mkString(" .\n") + " }"
}

/**
 * Indicates a named sub graph
 * @param group The name of the sub graph
 * @param graphPattern The pattern for the graph
 */
case class GraphGraphPattern(val group : NamedNode, val graphPattern : List[GraphExpression]) 
  extends GraphExpression {
    override def toString = "GRAPH " + group + " { " + graphPattern.mkString(" .\n") + " }"
}

/**
 * Indicates a union of several graph patterns. 
 * @param graphPatterns The graph patterns to be unified, must not be empty
 */
case class UnionGraphPattern(val graphPatterns : List[List[GraphExpression]]) extends GraphExpression {
  require(!graphPatterns.isEmpty)
  override def toString = (graphPatterns map 
    { pattern => "{ " + pattern.mkString(" .\n") + " }" }).mkString(" UNION ")
}

/**
 * Indicate a filter on some values
 * @param constraint The constraint used by this filter
 */
case class Filter(val constraint : Constraint) extends GraphExpression {
  override def toString = "FILTER " + constraint
}

/**
 * A constraint used by a filter or an ordering
 */
trait Constraint extends OrderCondition

/**
 * A call to a named function
 * @param function The name of the function
 * @param args The arguments to be passed to the function
 */
case class FunctionCall(val function : NamedNode, args : List[LogicalExpression]) 
extends Constraint with PrimaryExpression {
  override def toString = function + "(" + args.mkString(",") + ")"
}

/**
 * The template used by a construct
 * @param body The body of the template. Must not be empty
 */
case class ConstructTemplate(val body : List[Triple]) {
  require(!body.isEmpty)
  override def toString = "{ " + body.mkString(" .\n") + " }"
}

/**
 * A triple. This preserves the form of the original query
 * @param head The head of the triple
 * @param body The associated predicates and objects. May be empty only if the head is a BNodeList or Collection
 */
case class Triple(val head : Resource, val body : List[PredicateObject]) extends GraphExpression {
  require(!body.isEmpty || head.isInstanceOf[BNodeList] || head.isInstanceOf[Collection])
  override def toString = head + " " + body.mkString(" ;\n")
  /** Convert to a list of statements */
  def toStats : List[Statement] = (head match {
    case x : BNodeList => x.toStats
    case x : Collection => x.toStats
    case x => Nil
  }) ::: (body flatMap { case PredicateObject(pred,objs) => (objs flatMap {
    obj => (obj match {
      case x : BNodeList => x.toStats
      case x : Collection => x.toStats
      case x => Nil
  }) ::: List(head ~> pred ~> obj)})})
}
    

/**
 * The right hand side of a SPARQL triple
 * @param pred The predicate
 * @param objs The List of objects, may not be empty
 */
case class PredicateObject(val pred : NamedNode, val objs : List[RDFValue]) {
  require(!objs.isEmpty)
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
case class Collection(val elems : List[RDFValue]) extends AnonymousNode {
  import scala.rdf.RDF._
  
  override def toString = "( " + elems.mkString(" ") + " )"
  /** Convert to a list of statements */
  def toStats : List[Statement] = {
    def _toStats(last : Resource, els : List[RDFValue]) : List[Statement] = els match {
      case x :: xs => {
        val node = new AnonymousNode
        (x match {
          case y : BNodeList => y.toStats
          case y : Collection => y.toStats
        case _ => Nil}) ::: (         
        (last ~> RDF.rest ~> node) ::
        (node ~> RDF.first ~> x) :: _toStats(node,xs))
      }
      case Nil => List(last ~> RDF.rest ~> RDF.nil)
    }
    if(elems.isEmpty) {
      Nil
    } else {
      (this ~> RDF.first ~> elems.head) ::
      _toStats(this, elems.tail)
    }
  }
}

/**
 * A blank node defined with a set of predicates and objects
 * @param body The body of the blank node declaration
 */
case class BNodeList(val body : List[PredicateObject]) extends AnonymousNode {
  override def toString = "[ " + body.mkString(" ;\n") + " ]"
  /** Convert to a list of statements */
  def toStats : List[Statement] = {
    body flatMap { case PredicateObject(pred,objs) => objs flatMap {
      obj => (obj match {
          case x : BNodeList => x.toStats
          case x : Collection => x.toStats
          case x => Nil
        }) ::: List(this ~> pred ~> obj)
      }
    }
  }
}

/**
 * A variable in a SPARQL expression
 * @param id The id of the variable (no ? or $)
 */
case class Variable(val id : String) extends NamedNode with Constraint with PrimaryExpression {
  override def toString = "?"+id
  /** The uri is of the form <code>var:x</code> */
  def uri = URI.create("var:"+id)
}

/**
 * A logical expression, resulting in a value
 */
trait LogicalExpression

/**
 * An or combination of one or more expressions
 * @param exprs The expressions
 */
case class OrExpression(val exprs : List[AndExpression]) extends LogicalExpression {
  require(!exprs.isEmpty)
  override def toString = exprs.mkString(" || ")
}

/**
 * An and combination of one or more expressions
 * @param exprs The expressions
 */
case class AndExpression(val exprs : List[ValueLogical]) {
  override def toString = exprs.mkString(" && ")
}

/**
 * A logical value
 */
trait ValueLogical extends LogicalExpression

/**
 * A logical operand expression
 * @param left Left operand
 * @param op Operator. One of =, !=, <, >;, <=, >=
 * @param right Right operand
 */
case class LogicalExpr(val left : NumericExpression, 
val op : String, val right : NumericExpression) extends ValueLogical {
  require(op == "=" || op == "<" || op == ">" || op == "!=" || op == "<=" || op == ">=")
  override def toString = left + " " + op + " " + right
}

/**
 * A numeric expression
 */
trait NumericExpression extends ValueLogical

/**
 * A summation operand expression
 * @param left Left operand
 * @param op Operator. One of +, -
 * @param right Right operand
 */
case class NumericExpr(val left : MultiplicativeExpression, 
val op : String, val right : NumericExpression) extends NumericExpression {
  require(op == "+" || op == "-")
  override def toString = left + " "+op+" " + right
}

/**
 * A numeric expression
 */
trait MultiplicativeExpression extends NumericExpression

/**
 * A multiplicative operand expression
 * @param left Left operand
 * @param op Operator. One of *, /
 * @param right Right operand
 */
case class MultiplicationExpr(val left : UnaryExpression, 
val op : String,val right : MultiplicativeExpression) extends MultiplicativeExpression {
  require(op == "*" || op == "/")
  override def toString = left + " "+op+" " + right
}

/**
 * A unary expression
 */
trait UnaryExpression extends MultiplicativeExpression

/**
 * A unary expression
 * @param op Operator. One of +, -, !
 * @param value The value operated on
 */
case class UnaryExpr(val op : String, val value : PrimaryExpression) extends UnaryExpression {
  require(op == "+" || op == "-" || op == "!")
  override def toString = "-"+value
}

/**
 * A primary expression
 */
trait PrimaryExpression extends UnaryExpression 

/**
 * An expression in brackets
 * @param expr The expression within the brackets
 */
case class BrackettedExpression(val expr : LogicalExpression) extends PrimaryExpression with Constraint {
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
    override def toString = function + "(" + arg1 + (arg2 match {
      case Some(x) => ","+x
      case None => ""
    }) + (arg3 match {
      case Some(x) => ","+x
      case None => ""
    }) + ")"
}

/**
 * Call to a named (not built-in) function
 * @param name The function name
 * @param args The list of arguments
 */
case class FunctionExpr(val name : NamedNode, val args : List[LogicalExpression]) extends PrimaryExpression {
  override def toString = name + "(" + args.mkString(",") + ")"
}

/**
 * A RDF value within an expression
 * @param value The RDF value
 */
case class ValueExpr(val value : RDFValue) extends PrimaryExpression {
  override def toString = value.toString 
}

/**
 * Parser to read SPARQL queries
 */
object SPARQLParser  {
  /**
   * Parse a SPARQL query as a string
   */
  def parse(q : String) = {
    val parser = new ParserImpl
    parser.parseAll(parser.query,q) match {
      case parser.Success(res,_) => res
      case x => throw new IllegalArgumentException(x.toString)
    }
  }
  
  private[SPARQLParser] class ParserImpl extends JavaTokenParsers {
    import scala.collection.mutable.HashMap    
  
    var prefixMap = new HashMap[String,NameSpace]()
    
    def query = prologue ~ ( selectQuery | constructQuery | describeQuery | askQuery ) ^^
    { case x ~ y => SPARQLQuery(x,y) }
  
    def prologue = (baseDecl?) ~ (prefixDecl*) ^^ 
    { case Some(x) ~ y => x :: y
      case None ~ y => y
    }
    
    def baseDecl = "BASE" ~> "<" ~> ("""[^>]+"""r) <~ ">" ^^
    { x => prefixMap.put("",NameSpace("",x)); NameSpace("",x) }
    
    def prefixDecl = "PREFIX" ~> pName ~ (":" ~> "<" ~> ("""[^>]+"""r) <~ ">") ^^
    { case x ~ y => prefixMap.put(x,NameSpace(x,y)); NameSpace(x,y) }
    
    def selectQuery = "SELECT" ~> (( "DISTINCT" ^^^ SelectModifier.distinct | "REDUCED" ^^^ SelectModifier.reduced)?) ~ 
    ( (Var+) | "*" ^^^ Nil ) ~ (datasetClause *) ~  whereClause ~ solutionModifier ^^
    { case a ~ b ~ c ~ d ~ e => SelectQuery(a,b,c,d,e) }
      
    def constructQuery = "CONSTRUCT" ~> constructTemplate ~ (datasetClause*) ~ whereClause ~ solutionModifier ^^
    { case x ~ y ~ z ~ w  => ConstructQuery(x,y,z,w) }
    
    def describeQuery = "DESCRIBE" ~> ( (varOrNamedNode+) | "*" ^^^ Nil ) ~ (datasetClause*) ~ 
      (whereClause?) ~ solutionModifier ^^
      { case x ~ y ~ z ~ w => DescribeQuery(x,y,z.getOrElse(Nil),w) }
      
    def askQuery = "ASK" ~> (datasetClause*) ~ whereClause ^^ 
    { case x ~ y => AskQuery(x,y) }
    
    def datasetClause : Parser[DatasetClause] = "FROM" ~> ( namedGraphClause | defaultGraphClause )
    
    def defaultGraphClause = sourceSelector ^^ { DefaultGraphClause(_) }
    
    def namedGraphClause = "NAMED" ~> sourceSelector ^^ { NamedGraphClause(_) }
    
    def sourceSelector = uriRef
    
    def whereClause = ("WHERE"?) ~> groupGraphPattern
    
    def solutionModifier = (orderClause?) ~ (limitOffsetClause?) ^^
    { case Some(x) ~ Some(y) => x ++ y
      case Some(x) ~ None => x
      case None ~ Some(x) => x
      case None ~ None => SolutionModifier()
    }
    
    def limitOffsetClause = limitClause ~ offsetClause ^^ 
    { case x ~ y => SolutionModifier(limit=Some(Integer.parseInt(x)),offset=Some(Integer.parseInt(y)))} | 
    limitClause ^^ { x => SolutionModifier(limit=Some(Integer.parseInt(x))) } | 
    offsetClause ^^ { x => SolutionModifier(offset=Some(Integer.parseInt(x))) }
    
    def limitClause = "LIMIT" ~> wholeNumber
    
    def offsetClause = "OFFSET" ~> wholeNumber
    
    def orderClause = "ORDER" ~> "BY" ~> orderCondition ^^ { case x => SolutionModifier(order=Some(x)) }
    
    def orderCondition = ( "ASC" | "DESC" ) ~ brackettedExpression  ^^
    { case x ~ y => AscDesc(x == "ASC", y) } |
                         constraint |
                         Var
    
    def groupGraphPattern : Parser[List[GraphExpression]] = "{" ~> repsep(expr,".") <~ "}"
    
    def expr = optionalGraphPattern | groupOrUnionGraphPattern | graphGraphPattern | filter | triplesSameSubject
    
    def optionalGraphPattern = "OPTIONAL" ~> groupGraphPattern ^^ { OptionalGraphPattern(_) }
    
    def graphGraphPattern = "GRAPH" ~> varOrNamedNode ~ groupGraphPattern ^^
    { case x ~ y => GraphGraphPattern(x,y) }
    
    def groupOrUnionGraphPattern = groupGraphPattern ~ (( "UNION" ~> groupGraphPattern )*) ^^
    { case x ~ y => UnionGraphPattern(x :: y) }
    
    def filter = "FILTER" ~> constraint ^^ { Filter(_) }
    
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
    
    def triplesNode = collection | blankNodePropertyList
    
    def blankNodePropertyList = "[" ~> propertyListNotEmpty <~ "]" ^^ { BNodeList(_) }
    
    def collection = "(" ~> (graphTerm+) <~ ")" ^^ { Collection(_) }
    
    def graphNode : Parser[Resource] = Var | triplesNode | blankNode | uriRef
    
    def varOrTerm : Parser[RDFValue] = Var | graphTerm
    
    def varOrNamedNode = Var | uriRef
    
    def Var : Parser[Variable] = var1 | var2
    
    def graphTerm : Parser[RDFValue] = Var | blankNode | uriRef | rdfLiteral | numericLiteral | booleanLiteral
    
    def expression = conditionalOrExpression
    
    def conditionalOrExpression = rep1sep(conditionalAndExpression, "||") ^^ { OrExpression(_) }
    
    def conditionalAndExpression = rep1sep(valueLogical, "&&")  ^^ { AndExpression(_) }
    
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
    
    def builtInCall : Parser[BuiltInCall] = 
                      "STR" ~> "(" ~> expression <~ ")"  ^^ { case x => BuiltInCall(BuiltIn.str, x) } |
                      "LANG" ~> "(" ~> expression <~ ")" ^^ { case x => BuiltInCall(BuiltIn.lang, x) } |
                      "LANGMATCHES" ~> "(" ~> (expression ~ ("," ~> expression)) <~ ")" ^^ 
                        { case x ~ y => BuiltInCall(BuiltIn.langmatches, x,Some(y)) } |
                      "DATATYPE" ~> "(" ~> expression <~ ")" ^^ { case x => BuiltInCall(BuiltIn.datatype,x) } |
                      "BOUND" ~> "(" ~> Var <~ ")" ^^ { case x => BuiltInCall(BuiltIn.bound,x) } |
                      "sameTerm" ~> "(" ~> (expression ~ ("," ~> expression)) <~ ")"  ^^
                      { case x ~ y => BuiltInCall(BuiltIn.sameTerm,x,Some(y)) } |
                      "isIRI" ~> "(" ~> expression <~ ")" ^^ { case x => BuiltInCall(BuiltIn.isIRI, x) } |
                      "isURI" ~> "(" ~> expression <~ ")" ^^ { case x => BuiltInCall(BuiltIn.isURI, x) } |
                      "isBLANK" ~> "(" ~> expression <~ ")" ^^ { case x => BuiltInCall(BuiltIn.isBLANK, x) } |
                      "isLITERAL" ~> "(" ~> expression <~ ")" ^^ 
                      { case x => BuiltInCall(BuiltIn.isLITERAL, x) } |
                      regexExpression
                      
    def regexExpression = "REGEX" ~> "(" ~> ((expression <~ ",") ~ (expression) ~ 
      (("," ~> expression)?)) <~ ")" ^^
      { case x ~ y ~ z => BuiltInCall(BuiltIn.regex, x, Some(y), z) }
    
    def uriRefOrFunction = uriRef ~ (argList?) ^^
    { case x ~ Some(y) => FunctionExpr(x,y) 
      case x ~ None => x }
      
    
    def rdfLiteral = strng ~ ( "^^" ~> uriRef ) ^^ { case x ~ y => TypedLiteral(x,y) } |
      strng ~ langTag ^^ { case x ~ y => LangLiteral(x,y) } |
      string ^^ { case x => SimpleLiteral(x) }
    
    def numericLiteral = decimalNumber ^^ 
      { case x => TypedLiteral(x,XSD._double) }
    
    def booleanLiteral = "true" ^^^ TypedLiteral("true",XSD._boolean) | 
      "false" ^^^ TypedLiteral("false",XSD._boolean)
    
    def blankNode = anon | blankNodeID
    
    def blankNodeID = regex("""_:[A-Za-z0-9_]+"""r) ^^ 
      { case x => BlankNode(x.substring(2)) }
    
    def anon = literal("[ ]") ^^ { x => new AnonymousNode() }
    
    def langTag = regex("""@[a-z][a-z0-9]*"""r) ^^ 
      { case x => x.substring(1) }
    
    def uriRef = uriref | qname
    
    def qname = (((prefixName ?) <~ ":") ~ pName) ^^
    { case Some(x) ~ y => {
        if(!prefixMap.contains(x)) {
          throw new IllegalArgumentException("Prefix " + x + " not declared")
        }
        prefixMap(x)&y
        }
      case None ~ y => {
        if(!prefixMap.contains("")) {
         throw new IllegalArgumentException("Base prefix not declared")
        }
        prefixMap("")&y
      }
    }
        
    def uriref = ("<" ~> relativeURI <~ ">") ^^ 
      { case x => URIRef(URI.create(x)) }
    
    def relativeURI = """[^ <>]+"""r
        
    def pName = """[A-Za-z_][^ <>]*"""r
        
    def prefixName = regex("""[A-Za-z][^ <>:]*"""r)
        
    def strng = longString | string
        
    def string = regex("\"[^\"]*\""r) ^^ { case x => x.substring(1,x.length-1) }
        
    def longString = regex(("\"\"\"" + """(([^"])|(\"))+?""" + "\"\"\"")r) ^^ 
      { case x => x.substring(3,x.length - 3) }
     
      
    def var1 = "?" ~> pName ^^ 
      { case x => Variable(x) }
    
    def var2 = "$" ~> pName ^^ 
      { case x => Variable(x) }
  }
}
  
