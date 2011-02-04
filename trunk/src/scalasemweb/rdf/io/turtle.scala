package scalasemweb.rdf.io

import scalasemweb.rdf.model._
import java.io._
import scala.collection.mutable.{HashMap,LinkedList,HashSet}

object Turtle extends RDFWriter with RDFParser {
  private val printer = new TurtlePrinter()
  
  def write(statSet : StatementSet[Statement]) : String = printer.format(statSet)
  
  def write(statSet : StatementSet[Statement], out : Appendable) { printer.format(statSet,out) }
  
  def parse(doc:String) : StatementSet[Statement] = TurtleParser.parse(doc)
  
  def parse(in: java.io.Reader) : StatementSet[Statement] = TurtleParser.parse(in)
}

/**
 * Create a Turtle pretty printer
 * @param tabWidth The number of spaces in a tab
 * @param postStatSpacing The number of new lines after each statement
 * @param maxObjs The maximum number of objects to insert before a line break
 */
class TurtlePrinter(tabWidth : Int = 2, postStatSpacing : Int = 2, maxObjs : Int = 3) {
  private implicit def str2rep(str : String) = new {
    def rep(x:Int) = {
      val sb = new StringBuffer
      for(i <- 1 to x) {
        sb.append(str)
      }
      sb.toString
    }
  }
  
  /**
   * Format a list of statements in Turtle
   * @param statList The list of statements
   */
  def format(statList : StatementSet[Statement]) : String = {
    val writer = new StringWriter();
    format(statList,writer)
    writer.toString
  }
  
  /** Format a list of statements in Turtle
   * @param out The appendable buffer to add the result to
   */
  def format(statList : StatementSet[Statement], out : Appendable) : Unit = {
    val (theMap,nameSpaces,dupes) = buildMap(statList) 
    
    nameSpaces += RDF
    
    for(ns <- nameSpaces) {
      if(ns.id == "") {
        out.append("@base <"+ns.prefix + "> ." + "\n".rep(postStatSpacing-1))
      } else {
        out.append("@prefix " + ns.id + ": <"+ns.prefix+"> ." + "\n".rep(postStatSpacing-1))
      }
    }
    
    out.append("\n".rep(postStatSpacing-1))
    
    val blanks = theMap.keySet filter { _.isInstanceOf[BlankNode] }
    val nonHeads = statList filter { stat => stat.pred == RDF.rest && stat.obj.isInstanceOf[Resource] } map
      { stat => stat.obj.asInstanceOf[Resource] }
    val blankHeads = blanks -- nonHeads
    
    for(stat <- (statList.filter { stat => stat.subj.isInstanceOf[NamedNode] })) {
      writeNode(stat.subj, theMap, dupes, out);
    }
    for(head <- blankHeads) {
      writeNode(head, theMap, dupes, out);
    }
  }
  
  private def writeNode(res : Resource, theMap : HashMap[Resource, HashMap[NamedNode, HashSet[Value]]],
  dupes : HashSet[BlankNode], 
  out : Appendable) {
    
      if(theMap.contains(res)) {
        val head = theMap(res)
        theMap.remove(res)
        
        out.append(stringRes(res) + " ")
        
        formatPO(theMap, head, dupes, out, 1)
        
        out.append(" ." + "\n".rep(postStatSpacing))
      }
  }
  
  private def stringRes(res : Resource) : String = {
    res match {
      case qn : QName => if(!qn.suffix.matches(TurtleParser.nameStartChar + TurtleParser.nameChars + "*")) {
        return "<" + qn.uri.toString + ">"
      } else {
        return qn.toString()
      }
      case x => x.toString()
    }
  }
  
  private def buildMap(statList : StatementSet[Statement]) = {
    val theMap = new HashMap[Resource, HashMap[NamedNode, HashSet[Value]]]();
    val nameSpaces = new HashSet[NameSpace]()
    val mentioned = new HashSet[BlankNode]()
    val dupes = new HashSet[BlankNode]()
    
    for(stat <- statList) {
      if(theMap.contains(stat.subj)) {
        if(theMap(stat.subj).contains(stat.pred)) {
          theMap(stat.subj)(stat.pred) += (stat.obj)
        } else {
          theMap(stat.subj).put(stat.pred, (HashSet[Value]() + stat.obj))
        }
      } else {
        theMap.put(stat.subj, new HashMap())
        theMap(stat.subj) += (stat.pred -> (HashSet[Value]() + stat.obj))
      }
      
      stat.subj match {
        case qn : QName => nameSpaces += qn.nameSpace
        case _ =>
      }
      
      stat.pred match {
        case qn : QName => nameSpaces += qn.nameSpace
        case _ =>
      }
      
      stat.obj match {
        case qn : QName => nameSpaces += qn.nameSpace
        case bn : BlankNode => if(mentioned.contains(bn)) {
          dupes += (bn)
        } else {
          mentioned += (bn)
        }
        case _ =>
      }
    }
    (theMap,nameSpaces,dupes)
  }
  
  private def formatPO(theMap : HashMap[Resource, HashMap[NamedNode, HashSet[Value]]], 
  head : HashMap[NamedNode, HashSet[Value]], 
  dupes : HashSet[BlankNode],
  out : Appendable, 
  tabDepth : Int) {
    val iter = head.iterator
    var firstPO = true
    while(iter.hasNext) {
      val (pred,objs) = iter.next
      if(firstPO) {
        out.append(stringRes(pred) + " ")
        firstPO = false
      } else {
        out.append("\n" + " ".rep(tabWidth * tabDepth) + stringRes(pred) + " ")
      }
      var i = 0
      val iter2 = objs.iterator
      while(iter2.hasNext) {
        val obj = iter2.next
        if(i != 0 && i % maxObjs == 0) {
          out.append("\n" + " ".rep(tabWidth * (tabDepth + 1)))
        }
        if(formatObj(obj,theMap,dupes,out,tabDepth)) {
          i = maxObjs
        } else {
          i += 1
        }
        
        if(iter2.hasNext) {
          out.append(" ,")
        }
      }
      if(iter.hasNext) {
        out.append(" ;")
      }
    }
  }
  
  private def formatObj(obj : Value, theMap : HashMap[Resource, HashMap[NamedNode, HashSet[Value]]],  
  dupes : HashSet[BlankNode],
  out : Appendable, 
  tabDepth : Int) : Boolean =
    obj match {
      case x : NamedNode => { out.append(stringRes(x)); false }
      case x : BlankNode => { 
        if(!theMap.contains(x) || theMap(x).size == 0 || dupes.contains(x)) {
          out.append(stringRes(x));
          false
        } else if(theMap(x).size == 2 &&
        theMap(x).contains(RDF.first) &&
        theMap(x).contains(RDF.rest)) {
          out.append("( ")
          var node : Resource = x
          do {
            if(!theMap.contains(node)) {
              println(theMap)
              node == RDF.nil
            } else {
            if(!theMap(node).contains(RDF.first) ||
               !theMap(node).contains(RDF.rest)) {
              throw new IllegalArgumentException("Invalid list at " + node)
            }
            formatObj(theMap(node)(RDF.first).head,theMap,dupes,out,tabDepth+1)
            out.append("\n"+ " ".rep(tabWidth * (tabDepth + 1)))
            val old = node
            node = theMap(node)(RDF.rest).head.asInstanceOf[Resource]
            theMap.remove(old)
            }
          } while(node != RDF.nil)
          out.append(")")
          true
        } else {
          out.append("[ ")
          val newHead = theMap(x)
          theMap.remove(x)
          formatPO(theMap, newHead, dupes, out,tabDepth+1)
          out.append(" ]")
          true
        }
      }
      case x : Literal => { out.append(x.toString); false }
    }
}

import scala.collection.mutable.ListBuffer
import scala.util.parsing.combinator._
import scala.util.parsing.input.Reader
import java.lang.Math._
import java.net.URI
import java.util.Random

/**
 * A Turtle parser. Reads a turtle file into a list of statements
 */
object TurtleParser {
  
    private[TurtleParser] val rand = new Random()
    
    /*private[TurtleParser] sealed abstract class Directive
    
    private[TurtleParser] case class BaseDirective(val base:String) extends Directive
    private[TurtleParser] case class PrefixDirective(val prefix:Option[NameSpace],val uri:String) extends Directive*/
    
    private[TurtleParser] final case class Collection(elems:List[Any]) 
    
    /** Parse a Turtle document.
     * @param doc The Turtle document as a string
     * @return The list of statements as in the Turtle document
     */
    def parse(doc:String) = {
      val parser = new Parser
      parser.parseAll(parser.turtleDoc, doc) match {
        case parser.Success(p : List[_], _) => new StdStatementSet(deparse(p).toSet)
        case parser.Failure(msg, in) => throw new RDFTurtleParseException(msg + " @ " + getNextN(20,in))
        case _ => throw new RDFTurtleParseException("Unexpected parse result")
      }
    }    
    
    /** Parse a Turtle document.
     * @param doc The Turtle document as a stream
     * @return The list of statements as in the Turtle document
     */
    def parse(in: java.io.Reader) = {
      val parser = new Parser
      parser.parseAll(parser.turtleDoc, in) match {
        case parser.Success(p : List[_], _) => new StdStatementSet(deparse(p).toSet)
        case parser.Failure(msg, in) => throw new RDFTurtleParseException(msg + " @ " + getNextN(20,in))
        case _ => throw new RDFTurtleParseException("Unexpected parse result")
      }
    }
    
    private def getNextN(n:Int,in:Reader[Char]) : String = {
      if(n <= 0 || in.atEnd) { "" }
      else { in.first + getNextN(n-1,in.rest) }
    }
    
    private def deparse(ps:List[Any]) : List[Statement] = {
      ps.flatMap(res => res match {
        case x : Statement => List(x)
        case x : List[_] => deparse(x)
        case _ => Nil
      })
    }
           
    private[io] val pnChars =  	"""[A-Za-z%0-9\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02FF\u0370-\u037D\u037F-\u1FFF\u200C-\u200D\u2070-\u218F\u2C00-\u2FEF\u3001-\uD7FF\uF900-\uFDCF\uFDF0-\uFFFD]"""
  
    private[io] val nameChars =
  "[A-Za-z_%0-9\u00B7\u0300-\u036F\u203F-\u2040\u00B7\u0300-\u036F\u203F-\u2040\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02FF\u0370-\u037D\u037F-\u1FFF\u200C-\u200D\u2070-\u218F\u2C00-\u2FEF\u3001-\uD7FF\uF900-\uFDCF\uFDF0-\uFFFD\\-]"
      
    private[io] val nameStartChar = "[A-Z_a-z\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02FF\u0370-\u037D\u037F-\u1FFF\u200C-\u200D\u2070-\u218F\u2C00-\u2FEF\u3001-\uD7FF\uF900-\uFDCF\uFDF0-\uFFFD]"
  
    private[io] val nameStartChar2 =
    "[A-Za-z\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02FF\u0370-\u037D\u037F-\u1FFF\u200C-\u200D\u2070-\u218F\u2C00-\u2FEF\u3001-\uD7FF\uF900-\uFDCF\uFDF0-\uFFFD]"
      
    /**
     * The parser as a Scala combinator parser
     */
    private class Parser extends JavaTokenParsers {
           
      var namespaces = Map[String,NameSpace]()
      
      def turtleDoc = statement*
      
      def statement = directive <~ "." | triples <~ "." | comment
      
      def directive = prefixID | base
      
      def prefixID = "@prefix" ~> (prefixName2 ?) ~ (":" ~> uriref2) ^^ { case x ~ y => 
        namespaces += (x.getOrElse("") -> NameSpace(x.getOrElse(""),y)) 
      }
      
      def base = "@base" ~> uriref2 ^^ { case x =>
        namespaces += ("" -> NameSpace("",x))
      }
      
      def triples = subject ~ predicateObjectList ^^ { case (node,stats) ~ y => makeTriple(node,y) ::: stats }
      
      def predicateObjectList : Parser[List[~[NamedNode,List[Tuple2[Value,List[Statement]]]]]] =
        repsep(verb ~ objectList , ";" )
      
      def objectList : Parser[List[Tuple2[Value,List[Statement]]]] = repsep( objct, "," )
      
      def verb : Parser[NamedNode] = literal("a") ^^ { case _ => RDF._type } | predicate
      
      def comment = """#([^\n])*"""r
      
      def subject : Parser[Tuple2[Resource, List[Statement]]] = resource ^^ { case x => (x,Nil) } | blank 
      
      def predicate = resource
      
      def objct : Parser[Tuple2[Value,List[Statement]]] = resource ^^ { case x => (x,Nil) } | blank | 
        lit ^^ { case x => (x,Nil) }
      
      def lit = datatypeString ^^ { case x ~ y => new TypedLiteral(x.toString, y) } |  
        quotedString ~ (language ?) ^^ {
          case x ~ None => new SimpleLiteral(x.toString)
          case x ~ Some(y) => new LangLiteral(x.toString,y.toString) } | 
        integer ^^ { case x => new TypedLiteral(x.toString, XSD.integer) } | 
        duble ^^ { case x => new TypedLiteral(x.toString, XSD._double) } | 
        bool ^^ { case x => new TypedLiteral(x.toString,XSD._boolean) }
      
      def datatypeString = (quotedString <~ "^^") ~ resource
      
      def integer = """[+-]?[0-9]+"""r
      
      def duble = """[+-]?(([0-9]+(\.[0-9]*))|(\.[0-9]+))([Ee][+-]?[0-9]+)?"""r
      
      def bool = "true" | "false"
      
      def blank : Parser[Tuple2[Resource, List[Statement]]] = 
        blankNodeID | blankNodeEmpty | blankNodePreds | blankNodeCollection
        
      
      def blankNodeID : Parser[Tuple2[Resource, List[Statement]]]= 
        "_:" ~> nodeID ^^ { case nodeID => (nodeID,Nil) } 
      
      def blankNodeEmpty : Parser[Tuple2[Resource, List[Statement]]] = 
        regex("""\[\w*\]"""r) ^^ { case _ => (new BlankNode("id" + abs(rand.nextLong)),Nil) } 
      
      
      def blankNodePreds : Parser[Tuple2[Resource, List[Statement]]] = 
      "[" ~> predicateObjectList <~ "]" ^^ { case x => {
          val bn = new BlankNode("id" + abs(rand.nextLong))
          (bn,makeTriple(bn,x)) 
        }
      }
          
      def blankNodeCollection : Parser[Tuple2[Resource, List[Statement]]] = 
      "(" ~> collection ^^ { x =>
         val bn = AnonymousNode
         (bn,makeCollection(bn,x)) 
      }
      
      def itemList : Parser[List[Tuple2[Value,List[Statement]]]] = objct*
      
      def collection : Parser[List[Tuple2[Value,List[Statement]]]] = (itemList) <~ ")"
      
      def resource : Parser[NamedNode] = uriref | qname
      
      def nodeID = name ^^ { case x:String => new BlankNode(x) }
      
      def language = ("""@[a-z][a-z0-9\-A-Z]*"""r) ^^ { case x => x.substring(1) }
      
      def qname = (((prefixName ?) <~ ":") ~ name) ^^ { 
        case Some(x) ~ y  => x & y
        case None ~ y => namespaces.getOrElse("",RDF.base) & y
        }  
     
      def uriref = ("<" ~> relativeURI <~ ">") ^^ { case x:String => x.uri }
      
      def uriref2 = ("<" ~> relativeURI <~ ">")
      
      def name = (nameStartChar + nameChars + "*")r//"""[A-Za-z_][^ <>]*"""r
      
      def prefixName2 = regex((nameStartChar2 + pnChars + "*")r)//"""[A-Za-z][^ <>:]*"""r)
       
      def prefixName = regex((nameStartChar2 + pnChars + "*")r) ^^ { case x => if(namespaces.contains(x)) {
        namespaces(x) 
      } else { 
        throw new NameSpaceException(x) 
      } }
      //"""[A-Za-z][^ <>:]*"""r) ^^ { case x => namespaces(x) }
      
      def relativeURI = """[^ <>{}\|^`\\]+"""r
      
      def quotedString = longString | string
      
      //def string = "\"(([^\"])|(\\\"))+\""r
      def string = regex(("\"" + """(([^"])|(\"))*?""" + "\"")r) ^^ { case x => x.substring(1,x.length-1) }
      
      def longString = regex(("\"\"\"" + """(([^"])|(\"))*?""" + "\"\"\"")r) ^^ { case x => x.substring(3,x.length - 3) }
      
      private def makeTriple(subject:Resource, 
      predObjs : List[~[NamedNode,List[Tuple2[Value,List[Statement]]]]]) : List[Statement] = {
        predObjs.flatMap{
          case pred ~ objs => objs.flatMap { case (obj,stats) => Statement(subject,pred,obj) :: stats }
        }
      }
      
      private def makeCollection(node:Resource, elems:List[Tuple2[Value,List[Statement]]]) 
      : List[Statement] = {
        elems match {
          case (obj,stats) :: Nil => {
            (node %> RDF.rest %> RDF.nil) :: (node %> RDF.first %> obj) :: stats
          }
          case (obj, stats) :: tail => {
            val next = AnonymousNode()
            (node %> RDF.rest %> next) :: (node %> RDF.first %> obj) :: 
            (makeCollection(next,tail) ::: stats)
          }
          case Nil => List(node %> RDF.rest %> RDF.nil)
        }
      }
      
      
    }
    class NameSpaceException(str : String) extends RDFParseException(str)
}

class RDFTurtleParseException(message : String) extends RDFParseException(message)
