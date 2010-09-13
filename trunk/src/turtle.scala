package scala.rdf.turtle

import scala.rdf._
import java.io._
import scala.collection.mutable.{HashMap,LinkedList,HashSet}

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
  def format(statList : List[Statement]) : String = {
    val writer = new StringWriter();
    format(statList,writer)
    writer.toString
  }
  
  /** Format a list of statements in Turtle
   * @param out The appendable buffer to add the result to
   */
  def format(statList : List[Statement], out : Appendable) : Unit = {
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
    
    for(stat <- statList) {
      if(theMap.contains(stat.subj)) {
        val head = theMap(stat.subj)
        theMap.remove(stat.subj)
        
        out.append(stat.subj.toString + " ")
        
        formatPO(theMap, head, dupes, out, 1)
        
        out.append(" ." + "\n".rep(postStatSpacing))
      }
    }
  }
  
  private def buildMap(statList : List[Statement]) = {
    val theMap = new HashMap[Resource, HashMap[NamedNode, LinkedList[Value]]]();
    val nameSpaces = new HashSet[NameSpace]()
    val mentioned = new HashSet[BlankNode]()
    val dupes = new HashSet[BlankNode]()
    
    for(stat <- statList) {
      if(theMap.contains(stat.subj)) {
        if(theMap(stat.subj).contains(stat.pred)) {
          theMap(stat.subj)(stat.pred) :+= (stat.obj)
        } else {
          theMap(stat.subj).put(stat.pred, new LinkedList(stat.obj, new LinkedList()))
        }
      } else {
        theMap.put(stat.subj, new HashMap())
        theMap(stat.subj) += (stat.pred -> new LinkedList(stat.obj, new LinkedList()))
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
  
  private def formatPO(theMap : HashMap[Resource, HashMap[NamedNode, LinkedList[Value]]], 
  head : HashMap[NamedNode, LinkedList[Value]], 
  dupes : HashSet[BlankNode],
  out : Appendable, 
  tabDepth : Int) {
    val iter = head.iterator
    var firstPO = true
    while(iter.hasNext) {
      val (pred,objs) = iter.next
      if(firstPO) {
        out.append(pred.toString + " ")
        firstPO = false
      } else {
        out.append("\n" + " ".rep(tabWidth * tabDepth) + pred.toString + " ")
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
        if(iter.hasNext) {
          out.append(" ;")
        }
      }
    }
  }
  
  private def formatObj(obj : Value, theMap : HashMap[Resource, HashMap[NamedNode, LinkedList[Value]]],  
  dupes : HashSet[BlankNode],
  out : Appendable, 
  tabDepth : Int) : Boolean =
    obj match {
      case x : NamedNode => { out.append(x.toString); false }
      case x : BlankNode => { 
        if(!theMap.contains(x) || theMap(x).size == 0 || dupes.contains(x)) {
          out.append(x.toString);
          false
        } else if(theMap(x).size == 2 &&
        theMap(x).contains(RDF.first) &&
        theMap(x).contains(RDF.rest)) {
          out.append("( ")
          var node : Resource = x
          do {
            formatObj(theMap(node)(RDF.first)(0),theMap,dupes,out,tabDepth+1)
            out.append("\n"+ " ".rep(tabWidth * (tabDepth + 1)))
            val old = node
            node = theMap(node)(RDF.rest)(0).asInstanceOf[Resource]
            theMap.remove(old)
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
    import scala.rdf.sugar._
  
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
        case parser.Success(p : List[_], _) => deparse(p)
        case parser.Failure(msg, in) => throw new RuntimeException(msg + " @ " + getNextN(20,in))
        case _ => throw new RuntimeException("Unexpected parse result")
      }
    }    
    
    /** Parse a Turtle document.
     * @param doc The Turtle document as a stream
     * @return The list of statements as in the Turtle document
     */
    def parse(in: java.io.Reader) = {
      val parser = new Parser
      parser.parseAll(parser.turtleDoc, in) match {
        case parser.Success(p : List[_], _) => deparse(p)
        case parser.Failure(msg, in) => throw new RuntimeException(msg + " @ " + getNextN(20,in))
        case _ => throw new RuntimeException("Unexpected parse result")
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
      /*deparse2(stats,dirs.asInstanceOf[List[Directive]])
    }
  
    private def deparse2(p:List[Any], dirs:List[Directive]) : List[Statement] = {
      p match {
        case head :: rest => (head match {
          case x:List[_] => deparse2(x,dirs)
          case x:Statement => List(x) //List(applyNameSpace(x,dirs))
          case _ => Nil
        }) ::: deparse2(rest, dirs)
        case Nil => Nil
      }
    }*/
    /*
    private def applyNameSpace(x:Statement, dirs:List[Directive]) : Statement = {
      x match {
        case Statement(subj, pred, obj) => Statement(applyNameSpace1(subj,dirs),
                                                     applyNameSpace2(pred,dirs),
                                                     applyNameSpace3(obj,dirs))
      }
    }
    
    private def applyNameSpace1(x:Resource, dirs:List[Directive]) : Resource = {
      x match {
        case y : QName => applyNS2(y, dirs)
        case y => y
      }
    }
    
    private def applyNameSpace2(x:NamedNode, dirs:List[Directive]) : NamedNode = {
      x match {
        case y : QName => applyNS2(y, dirs)
        case y => y
      }
    }
    
    private def applyNameSpace3(x:Value, dirs:List[Directive]) : Value = {
      x match {
        case y : QName => applyNS2(y, dirs)
        case y => y
      }
    }
    
    private def applyNS2(y:QName, dirs:List[Directive]) : QName = {
      dirs match {
        case head :: rest => head match {
          case x: BaseDirective => if(y.nameSpace.id == "") { 
            NameSpace("",x.base) & y.suffix 
          } else {
            applyNS2(y,rest)
          }
          case PrefixDirective(Some(x),prefix) => if(y.nameSpace.id == x) {
            NameSpace(x.id,prefix) & y.suffix
          } else {
            applyNS2(y,rest)
          }
          case PrefixDirective(None,prefix) => if(y.nameSpace.id == "") { 
            NameSpace("",prefix) & y.suffix 
          } else {
            applyNS2(y,rest)
          }
        }
        case Nil => y
      }
    }*/
    
    /**
     * The parser as a Scala combinator parser
     */
    class Parser extends JavaTokenParsers {
           
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
      
      def verb : Parser[NamedNode] = predicate | literal("a") ^^ { case _ => RDF._type }
      
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
         val bn = new AnonymousNode
         (bn,makeCollection(bn,x)) 
      }
      
      def itemList : Parser[List[Tuple2[Value,List[Statement]]]] = objct*
      
      def collection : Parser[List[Tuple2[Value,List[Statement]]]] = (itemList) <~ ")"
      
      def resource : Parser[NamedNode] = uriref | qname
      
      def nodeID = name ^^ { case x:String => new BlankNode(x) }
      
      def language = """@[a-z][a-z0-9]*"""r
      
      def qname = (((prefixName ?) <~ ":") ~ name) ^^ { 
        case Some(x) ~ y  => x & y
        case None ~ y => namespaces.getOrElse("",RDF.base) & y
        }  
      
      def uriref = ("<" ~> relativeURI <~ ">") ^^ { case x:String => x.uri }
      
      def uriref2 = ("<" ~> relativeURI <~ ">")
      
      def name = """[A-Za-z_][^ <>]*"""r
      
      def prefixName2 = regex("""[A-Za-z][^ <>:]*"""r)
      
      def prefixName = regex("""[A-Za-z][^ <>:]*"""r) ^^ { case x => namespaces(x) }
      
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
            (node ~> RDF.rest ~> RDF.nil) :: (node ~> RDF.first ~> obj) :: stats
          }
          case (obj, stats) :: tail => {
            val next = new AnonymousNode
            (node ~> RDF.rest ~> next) :: (node ~> RDF.first ~> obj) :: 
            (makeCollection(next,tail) ::: stats)
          }
          case Nil => List(node ~> RDF.rest ~> RDF.nil)
        }
      }
    }
}
/*
object TestParser {
  import java.io._ 
  
  def main(args:Array[String]) {
    val parser = new TurtleParser
    val in = new FileReader("ifrs-lemon-short.rdf")
    val model = parser.parse(in)
    println(model.toString)
  }
}*/
