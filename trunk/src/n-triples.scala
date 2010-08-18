package scala.rdf

import scala.util.parsing.combinator._
import scala.util.parsing.input.Reader
import java.net.URI

object NTriples {
  
  def convert(stats : List[Statement]) = {
    val buffer = new StringBuffer
    for(stat <- stats) { 
      stat match {
        case Statement(subj,pred,obj) =>
          buffer.append(format(subj))
          buffer.append(" ")
          buffer.append(format(pred))
          buffer.append(" ")
          buffer.append(format(obj))
          buffer.append(" .\n")
      }
    }
    buffer.toString
  }
  
  private def format(value : Value) = value match {
    case BlankNode(id) => "_:"+id
    case nn : NamedNode => "<"+nn.uri.toString+">"
    case lit : Literal => lit.toString
  }
  
  /** Parse an N-Triples document.
     * @param doc The N-Triples document as a string
     * @return The list of statements as in the Turtle document
     */
    def parse(doc:String) = {
      val parser = new Parser
      parser.parseAll(parser.doc, doc) match {
        case parser.Success(p : List[Statement], _) => p
        case parser.Failure(msg, in) => throw new RuntimeException(msg + " @ " + getNextN(20,in))
        case _ => throw new RuntimeException("Unexpected parse result")
      }
    }    
    
    /** Parse an N-Triples document.
     * @param doc The N-Triples document as a stream
     * @return The list of statements as in the N-Triples document
     */
    def parse(in: java.io.Reader) = {
      val parser = new Parser
      parser.parseAll(parser.doc, in) match {
        case parser.Success(p : List[Statement], _) => p
        case parser.Failure(msg, in) => throw new RuntimeException(msg + " @ " + getNextN(20,in))
        case _ => throw new RuntimeException("Unexpected parse result")
      }
    }
    private def getNextN(n:Int,in:Reader[Char]) : String = {
      if(n <= 0 || in.atEnd) { "" }
      else { in.first + getNextN(n-1,in.rest) }
    }
    
    class Parser extends JavaTokenParsers {
      
      def doc = statement *
      
      def statement = subject ~ predicate ~ obj ^^ { case x ~ y ~ z => Statement(x,y,z) }
      
      def subject = uriref | blankNode
      
      def predicate = uriref
      
      def obj = uriref | blankNode | _literal
      
      def uriref = "<" ~> regex("^[<>]+"r) <~ ">" ^^ { case x => URIRef(URI.create(x)) }
      
      def blankNode = "_:" ~> regex("^[ .]+"r) ^^{  case x => BlankNode(x) }
      
      def _literal = plain | langed | typed
      
      def plain = string ^^ { case x => SimpleLiteral(x) }
      
      def langed = (string <~ "@") ~ lang ^^ { case x ~ y => LangLiteral(x,y) }
      
      def typed = (string <~ "^^") ~ uriref ^^ { case x ~ y => TypedLiteral(x,y) }
      
      def lang = "[^ ]+"r
      
      def string = regex("\"[^\"]+\""r ) ^^ { case x => x.substring(1,x.length-1) }
      
    }
}
