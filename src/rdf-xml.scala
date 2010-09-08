package scala.rdf.xml

import scala.rdf._
import scala.collection.mutable.{HashMap,HashSet,ListBuffer,LinkedList}
import scala.xml._
import java.net.URI

object RDFXML {
  import RDF._
  
  private val rdfuri = """http://www.w3.org/1999/02/22-rdf-syntax-ns#"""
  private val xsduri = """http://www.w3.org/2001/XMLScheme-datatypes"""
  
  def convert(xmlDoc : Node) : List[Statement] = {
    if(xmlDoc.prefix == "rdf" &&
       xmlDoc.label == "RDF") {
         val parser = new RDFXMLParser
         parser.buildNameSpaces(xmlDoc.scope)
         (xmlDoc.child flatMap (parser.nodeElement(_) match {
         case Some(x) => x.stats; case None => Nil})).toList
    } else {
      throw new RDFXMLFormatException("Document does not start in rdf:RDF")
    }
  }
  
  private case class ResStats(value:Value,stats:List[Statement])
  
  
  private[RDFXML] sealed abstract class ParseType
  
  private[RDFXML] object defaultParse extends ParseType
  private[RDFXML] object literalParse extends ParseType
  private[RDFXML] object resourceParse extends ParseType
  private[RDFXML] object collectionParse extends ParseType
  
  private class RDFXMLParser {
    
    var nameSpaces = new HashMap[String,NameSpace]()
    var nsByURI = new HashMap[String,NameSpace]()
    
    def nameSpace(key:String) : NameSpace =  if(key == null) {
        nameSpaces("")
      } else { 
        nameSpaces(key)
      }
    
    def buildNameSpaces(scope : NamespaceBinding) = {
      var curr = scope
      while(curr.uri != null) {
        if(curr.prefix == null) {
            val ns = NameSpace("",curr.uri)
            nameSpaces.put("",ns)
            nsByURI.put(curr.uri,ns)
        } else {
          val ns = NameSpace(curr.prefix,curr.uri)
          nameSpaces.put(curr.prefix,ns)
          nsByURI.put(curr.uri,ns)
        } 
        curr = curr.parent
      }
      if(!nameSpaces.contains("")) {
        nameSpaces.put("",RDF.base)
      }
    } 
    
    var bnIDs = 1
    
    def nodeElement(node : Node) : Option[ResStats] = {
      if(node.label == "#PCDATA") {
        None
      } else {
        var subject : Option[Resource] = None
        var predObjs = new HashSet[PredObj]()
        
        for(attr <- node.attributes) {
          attr match {
            case PrefixedAttribute("rdf","ID",value,_) => {
              subject = Some(resolve_hash(value.text))
            }
            case PrefixedAttribute("rdf","nodeID",value,_) => {
              subject = Some(bNode(value.text))
            }
            case PrefixedAttribute("rdf","about",value,_) => {
              subject = Some(resolve(value.text))
            }
            case PrefixedAttribute(pre,label,value,_) => {
              predObjs += PredObj(nameSpace(pre)&label, SimpleLiteral(value.text))
            }
            case UnprefixedAttribute(key,value,_) => {
              predObjs += PredObj(nameSpace("")&key, SimpleLiteral(value.text))
            }
          }
        }
                  
        if(subject == None) {
          subject = Some(genid())
          bnIDs += 1
        }
        Some(ResStats(subject.get,
          (node.child flatMap { childNode => propertyElt(childNode,subject.get) }).toList))
      }
    }
    
    def propertyElt(node : Node, subj : Resource) : List[Statement] = {
      if(node.label == "#PCDATA") {
        Nil
      } else {
        val pred = node.attribute(rdfuri,"ID") match {
          case Some(Seq(id,_*)) => resolve_hash(id.text)
          case _ => nameSpace(node.prefix)&node.label
        }
        
        (node \ ("@{"+rdfuri+"}resource")) match {
          case Seq(res,_*) => List(subj ~> pred ~> resolve(res.text))
          case Seq() => {         
            if(node.child.size == 1 && node.child(0).label == "#PCDATA") {
              val childNode = node.child(0)
              var lang : Option[String] = None
              var datatype : Option[NamedNode] = None
              
              for(attr <- node.attributes) {
                attr match {
                  case PrefixedAttribute("xml","lang",value,_) => {
                    lang = Some(value.text)
                  }
                  case PrefixedAttribute("rdf","datatype",value,_) => {
                    datatype = Some(resolve(value.text))
                  }
                  case x => throw new RDFXMLFormatException("Unrecognised attribute: " + x)
                }
              }
              
              if(lang != None && datatype != None) {
                throw new RDFXMLFormatException("String value cannot both have language tag and data type")
              } else if(lang != None) {
                List(subj ~> pred ~> LangLiteral(childNode.text,lang.get))
              } else if(datatype != None) {
                List(subj ~> pred ~> TypedLiteral(childNode.text,datatype.get))
              } else {
                List(subj ~> pred ~> SimpleLiteral(childNode.text))
              }
            } else {
              var parseType : ParseType = defaultParse
              
              for(attr <- node.attributes) { 
                attr match {
                  case PrefixedAttribute("rdf","parseType",x,_) => {
                    x(0).text match {
                      case "Literal" => parseType = literalParse
                      case "Resource" => parseType = resourceParse
                      case "Collection" => parseType = collectionParse
                      case parse => throw new RDFXMLFormatException("Unrecognised parse type:" + parse)
                    }
                  }
                  case _ => // TODO
                }
              }
              
              if(parseType == defaultParse) {
                var newStats = new ListBuffer[Statement]()
                  
                for(childNode <- node.child) {
                  nodeElement(childNode) match {
                    case Some(x) => { 
                      val ResStats(node, stats) = x
                      newStats ++= (subj ~> pred ~> node) :: stats
                    }
                    case None => 
                  }
                }
                  
                newStats.toList
              } else if(parseType == literalParse) { 
                List(subj ~> pred ~> SimpleLiteral(node.child.mkString(""))) 
              } else if(parseType == resourceParse) {
                val bn = genid()
                  
                (subj ~> pred ~> bn) :: (node.child flatMap ( propertyElt(_,bn) )).toList
              } else if(parseType == collectionParse) {
                val bn = genid()
                  
                (subj ~> pred ~> bn) :: collParse(bn,node.child)
              } else {
                throw new RuntimeException("Unreachable")
              }
            }
          }
        }
      }
    }
    
    def collParse(subj : BlankNode, children : Seq[Node]) : List[Statement] = {
      if(children.isEmpty) {
        Nil
      } else {
        val childNode = children.head
        
        if(childNode.label == "#PCDATA") {
          collParse(subj, children.tail)
        } else {
          nodeElement(childNode) match {
            case Some(x) => {
              val ResStats(obj,stats) = x 
              val next = genid()
              
              (subj ~> RDF.first ~> obj) :: 
              (subj ~> RDF.rest ~> next) :: 
              collParse(next,children.tail) :::
              stats
            }
            case None => Nil
          }
        }
      }
    }
    
    def resolve_hash(value : String) = {
      if(value.contains(":")) {
        URIRef(URI.create(value))
      } else {
        try {
          nameSpace("")&value
        } catch {
          case _ : RDFXMLFormatException => RDF.base&value
        }
      }
    }
    
    val qualRegex = """&([^;]);(.*)"""r
    def resolve(value : String) = {
      if(value.matches("""&([^;]);(.*)""")) {
        val qualRegex(ns,suf) = value
        nameSpace(ns)&suf
      } else if(value.matches("""#.*""")) {
        nameSpace("")&value.substring(1)
      } else {
        nsByURI.keys.find(x => value.startsWith(x)) match {
          case Some(key) => nsByURI(key)&(value.substring(key.length))
          case None => URIRef(URI.create(value))
        }
      }
    }
    
    val bNodeRegex = """genid([0-9])+"""r
    def bNode(value : String) = {
      BlankNode("genid-"+value)
    }
    
    def genid() = {
      val bn = BlankNode("genid"+bnIDs)
      bnIDs += 1
      bn
    }
  }
    
    /**
     * Format a list of statements in Turtle
     * @param statList The list of statements
     */
    def convert(statList : List[Statement]) : Node = {
      var (theMap,nameSpaces,dupes) = RDFXMLConverter.buildMap(statList) 
      
      nameSpaces += RDF
      
      val scope = RDFXMLConverter.buildScope(nameSpaces)
      
      val nodes = new ListBuffer[Node]()
      
      for(stat <- statList) {
        if(theMap.contains(stat.subj)) {
          nodes += RDFXMLConverter.node(stat.subj,theMap,scope,dupes)
        }
      }
      
      Elem("rdf","RDF",Null,scope,nodes:_*)
    }
    
    private[RDFXML] def getHeader(node : Node) : String = {
      val rv = new StringBuffer()
      rv.append("<?xml version=\"1.0\"?>\n\n<!DOCTYPE rdf:RDF [\n")
      var scope = node.scope
      while(scope != TopScope) {
        rv.append("\t<!ENTITY " + scope.prefix + " \"" + scope.uri + "\" >\n")  
        scope = scope.parent
      }
      rv.append("]>\n")
      rv.toString
    }
      
    object RDFXMLConverter { 
    
    def node(n : Resource, theMap : HashMap[Resource, HashMap[NamedNode, LinkedList[Value]]],
    scope : NamespaceBinding, dupes : HashSet[BlankNode]) : Node = {
      if(theMap.contains(n)) {
       val head = theMap(n)
       theMap.remove(n)
         
       val (prefix,label) = getPrefixLabel(n,head)
          
       Elem(prefix,label,nodeAttrs(n),scope,elems(theMap,head,dupes,scope):_*)
      } else {
        Elem("rdf","Description",nodeAttrs(n),scope)
      }        
    }
    
    def buildScope(nameSpaces : HashSet[NameSpace]) : NamespaceBinding = {
      if(nameSpaces.isEmpty) {
        TopScope
      } else {
        //println(nameSpaces.head.id + " : " + nameSpaces.head.prefix)
        NamespaceBinding(nameSpaces.head.id,nameSpaces.head.prefix,buildScope(nameSpaces.tail))
      }
    }
    
    def buildMap(statList : List[Statement]) = {
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
    
    val uriRegex = """.*[/:]([^/#]+)[/#]([^/#]*)"""r
    
    def getPrefixLabel(typ : Value) : Tuple2[String,String] = {
      typ match {
        case x : QName => (x.nameSpace.id,x.suffix)
        case x : URIRef => {
          val uriRegex(prefix,label) = x.uri.toString
          (prefix,label)
        }
        case _ => throw new RDFXMLFormatException("Could not deduce prefix, possibly blank node or literal as RDF type?")
      }
    }
    
    def getPrefixLabel(res : Resource, head : HashMap[NamedNode, LinkedList[Value]]) : Tuple2[String,String] = {
      res match {
        case _ : BlankNode => ("rdf","Description")
        case _ => {
          head.get(RDF._type) match {
            case Some(LinkedList(typ,_*)) => getPrefixLabel(typ.asInstanceOf[Value])
            case Some(LinkedList()) => ("rdf","Description")
            case None => ("rdf","Description")
          }
        }
      }
    }
    
    def nodeAttrs(res : Resource) = {
      res match {
        case qn : QName => {
          if(qn.nameSpace.id == "") {
            new PrefixedAttribute("rdf","about",qn.suffix,Null)
          } else {
            new PrefixedAttribute("rdf","about",Unparsed("&"+qn.nameSpace.id + ";"+qn.suffix),Null)
          }
        }
        case ref : URIRef => {
          new PrefixedAttribute("rdf","id",ref.uri.toString,Null)
        }
        case bn : BlankNode => {
          new PrefixedAttribute("rdf","nodeID",bn.id,Null)
        }
        case _ => throw new IllegalArgumentException("Invalid RDF resource")
      }
    }
          
    def elems(theMap : HashMap[Resource, HashMap[NamedNode, LinkedList[Value]]],
      head : HashMap[NamedNode, LinkedList[Value]], 
      dupes : HashSet[BlankNode],
      scope : NamespaceBinding) : Seq[Node] = {
          (head.keys flatMap { pred => {
            val parseType = deduceParseType(head(pred), theMap,dupes)
              val (prefix,label) = getPrefixLabel(pred)
            
            if(parseType == defaultParse) {
              
              head(pred) map { obj =>
                obj match {
                  case lit : SimpleLiteral => Elem(prefix,label,Null,scope,Unparsed(lit.value))
                  case lit : LangLiteral => 
                    Elem(prefix,label,new PrefixedAttribute("xml","lang",lit.lang,Null),scope,Unparsed(lit.value))
                  case lit : TypedLiteral =>
                    Elem(prefix,label,new PrefixedAttribute("rdf","datatype",toResourceName(lit.typ),Null),scope,Unparsed(lit.value))
                    case res : Resource => {
                      if(theMap.contains(res)) {
                        Elem(prefix,label,Null,scope,node(res,theMap,scope,dupes))
                      } else {
                        res match {
                          case nn : NamedNode => {
                            Elem(prefix,label,new PrefixedAttribute("rdf","resource",toResourceName(nn),Null),
                                 scope)
                          }
                          case bn : BlankNode => {
                            Elem(prefix,label,Null, scope, Elem(
                              "rdf","Description",new PrefixedAttribute("rdf","nodeID",bn.id,Null),scope))
                          }
                        }
                      }
                    }
                }
              }     
            } else if(parseType == resourceParse) {
              Elem(prefix,label,new PrefixedAttribute("rdf","parseType","Resource",Null),scope,
              (head(pred) flatMap { 
                obj => {
                  val head2 = theMap(obj.asInstanceOf[Resource])
                  theMap.remove(obj.asInstanceOf[Resource])
                  
                  elems(theMap,head2,dupes,scope)
                }
              }):_*)
            } else if(parseType == collectionParse) {
              head(pred) map { obj =>
                val head2 = theMap(obj.asInstanceOf[Resource])
                theMap.remove(obj.asInstanceOf[Resource])
                Elem(prefix,label,new PrefixedAttribute("rdf","parseType","Collection",Null),scope,collParse(theMap,head2,dupes,scope):_*)
              }
            } else {
              throw new IllegalArgumentException("unreachable")
            }
            }
        }).toSeq
    }
    
    def collParse(theMap : HashMap[Resource, HashMap[NamedNode, LinkedList[Value]]],
      head : HashMap[NamedNode, LinkedList[Value]], 
      dupes : HashSet[BlankNode],
      scope : NamespaceBinding) : Seq[Node] = {
        val next = head(RDF.rest)(0).asInstanceOf[Resource]
        val first = head(RDF.first)(0).asInstanceOf[Resource]
        if(next == RDF.nil) {
          node(first,theMap,scope,dupes)
        } else {
          val nextHead = theMap(next)
          theMap.remove(next)
          node(first,theMap,scope,dupes) ++ collParse(theMap,nextHead,dupes,scope)
        }
      }
    }

    
    private[RDFXML] def deduceParseType(objs : LinkedList[Value], 
    theMap : HashMap[Resource, HashMap[NamedNode, LinkedList[Value]]],
    dupes : HashSet[BlankNode]) : ParseType = {
      var collectionFound = false
      var bNodeFound = false
      var nonBNodeFound = false
      
      for(obj <- objs) {
        obj match {
          case x : BlankNode => {
            if(theMap.contains(x) && !dupes.contains(x)) {
              bNodeFound = true
              if(theMap(x).size == 2 &&
                theMap(x).contains(RDF.first) && theMap(x).contains(RDF.rest)) {
                  collectionFound = true
              }
            } else {
              nonBNodeFound = true
            }
          }
          case _ => nonBNodeFound = true
        }
      }
      if(collectionFound && !nonBNodeFound) {
        collectionParse
      } else if(bNodeFound && !nonBNodeFound) {
        resourceParse
      } else {
        defaultParse
      }
    }
              
    private[RDFXML] def toResourceName(nn : NamedNode) = {
      nn match {
        case QName(ns,suf) => Unparsed("&"+ns.id +";"+suf)
        case URIRef(uri) => Unparsed(uri.toString)
      }
    }
    
    /*
    def convertPO(theMap : HashMap[Resource, HashMap[NamedNode, LinkedList[Value]]], 
    head : HashMap[NamedNode, LinkedList[Value]], 
    dupes : HashSet[BlankNode]) {
      val iter = head.iterator
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
      
      head.key map { pred =>
        Elem(getPredPrefix(pred,head),getPredLabel(pred,head),getPred
    }
    
    def formatObj(obj : Value, theMap : HashMap[Resource, HashMap[NamedNode, LinkedList[Value]]],  
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
  }*/
    
}

class RDFXMLFormatException(message : String) extends RuntimeException(message)
