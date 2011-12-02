package org.mandubian

import dispatch.Request
import dispatch.json
import dispatch.HandlerVerbs
import dispatch.json._
	
package object rescator extends ImplicitHandlerVerbs with Js{
	import dispatch.json.Js._

	def GET(req:Request):Request = req
	def POST(req:Request):Request = req.POST
	
}

package rescator {

	trait ImplicitHandlerVerbs {		
		import dispatch.json.Js._
		
		implicit def handlertoRescatorHandler(subject: HandlerVerbs) = new RescatorHandler(subject)
		implicit def requesttoRescatorHandler(request:Request) = new RescatorHandler(request)
		implicit def stringToRescatorHandler(str: String) = new RescatorHandler(new Request(str))
		
		//implicit def symToRescatorJsonSymOp(sym:Symbol) = RescatorChild(sym, None)
		implicit def symToJsonXPath(sym:Symbol) = JsonXPath(None, sym)
	}

	// JsonXPath('child, 'parent)
	// JsonXPath(JsonXPath('parent), 'child)
	// JsonXPath(JsonXPath(JsonXPath('parent), 'child), 'child2)
	
	// JsonXPath('parent, JsonXPath('child1, JsonXPath('child11)))
	// { "parent" : { "child1" : { "child11" : "blabla" } } } 
	// /:parent/child1/child11
	// //:parent/child1/child11
	case class JsonXPath(parent:Option[JsonXPath] = None, sym:Symbol) {
		def / (childSym: Symbol)  = JsonXPath(Some(this), childSym)
		def as[T](ext:Extract[T]):Child[T, Property[T]] = {
		  val parentObj:Option[Obj] = parent match {
		    case Some(p) => Some(p as obj)
		    case None => None
		  }
		  new Child[T, Property[T]](parent..getOrElse(None) as obj, Property(sym, ext))
		}
	}
	
	trait RestHandler {
		def >>>[T](paths:JsonXPath*)
	}
	
	case class RescatorHandler(subject:HandlerVerbs) {
		import dispatch.json.JsHttp._
		import dispatch.Http
		def >>> [T](block: json.Js.JsF[T]) = Http(subject ># block)
	}
	
	case class RescatorChild(sym:Symbol, parent:Option[Obj]) {
	  import dispatch.json.Property
	  
	  def as[T](ext:Extract[T]) = {
	    new Child[T, Property[T]](parent, Property(sym, ext))
	  }
	  
	  def / [T](childsym: Symbol) = RescatorChild(childsym, Some(new Obj(sym)))
	}
	// (alpha, delta) = GET("http://...") >>> ( 'alpha, 'beta/'delta )
}