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
		
		implicit def symToRescatorJsonSymOp(sym:Symbol) = RescatorChild(sym, None)
	}

	case class JsonXPath(sym:Symbol, parent:Option[Obj] = None) {
	
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