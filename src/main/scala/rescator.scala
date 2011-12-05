package org.mandubian

import dispatch.Request
import dispatch.json
import dispatch.HandlerVerbs
import dispatch.json._
	
package object rescator extends ImplicitHandlerVerbs with Js{
	import dispatch.json.Js._

	def GET(req:Request):Request = req
	def POST(req:Request):Request = req.POST
	
	def JS(jsString:String) = Js(jsString)
}

package rescator {

	trait ImplicitHandlerVerbs {		
		import dispatch.json.Js._
		
		implicit def symToJsonXPath(sym:Symbol) = JsonXPath(None, sym)
		implicit def stringToJsValue(str:String) = Js(str)

		implicit def handlertoRescatorHandler(subject: HandlerVerbs) = new RescatorHttpHandler(subject)
		implicit def requesttoRescatorHandler(request:Request) = new RescatorHttpHandler(request)
		implicit def stringToRescatorHandler(str: String) = new RescatorHttpHandler(new Request(str))			
	}

	// JsonXPath('child, 'parent)
	// JsonXPath(JsonXPath('parent), 'child)
	// JsonXPath(JsonXPath(JsonXPath('parent), 'child), 'child2)
	
	// { "parent" : { "child1" : { "child11" : "blabla" } } } 
	// /:parent/child1/child11
	// //:parent/child1/child11
	case class JsonXPath(parent:Option[JsonXPath] = None, sym:Symbol) {
		type JsF[T] = JsValue => T
		
		def \ (childSym: Symbol)  = JsonXPath(Some(this), childSym)
				
		def parentOf(childSym: Symbol) = this \ childSym
				
		def as[T](ext:Extract[T]):JsF[T] = {		  
		  Js.ext2fun(new Child[T, Property[T]](
		      parent.flatMap(_.asObj), 
		      Property(sym, ext)
		  ))
		}
		
		private def asObj:Option[Obj] = {
		  val parentObj:Option[Obj] = parent match {
		    case Some(p) => p asObj
		    case None => None
		  }
		  Some(new Obj(sym)(parentObj))
		}
	}
	
	trait HttpHandler {
		import dispatch.Http
		protected def callHttp[T](subject:HandlerVerbs)(block: (java.io.InputStream, String) => T):T = Http(subject >> block)  

		// a simple extractor wrapper function taking a JsValue and returning another type
		type JsF[T] = JsValue => T

		def >>>[OS <: java.io.OutputStream](out: OS)
		def >>>[T](block: JsF[T]):T
		def >>>[T1, T2](block1: JsF[T1], block2: JsF[T2]):(T1, T2)
		def >>>[T1, T2, T3](block1: JsF[T1], block2: JsF[T2], block3: JsF[T3]):(T1, T2, T3)
		def >>>[T1, T2, T3, T4](block1: JsF[T1], block2: JsF[T2], block3: JsF[T3], block4: JsF[T4]):(T1, T2, T3, T4)
		def >>>[T1, T2, T3, T4, T5](block1: JsF[T1], block2: JsF[T2], block3: JsF[T3], block4: JsF[T4], block5: JsF[T5]): (T1, T2, T3, T4, T5)
	}
	
	class RescatorHttpHandler(subject:HandlerVerbs) extends HttpHandler {
		import dispatch.json.JsHttp._
		def >>> [OS <: java.io.OutputStream](out: OS) = subject >>> out
		 
		def >>>[T](block: JsF[T]) = {
			callHttp(subject) { (stream, charset) =>
				val jsValue = json.Js(stream, charset)
				block(jsValue)
			}
		}
		
		def >>>[T1, T2](block1: JsF[T1], block2: JsF[T2]) = {
			callHttp(subject) { (stream, charset) =>
				val jsValue = json.Js(stream, charset)
				(block1(jsValue), block2(jsValue))
			}
		}

		def >>>[T1, T2, T3](block1: JsF[T1], block2: JsF[T2], block3: JsF[T3]) = {
			callHttp(subject) { (stream, charset) =>
				val jsValue = json.Js(stream, charset)
				(block1(jsValue), block2(jsValue), block3(jsValue))
			}
		}

		def >>>[T1, T2, T3, T4](block1: JsF[T1], block2: JsF[T2], block3: JsF[T3], block4: JsF[T4]) = {
			callHttp(subject) { (stream, charset) =>
				val jsValue = json.Js(stream, charset)
				(block1(jsValue), block2(jsValue), block3(jsValue), block4(jsValue))
			}
		}
		
		def >>>[T1, T2, T3, T4, T5](block1: JsF[T1], block2: JsF[T2], block3: JsF[T3], block4: JsF[T4], block5: JsF[T5]) = {
			callHttp(subject) { (stream, charset) =>
				val jsValue = json.Js(stream, charset)
				(block1(jsValue), block2(jsValue), block3(jsValue), block4(jsValue), block5(jsValue))
			}
		}

	}

}