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
		
		implicit def symToJsonXPath(sym:Symbol) = JsonXPath(None, sym)
	}

	// JsonXPath('child, 'parent)
	// JsonXPath(JsonXPath('parent), 'child)
	// JsonXPath(JsonXPath(JsonXPath('parent), 'child), 'child2)
	
	// { "parent" : { "child1" : { "child11" : "blabla" } } } 
	// /:parent/child1/child11
	// //:parent/child1/child11
	case class JsonXPath(parent:Option[JsonXPath] = None, sym:Symbol) {
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
	
	class \ {
	  
	}
	
	trait RestHandler {
		def >>> [OS <: java.io.OutputStream](out: OS)
		def >>>[T](block: json.Js.JsF[T]):T
		def >>>[T1, T2](block1: json.Js.JsF[T1], block2: json.Js.JsF[T2]):(T1, T2)
		def >>>[T1, T2, T3](block1: json.Js.JsF[T1], block2: json.Js.JsF[T2], block3: json.Js.JsF[T3]):(T1, T2, T3)
		def >>>[T1, T2, T3, T4](block1: json.Js.JsF[T1], block2: json.Js.JsF[T2], block3: json.Js.JsF[T3], block4: json.Js.JsF[T4]):(T1, T2, T3, T4)
		def >>>[T1, T2, T3, T4, T5](block1: json.Js.JsF[T1], block2: json.Js.JsF[T2], block3: json.Js.JsF[T3], block4: json.Js.JsF[T4], block5: json.Js.JsF[T5]): (T1, T2, T3, T4, T5)
	}
	
	class RescatorHandler(subject:HandlerVerbs) extends RestHandler {
		import dispatch.json.JsHttp._
		import dispatch.Http
		def >>> [OS <: java.io.OutputStream](out: OS) = subject >>> out
		 
		private def callHttp[T](block: (java.io.InputStream, String) => T):T = Http(subject >> block)  
		  
		def >>>[T](block: json.Js.JsF[T]) = {
			callHttp { (stream, charset) =>
				val jsValue = json.Js(stream, charset)
				block(jsValue)
			}
		}
		
		def >>>[T1, T2](block1: json.Js.JsF[T1], block2: json.Js.JsF[T2]) = {
			callHttp { (stream, charset) =>
				val jsValue = json.Js(stream, charset)
				(block1(jsValue), block2(jsValue))
			}
		}

		def >>>[T1, T2, T3](block1: json.Js.JsF[T1], block2: json.Js.JsF[T2], block3: json.Js.JsF[T3]) = {
			callHttp { (stream, charset) =>
				val jsValue = json.Js(stream, charset)
				(block1(jsValue), block2(jsValue), block3(jsValue))
			}
		}

		def >>>[T1, T2, T3, T4](block1: json.Js.JsF[T1], block2: json.Js.JsF[T2], block3: json.Js.JsF[T3], block4: json.Js.JsF[T4]) = {
			callHttp { (stream, charset) =>
				val jsValue = json.Js(stream, charset)
				(block1(jsValue), block2(jsValue), block3(jsValue), block4(jsValue))
			}

		}
		
		def >>>[T1, T2, T3, T4, T5](block1: json.Js.JsF[T1], block2: json.Js.JsF[T2], block3: json.Js.JsF[T3], block4: json.Js.JsF[T4], block5: json.Js.JsF[T5]) = {
			callHttp { (stream, charset) =>
				val jsValue = json.Js(stream, charset)
				(block1(jsValue), block2(jsValue), block3(jsValue), block4(jsValue), block5(jsValue))
			}
		}

	}

}