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
	
	val R = ROOT
	val HEAD = ROOT
	
	// ROOT\'test'\'test2 
	// HEAD\'test'\'test2 
	// R\'test'\'test2
	// __\'test'\'test2
}

package rescator {

	trait ImplicitHandlerVerbs {		
		import dispatch.json.Js._
		
		implicit def symToJsonXPath(sym:Symbol) = \(ROOT, sym)
		implicit def stringToJsValue(str:String) = Js(str)
		
		implicit def jsValueToRescatorJsonHandler(jsValue: JsValue) = new RescatorJsonHandler(jsValue)			
		
		implicit def handlertoRescatorHandler(subject: HandlerVerbs) = new RescatorHttpHandler(subject)
		implicit def requesttoRescatorHandler(request:Request) = new RescatorHttpHandler(request)
		implicit def stringToRescatorHandler(str: String) = new RescatorHttpHandler(new Request(str))			
	}

	// JsonXPath('child, 'parent)
	// JsonXPath(JsonXPath('parent), 'child)
	// JsonXPath(JsonXPath(JsonXPath('parent), 'child), 'child2)
	
	object Flag extends Enumeration {
	  val RECURSIVE = Value
	}
	
	// { "parent" : { "child1" : { "child11" : "blabla" } } } 
	// ROOT\parent\child1\child11
	// ROOT\parent\child1\child11
	case class JsonXPath(parent:Option[JsonXPath] = Some(ROOT), sym:Symbol) {
		type JsF[T] = JsValue => Option[T]
		
		/** Converts a Json extractor to an assertion extracting function (JsF). */
		private[rescator] def ext2fun[T](ext: Extract[T]): JsF[T] = jsv => {
		  try {
			ext.unapply(jsv)
		  }catch {
		    case MatchError => None
		  }
		}
		
		private[rescator] var options = Set[Flag.Value]()
		
		def \ (childSym: Symbol)  = new \(this, childSym)
				
		def \\ (childSym: Symbol)  = {
		  val jxp = new \(this, childSym)
		  jxp.options += Flag.RECURSIVE
		  jxp
		}
		
		def parentOf(childSym: Symbol) = this \ childSym
				
		def as[T](ext:Extract[T]):JsF[T] = {		  
		  ext2fun(
		      parent match {
			    case Some(ROOT) => Property(sym, ext)
			    case Some(p) => new Child[T, Property[T]](
		    			  			p.asObj, 
		    			  			Property(sym, ext)
		    	  				)
			    case None => Property(sym, ext)
			  }
		  )
		}
		
		private[rescator] def asObj:Option[Obj] = {
		  val parentObj:Option[Obj] = parent match {
		    case Some(ROOT) => None
		    case Some(p) => p asObj
		    case None => None
		  }
		  Some(new Obj(sym)(parentObj))
		}
	}
	
	case class \(par:JsonXPath, override val sym:Symbol) 
		extends JsonXPath(Some(par), sym)
	
	case object ROOT extends JsonXPath(None, 'ROOT) 

	class \\(par:JsonXPath, override val sym:Symbol)
		extends JsonXPath(Some(par), sym) {
	  options += Flag.RECURSIVE
	}
	object \\ {
	  def unapply(jxp:JsonXPath):Option[(JsonXPath, Symbol)] = {
	    jxp match {
	      case e @ \(par, sym) if e.options.contains(Flag.RECURSIVE) => Some((par, sym))
	      case _ => None
	    }
	  }
	}
	
	trait JsonHandler {
		// a simple extractor wrapper function taking a JsValue and returning another type
		type JsF[T] = JsValue => T

		//def >>>[OS <: java.io.OutputStream](out: OS)
		def >>>[T](block: JsF[T]):T
		def >>>[T1, T2](block1: JsF[T1], block2: JsF[T2]):(T1, T2)
		def >>>[T1, T2, T3](block1: JsF[T1], block2: JsF[T2], block3: JsF[T3]):(T1, T2, T3)
		def >>>[T1, T2, T3, T4](block1: JsF[T1], block2: JsF[T2], block3: JsF[T3], block4: JsF[T4]):(T1, T2, T3, T4)
		def >>>[T1, T2, T3, T4, T5](block1: JsF[T1], block2: JsF[T2], block3: JsF[T3], block4: JsF[T4], block5: JsF[T5]): (T1, T2, T3, T4, T5)
	}
	
	trait HttpHandler {
		import dispatch.Http
		protected def callHttp[T](subject:HandlerVerbs)(block: (java.io.InputStream, String) => T):T = Http(subject >> block)  

	}
	
	class RescatorJsonHandler(jsValue: JsValue) extends JsonHandler {
		def >>>[T](block: JsF[T]) = {
			block(jsValue)
		}
		
		def >>>[T1, T2](block1: JsF[T1], block2: JsF[T2]) = {
			(block1(jsValue), block2(jsValue))
		}
		
		def >>>[T1, T2, T3](block1: JsF[T1], block2: JsF[T2], block3: JsF[T3]) = {
			(block1(jsValue), block2(jsValue), block3(jsValue))
		}

		def >>>[T1, T2, T3, T4](block1: JsF[T1], block2: JsF[T2], block3: JsF[T3], block4: JsF[T4]) = {
			(block1(jsValue), block2(jsValue), block3(jsValue), block4(jsValue))
		}

		def >>>[T1, T2, T3, T4, T5](block1: JsF[T1], block2: JsF[T2], block3: JsF[T3], block4: JsF[T4], block5: JsF[T5]) = {
			(block1(jsValue), block2(jsValue), block3(jsValue), block4(jsValue), block5(jsValue))
		}
	
	}

	
	class RescatorHttpHandler(subject:HandlerVerbs) extends JsonHandler with HttpHandler {
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