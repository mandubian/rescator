package org.mandubian.rescator

import _root_.dispatch.Request
import _root_.dispatch.json
import _root_.dispatch.HandlerVerbs
import _root_.dispatch.json._


trait HttpHandler {
	import _root_.dispatch.Http
	protected def callHttp[T](subject:HandlerVerbs)(block: (java.io.InputStream, String) => T):T = Http(subject >> block)  
}

class RescatorHttpHandler(subject:HandlerVerbs) extends RescatorHandler with HttpHandler {
	import _root_.dispatch.json.JsHttp._
	
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


	