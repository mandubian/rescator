package org.mandubian.rescator

import _root_.dispatch.Request
import _root_.dispatch.json
import _root_.dispatch.HandlerVerbs
import _root_.dispatch.json._

class RescatorJsonHandler(jsValue: JsValue) extends RescatorHandler {
	def >>> [OS <: java.io.OutputStream](out: OS) = {}
    
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
