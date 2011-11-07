package org.mandubian

import dispatch.Request
import dispatch.json
import dispatch.HandlerVerbs
import dispatch.json._
import dispatch.json.Js._
	
package object rescator {
	def GET(req:Request):Request = req
	def POST(req:Request):Request = req.POST
	
	implicit def handlertoJsonHandler(subject: HandlerVerbs) = new JsonHandler(subject)
}

package rescator {

	trait ImplicitHandlerVerbs {
	  implicit def toHandlerVerbs(req: Request) = new HandlerVerbs(req)
	  implicit def stringToHandlerVerbs(str: String) = new HandlerVerbs(new Request(str))
	}

	case class JsonHandler(subject:HandlerVerbs) {
		def >>> [T](block: json.Js.JsF[T]) = subject >> { (stm, charset) =>
			block(json.Js(stm, charset))
		}
	}
	
	
}