package org.mandubian

import _root_.dispatch.json.{Js, JsValue}
import _root_.dispatch.{Request, HandlerVerbs}

package object rescator extends RescatorImplicits with Js{	
	def GET(req:Request):Request = req
	def POST(req:Request):Request = req.POST
	
	def JS(jsString:String) = Js(jsString)
	
	val R = ROOT
	val HEAD = ROOT
}

package rescator {
	trait RescatorImplicits {		
		import _root_.dispatch.json.Js._
		import dispatch._
		
		implicit def symToJsonXPath(sym:Symbol) = \(ROOT, sym)
		implicit def stringToJsValue(str:String) = Js(str)
		
		implicit def jsValueToRescatorJsonHandler(jsValue: JsValue) = new RescatorJsonHandler(jsValue)			
		
		implicit def handlertoRescatorHandler(subject: HandlerVerbs) = new RescatorHttpHandler(subject)
		implicit def requesttoRescatorHandler(request:Request) = new RescatorHttpHandler(request)
		implicit def stringToRescatorHandler(str: String) = new RescatorHttpHandler(new Request(str))
	}
}

