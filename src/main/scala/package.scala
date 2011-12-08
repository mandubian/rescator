package org.mandubian

import _root_.dispatch.json.Js
import _root_.dispatch.Request

package object rescator extends RescatorImplicits with Js{	
	def GET(req:Request):Request = req
	def POST(req:Request):Request = req.POST
	
	def JS(jsString:String) = Js(jsString)
	
	val R = ROOT
	val HEAD = ROOT
}

