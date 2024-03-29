package org.mandubian.rescator

import _root_.dispatch.Request
import _root_.dispatch.json
import _root_.dispatch.HandlerVerbs
import _root_.dispatch.json._
	

object Flag extends Enumeration {
  val RECURSIVE = Value
}

// { "parent" : { "child1" : { "child11" : "blabla" } } } 
// ROOT\parent\child1\child11
// ROOT\parent\child1\child11
case class JsonPath(parent:Option[JsonPath] = Some(ROOT), sym:Symbol)
 {
	// Dispatch overriden to return an Option[T] instead of T
	type JsF[T] = JsValue => Option[T]		
	
	// Dispatch overriden to return a local JsF returning an Option[T]
	/** Converts a Json extractor to an assertion extracting function (JsF). */
	private[rescator] def ext2fun[T](ext: Extract[T]): JsF[T] = jsv => {
		ext.unapply(jsv)
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
		    case Some(p) => new org.mandubian.rescator.Child[T, Property[T]](
	    			  			p.asObj, 
	    			  			Property(sym, ext)
	    	  				)
		    case None => Property(sym, ext)
		  }
	  )
	}
	
	def asObj:Option[org.mandubian.rescator.Obj] = {
	  val parentObj:Option[org.mandubian.rescator.Obj] = parent match {
	    case Some(ROOT) => None
	    case Some(p) => p asObj
	    case None => None
	  }
	  Some(new org.mandubian.rescator.Obj(sym)(parentObj))
	}
}

case class \(par:JsonPath, override val sym:Symbol) 
	extends JsonPath(Some(par), sym)

case object ROOT extends JsonPath(None, 'ROOT) 

class \\(par:JsonPath, override val sym:Symbol)
	extends JsonPath(Some(par), sym) {
  options += Flag.RECURSIVE
}
object \\ {
  def unapply(jxp:JsonPath):Option[(JsonPath, Symbol)] = {
    jxp match {
      case e @ \(par, sym) if e.options.contains(Flag.RECURSIVE) => Some((par, sym))
      case _ => None
    }
  }
}

trait RescatorHandler {
	// a simple extractor wrapper function taking a JsValue and returning another type
	type JsF[T] = JsValue => T

	def >>>[OS <: java.io.OutputStream](out: OS)
	def >>>[T](block: JsF[T]):T
	def >>>[T1, T2](block1: JsF[T1], block2: JsF[T2]):(T1, T2)
	def >>>[T1, T2, T3](block1: JsF[T1], block2: JsF[T2], block3: JsF[T3]):(T1, T2, T3)
	def >>>[T1, T2, T3, T4](block1: JsF[T1], block2: JsF[T2], block3: JsF[T3], block4: JsF[T4]):(T1, T2, T3, T4)
	def >>>[T1, T2, T3, T4, T5](block1: JsF[T1], block2: JsF[T2], block3: JsF[T3], block4: JsF[T4], block5: JsF[T5]): (T1, T2, T3, T4, T5)
	def >>>[T1, T2, T3, T4, T5, T6](block1: JsF[T1], block2: JsF[T2], block3: JsF[T3], block4: JsF[T4], block5: JsF[T5], block6: JsF[T6]): (T1, T2, T3, T4, T5, T6)
}
