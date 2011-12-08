import dispatch.Request
import dispatch.json
import dispatch.HandlerVerbs
import dispatch.json._

package org.mandubian.rescator {

object int extends Extract[Int] {
    def unapply(js: JsValue) = js match {
      case JsNumber(v:BigDecimal) => Some(v.intValue())
      case _ => None
    }
}

object float extends Extract[Float] {
    def unapply(js: JsValue) = js match {
      case JsNumber(v:BigDecimal) => Some(v.floatValue())
      case _ => None
    }
}

object double extends Extract[Double] {
    def unapply(js: JsValue) = js match {
      case JsNumber(v:BigDecimal) => Some(v.doubleValue())
      case _ => None
    }
}

object long extends Extract[Long] {
    def unapply(js: JsValue) = js match {
      case JsNumber(v:BigDecimal) => Some(v.longValue())
      case _ => None
    }
}

object short extends Extract[Short] {
    def unapply(js: JsValue) = js match {
      case JsNumber(v:BigDecimal) => Some(v.shortValue())
      case _ => None
    }
}

/* 
 * REDEFINES CASE CLASS CHILD because of MatchError generated 
 * if the JsonXPath is not found or the type doesn't match
 * 
 * Extractor that resolves first by its parent extractor, if present.
 */
case class Child[T, E <: Insert[T]](parent: Option[Obj], self: E) extends Extract[T] with Insert[T] {
  def unapply(js: JsValue) = parent map { parent =>  js match {
      case parent(self(t)) => Some(t)
      case _ => None	// added this to prevent MatchError
    } } getOrElse { js match {
      case self(t) => Some(t)
      case _ => None
    }
  }
  /** Inserts the value t in self and replaces self in parent, if any. */
  def << (t: T)(js: JsValue) = parent map { parent => js match {
      case parent(my_js) => (parent << (self << t)(my_js))(js)
    } } getOrElse (self << t)(js)
}

/*
 * REDEFINES CLASS OBJ because Child has been redefined
 * 
 * Obj extractor, respects current parent context and sets a new context to itself.
 */
class Obj(sym: Symbol)(implicit parent: Option[Obj]) 
    extends Child[JsObject, Property[JsObject]](parent, Property(sym, Js.obj)) {
  implicit val ctx = Some(this)
}	

}
