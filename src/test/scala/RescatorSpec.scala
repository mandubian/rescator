package org.mandubian

import org.specs2.mutable._

object RescatorSpec extends Specification {
  import org.mandubian.rescator._

  "Rescator Features" >> {
	  "JsonXPath" should {
		  "provide XPath selector \\ (single backslash)" >> {
			  	"creating JsonXPath from ROOT & 1 symbol" in {
					val jxp = ROOT\'sym1
					jxp must equalTo( new \(ROOT, 'sym1) )
				}
	
			  	"creating JsonXPath from ROOT & 2 symbols" in {
			  		val jxp = ROOT\'sym1\'sym2
			  		jxp must equalTo( new \( new \(ROOT, 'sym1), 'sym2) )
			  	}
	
			  	"creating JsonXPath from 2 symbols as if from ROOT" in {
			  		val jxp = 'sym1\'sym2
			  		jxp must equalTo( new \( new \(ROOT, 'sym1), 'sym2) )
			  		jxp must equalTo( ROOT\'sym1\'sym2 )
			  	}
	
			  	"creating JsonXPath from 3 symbols as if from ROOT" in {
			  		val jxp = 'sym1\'sym2\'sym3
			  		jxp must equalTo( new \(new \(new \(ROOT, 'sym1), 'sym2), 'sym3) )
			  	}
	
			  	"creating JsonXPath up to 22 symbols as if from ROOT" in {
			  		pending
			  	}
			  	
			  	"pattern matching from ROOT for 1 symbol" in {
			  		ROOT\'sym1 match { case ROOT\'sym1 => success }
			  	}
	
			  	"pattern matching from ROOT for 2 symbols" in {
			  		ROOT\'sym1\'sym2 match { case ROOT\'sym1\'sym2 => success }
			  	}
	
	  		  	"pattern matching from ROOT for 3 symbols" in {
			  		ROOT\'sym1\'sym2\'sym3 match { case ROOT\'sym1\'sym2\'sym3 => success }
			  	}
	
	  		  	"pattern matching from ROOT for 3 symbols" in {
			  		ROOT\'sym1\'sym2\'sym3 match { case ROOT\'sym1\'sym2\'sym3 => success }
			  	}
		  
	  		  	"pattern matching from ROOT up to 22 symbols" in {
	  		  		pending
			  	}
	
	  		  	"pattern matching 2 symbols as if from ROOT" in {
			  		'sym1\'sym2 match { case ROOT\'sym1\'sym2 => success }
			  	}
	
	  		  	"pattern matching up to 22 symbols as if from ROOT" in {
	  		  		pending
			  	}
		  }
	  }
  
	  "JSON Map to TupleN[Option[...]]" should {
		  "map JS expression to a tuple" in {
			  val js = JS(""" { 
						      "child1" : { "child11" : { "child112" : "blabla" } }, 
						      "child2" : { "child21": 12345, "child22": [ "alpha", "beta", "delta" ] } 
						  }""")
						  
			  val child1 =  JS("""{ "child11" : { "child112" : "blabla" } }""")
			  val child22 = List("alpha", "beta", "delta")
			  js >>> (	
				  'child1 as obj, 
				  'child1\'child11\'child112 as str,
				  'child2\'child21 as num,
				  'child2\'child22 as list
			  ) match {
			    case (Some(child1), Some("blabla"), Some(child21), Some(child22)) if(child21 == 12345) => success
			    case _ => failure
			  }
				  
		  }
	  }
  }
}