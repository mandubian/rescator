name := "rescator"

version := "1.0"

scalaVersion := "2.9.1"

libraryDependencies ~= { seq =>
  val vers = "0.8.6"
  seq ++ Seq(
    "net.databinder" %% "dispatch-core" % vers,    
    "net.databinder" %% "dispatch-futures" % vers,    
    "net.databinder" %% "dispatch-nio" % vers,    
    "net.databinder" %% "dispatch-http" % vers,
    "net.databinder" %% "dispatch-mime" % vers,
    "net.databinder" %% "dispatch-oauth" % vers,
    "net.databinder" %% "dispatch-json" % vers,
    "net.databinder" %% "dispatch-http-json" % vers
  )
}
