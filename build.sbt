name := "decaf-in-scala"

version := "0.1"

scalaVersion := "2.13.0"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"

// https://mvnrepository.com/artifact/org.ow2.asm/asm
libraryDependencies += "org.ow2.asm" % "asm" % "7.2-beta"

libraryDependencies += "com.github.scopt" %% "scopt" % "4.0.0-RC2"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3"

libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2"

// antlr4

enablePlugins(Antlr4Plugin)

antlr4PackageName in Antlr4 := Some("decaf.parsing.antlr")

antlr4GenListener in Antlr4 := false // default: true

antlr4GenVisitor in Antlr4 := true // default: false

// assembly

assemblyOutputPath in assembly := file("target/decaf.jar")
