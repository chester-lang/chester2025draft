package chester.cli.platform

import typings.node.processMod
import scala.scalajs.js

inline def argsPlatform(args: Array[String]): Array[String] = {
  val argv = processMod.^.argv.toArray
  argv.slice(2, argv.length)
}

def testFunctionalities(): Unit = {
  println("functionalities test start")
  chester.scala.Test.callit()
  println("functionalities test end")
}