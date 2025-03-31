package chester.tsmorph

import scala.scalajs.js
import scala.scalajs.js.annotation._
import scala.util.Try

/**
 * Tests for interacting with ts-morph using ScalaJS
 */
object TsMorphTest {
  
  /**
   * Test basic ts-morph functionality
   */
  def testTsMorph(): Unit = {
    println("Testing ts-morph functionality...")
    
    try {
      // Try to import ts-morph from different possible locations
      val tsMorph = tryLoadTsMorph()
      
      if (tsMorph == null) {
        println("Failed to load ts-morph module. Make sure it's installed.")
        return
      }
      
      // Create a new project
      val project = js.Dynamic.newInstance(tsMorph.Project)()
      
      // Create a source file with some sample TypeScript code
      val sourceFile = project.createSourceFile(
        "test.ts",
        """
        export interface Person {
          id: number;
          name: string;
          email?: string;
        }
        
        export class PersonService {
          private people: Person[] = [];
          
          addPerson(person: Person): void {
            this.people.push(person);
          }
          
          getPerson(id: number): Person | undefined {
            return this.people.find(p => p.id === id);
          }
        }
        """
      )
      
      // Get interfaces from the source file
      // Convert the interfaces to a JavaScript array first
      println("Calling getInterfaces()...")
      val interfacesArray = sourceFile.getInterfaces().asInstanceOf[js.Dynamic]
      val interfaces = js.Array.from(interfacesArray.asInstanceOf[js.Iterable[js.Dynamic]]).toList
      
      println(s"Found ${interfaces.length} interfaces:")
      interfaces.foreach { iface =>
        val name = Try(iface.getName().toString).getOrElse("[unnamed]")
        println(s"Interface: $name")
        
        // Get properties of the interface
        println("Getting properties...")
        try {
          val propertiesArray = iface.getProperties().asInstanceOf[js.Dynamic]
          val properties = js.Array.from(propertiesArray.asInstanceOf[js.Iterable[js.Dynamic]]).toList
          
          println(s"  Properties: ${properties.length}")
          properties.foreach { prop =>
            try {
              val propName = Try(prop.getName().toString).getOrElse("[unnamed]")
              val typeText = Try(prop.getType().getText().toString).getOrElse("unknown")
              
              // Check if isOptional is available as a function or property
              val isOptional = Try {
                if (js.typeOf(prop.isOptional) == "function") {
                  prop.isOptional().asInstanceOf[Boolean]
                } else if (js.typeOf(prop.isOptional) == "boolean") {
                  prop.isOptional.asInstanceOf[Boolean]
                } else {
                  // Check if the property has a questionToken
                  js.typeOf(prop.getQuestionTokenNode) != "undefined"
                }
              }.getOrElse(false)
              
              println(s"    - $propName${if (isOptional) "?" else ""}: $typeText")
            } catch {
              case e: Throwable =>
                println(s"    Error processing property: ${e.getMessage}")
            }
          }
        } catch {
          case e: Throwable =>
            println(s"  Error getting properties: ${e.getMessage}")
        }
      }
      
      // Get classes from the source file
      println("Calling getClasses()...")
      try {
        // Convert the classes to a JavaScript array first
        val classesArray = sourceFile.getClasses().asInstanceOf[js.Dynamic]
        val classes = js.Array.from(classesArray.asInstanceOf[js.Iterable[js.Dynamic]]).toList
        
        println(s"Found ${classes.length} classes:")
        classes.foreach { cls =>
          val name = Try(cls.getName().toString).getOrElse("[unnamed]")
          println(s"Class: $name")
          
          // Get methods of the class
          try {
            val methodsArray = cls.getMethods().asInstanceOf[js.Dynamic]
            val methods = js.Array.from(methodsArray.asInstanceOf[js.Iterable[js.Dynamic]]).toList
            
            println(s"  Methods: ${methods.length}")
            methods.foreach { method =>
              try {
                val methodName = Try(method.getName().toString).getOrElse("[unnamed]")
                val returnType = Try(method.getReturnType().getText().toString).getOrElse("unknown")
                println(s"    - $methodName(): $returnType")
              } catch {
                case e: Throwable =>
                  println(s"    Error processing method: ${e.getMessage}")
              }
            }
          } catch {
            case e: Throwable =>
              println(s"  Error getting methods: ${e.getMessage}")
          }
        }
      } catch {
        case e: Throwable =>
          println(s"Error getting classes: ${e.getMessage}")
      }
      
      println("ts-morph test completed successfully!")
    } catch {
      case e: Throwable =>
        println(s"Error testing ts-morph: ${e.getMessage}")
        e.printStackTrace()
    }
  }
  
  /**
   * Try to load the ts-morph module from various locations
   */
  private def tryLoadTsMorph(): js.Dynamic = {
    val modulePaths = List(
      "ts-morph",
      "../js-typings/node_modules/ts-morph",
      "./js-typings/node_modules/ts-morph",
      "/Users/agent/Documents/chester/js-typings/node_modules/ts-morph"
    )
    
    var tsMorph: js.Dynamic = null
    
    for (path <- modulePaths if tsMorph == null) {
      try {
        println(s"Trying to load ts-morph from: $path")
        tsMorph = js.Dynamic.global.require(path)
        println(s"Successfully loaded ts-morph from: $path")
      } catch {
        case e: Throwable => 
          println(s"Failed to load from $path: ${e.getMessage}")
      }
    }
    
    tsMorph
  }
} 