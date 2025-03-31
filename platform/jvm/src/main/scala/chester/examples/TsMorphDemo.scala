package chester.examples

import chester.TsMorph
import chester.utils.tsmorph.Models.{TypeInfo, PropertyInfo}

/**
 * Demo application showing how to use the TsMorph functionality
 * for TypeScript analysis.
 */
object TsMorphDemo {
  def main(args: Array[String]): Unit = {
    println("TypeScript Analysis Demo")
    println("========================")
    
    // Sample TypeScript code to analyze
    val typeScriptCode = """
    /**
     * Person interface representing a user in the system
     */
    export interface Person {
      /** Unique identifier */
      id: number;
      /** Full name */
      name: string;
      /** Email address (optional) */
      email?: string;
      /** Age of the person */
      age: number;
      /** List of roles assigned */
      roles: string[];
    }
    
    /**
     * A service for managing people
     */
    export class PersonService {
      private people: Person[] = [];
      
      /**
       * Add a person to the collection
       */
      addPerson(person: Person): void {
        this.people.push(person);
      }
      
      /**
       * Get a person by their ID
       */
      getPerson(id: number): Person | undefined {
        return this.people.find(p => p.id === id);
      }
      
      /**
       * List all people
       */
      listPeople(): Person[] {
        return [...this.people];
      }
    }
    
    /**
     * Represents available role types
     */
    export type Role = 'admin' | 'user' | 'guest';
    """
    
    // Parse the TypeScript code
    TsMorph.parseTypeScript(typeScriptCode, "demo.ts") match {
      case Left(error) =>
        println(s"Error parsing TypeScript: $error")
        
      case Right(types) =>
        printTypeInfo(types)
    }
  }
  
  /**
   * Print information about the types found in the TypeScript code
   */
  private def printTypeInfo(types: List[TypeInfo]): Unit = {
    println(s"Found ${types.size} types in the TypeScript code:")
    println()
    
    types.foreach { typeInfo =>
      println(s"${typeInfo.kind}: ${typeInfo.name}")
      println("  " + "-" * 40)
      println(s"  Exported: ${typeInfo.isExported}")
      println(s"  Location: ${typeInfo.location.filePath}:${typeInfo.location.line}:${typeInfo.location.column}")
      
      if (typeInfo.properties.nonEmpty) {
        println()
        println("  Properties:")
        typeInfo.properties.foreach { prop =>
          val optionalMark = if (prop.isOptional) "?" else ""
          println(s"    - ${prop.name}$optionalMark: ${prop.typeText}")
        }
      }
      
      println()
    }
  }
} 