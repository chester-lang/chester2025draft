package chester

import java.nio.file.{Files, Path}
import upickle.default.*
import chester.utils.tsmorph.Models
import chester.utils.tsmorph.Models._
import chester.utils.tsmorph.Models.Serialization.{typeInfoRW, locationRW, propertyInfoRW, parameterDefinitionRW, methodDefinitionRW, propertyDefinitionRW, classDefinitionRW, interfaceDefinitionRW}
import _root_.scala.io.Source

/**
 * TypeScript analysis and code generation functionality using ts-morph.
 * Uses shared model definitions from chester.utils.tsmorph.Models.
 */
object TsMorph {
  // Re-export the shared models from utils for backward compatibility
  // This allows existing code to continue using TsMorph.TypeInfo instead of Models.TypeInfo
  type TypeInfo = Models.TypeInfo
  type PropertyInfo = Models.PropertyInfo
  type Location = Models.Location
  type ClassDefinition = Models.ClassDefinition
  type InterfaceDefinition = Models.InterfaceDefinition
  type PropertyDefinition = Models.PropertyDefinition
  type MethodDefinition = Models.MethodDefinition
  type ParameterDefinition = Models.ParameterDefinition

  /**
   * Analyze TypeScript file via NodeJS by using the `ts-morph` library
   * @param filePath Path to the TypeScript file
   * @return JSON string with analysis results
   */
  def analyzeTsFile(filePath: Path): String = {
    // Run the npm script to analyze the file
    val cmd = s"cd ${TsConfig.projectRoot} && node ${TsConfig.tsAnalyzerPath} --file ${filePath.toAbsolutePath}"
    val process = Runtime.getRuntime.exec(cmd)
    val output = Source.fromInputStream(process.getInputStream).mkString
    process.waitFor()
    
    output
  }
  
  /**
   * Parse JSON output from `analyzeTsFile` and extract TypeScript type information
   * @param json JSON string with analysis results
   * @return List of TypeInfo objects
   */
  def parseTypeScript(json: String): List[TypeInfo] = {
    upickle.default.read[List[TypeInfo]](json)
  }
  
  /**
   * Parse TypeScript code and extract type information
   * @param code TypeScript code as string
   * @param fileName Virtual filename for the TypeScript code
   * @return Either an error message or a list of TypeInfo objects
   */
  def parseTypeScript(code: String, fileName: String): Either[String, List[TypeInfo]] = {
    try {
      Right(parseTypeScript(analyzeTsFile(Files.write(Files.createTempFile("ts_", ".ts"), code.getBytes))))
    } catch {
      case e: Exception => Left(e.getMessage)
    }
  }
  
  /**
   * Generate TypeScript code from a Scala case class or a list of case classes
   * @param value The value to generate TypeScript for (either a case class or a list of case classes)
   * @return TypeScript code as string
   */
  def generateTypeScript[T: ReadWriter](value: T): String = {
    val json = upickle.default.write(value)
    // Call the JS function to generate TypeScript
    val cmd = s"cd ${TsConfig.projectRoot} && node ${TsConfig.tsGeneratorPath} --definition '${json}'"
    val process = Runtime.getRuntime.exec(cmd)
    val output = Source.fromInputStream(process.getInputStream).mkString
    process.waitFor()
    
    output
  }

  /**
   * Generate TypeScript interface from a Scala case class.
   * 
   * This is a convenience method to simplify TypeScript generation for common use cases.
   * 
   * @param name Interface name
   * @param props Properties to include in the interface
   * @param isExported Whether to export the interface
   * @param extends_ Interfaces this interface extends
   * @return Generated TypeScript code
   */
  def generateInterface(
      name: String,
      props: List[PropertyDefinition],
      methods: List[MethodDefinition] = Nil,
      isExported: Boolean = true,
      extends_ : List[String] = Nil
  ): String = {
    val interface = InterfaceDefinition(name, props, methods, isExported, extends_)
    generateTypeScript(interface)
  }
  
  /**
   * Generate TypeScript class from a Scala definition.
   * 
   * @param name Class name
   * @param props Properties to include in the class
   * @param methods Methods to include in the class
   * @param isExported Whether to export the class
   * @param extends_ Class this class extends (if any)
   * @param implements_ Interfaces this class implements
   * @return Generated TypeScript code
   */
  def generateClass(
      name: String,
      props: List[PropertyDefinition],
      methods: List[MethodDefinition] = Nil, 
      isExported: Boolean = true,
      extends_ : Option[String] = None,
      implements_ : List[String] = Nil
  ): String = {
    val classDef = ClassDefinition(name, props, methods, isExported, extends_, implements_)
    generateTypeScript(classDef)
  }
}

/**
 * Configuration for TypeScript interop
 */
object TsConfig {
  // Path to the project root (where node_modules and package.json are located)
  val projectRoot: String = "./js-for-jvm" 
  
  // Path to the TypeScript analyzer script
  val tsAnalyzerPath: String = "./analyzer.js"
  
  // Path to the TypeScript generator script  
  val tsGeneratorPath: String = "./generator.js"
} 