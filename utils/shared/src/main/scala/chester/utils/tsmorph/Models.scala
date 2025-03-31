package chester.utils.tsmorph

import upickle.default._

/**
 * Shared data models for TypeScript analysis and code generation.
 * These models are used by both Scala.js and JVM sides.
 */
object Models {
  // Models for TypeScript analysis
  case class TypeInfo(
      name: String,
      kind: String,
      isExported: Boolean,
      location: Location,
      properties: List[PropertyInfo] = Nil
  )

  case class PropertyInfo(
      name: String,
      kind: String,
      isOptional: Boolean,
      typeText: String
  )

  case class Location(
      filePath: String,
      line: Int,
      column: Int
  )

  // Models for TypeScript generation
  case class ClassDefinition(
      name: String,
      properties: List[PropertyDefinition],
      methods: List[MethodDefinition] = Nil,
      isExported: Boolean = true,
      extendsField: Option[String] = None,
      implementsField: List[String] = Nil
  )

  case class InterfaceDefinition(
      name: String,
      properties: List[PropertyDefinition],
      methods: List[MethodDefinition] = Nil,
      isExported: Boolean = true,
      extendsField: List[String] = Nil
  )

  case class PropertyDefinition(
      name: String,
      typeName: String,
      isOptional: Boolean = false,
      isReadonly: Boolean = false,
      documentation: Option[String] = None
  )

  case class MethodDefinition(
      name: String,
      returnType: String,
      parameters: List[ParameterDefinition] = Nil,
      documentation: Option[String] = None
  )

  case class ParameterDefinition(
      name: String,
      typeName: String,
      isOptional: Boolean = false,
      defaultValue: Option[String] = None
  )

  // upickle readers/writers for serialization
  object Serialization {
    // For TypeScript analysis
    implicit val locationRW: ReadWriter[Location] = macroRW
    implicit val propertyInfoRW: ReadWriter[PropertyInfo] = macroRW
    implicit val typeInfoRW: ReadWriter[TypeInfo] = macroRW
    
    // For TypeScript generation
    implicit val parameterDefinitionRW: ReadWriter[ParameterDefinition] = macroRW
    implicit val methodDefinitionRW: ReadWriter[MethodDefinition] = macroRW
    implicit val propertyDefinitionRW: ReadWriter[PropertyDefinition] = macroRW
    implicit val classDefinitionRW: ReadWriter[ClassDefinition] = macroRW
    implicit val interfaceDefinitionRW: ReadWriter[InterfaceDefinition] = macroRW
  }
} 