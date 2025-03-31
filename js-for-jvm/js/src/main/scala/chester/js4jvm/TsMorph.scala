package chester.js4jvm

import scala.scalajs.js
import scala.scalajs.js.annotation.*
import upickle.default.*
import chester.utils.tsmorph.Models

@JSExportTopLevel("analyzeTsFile")
def analyzeTsFile(filePath: String): String = {
  try {
    // Use direct JavaScript for the entire operation
    js.Dynamic.global.eval(s"""
      function analyze(filePath) {
        const { Project } = require("ts-morph");
        
        try {
          const project = new Project();
          const sourceFile = project.addSourceFileAtPath(filePath);
          
          // Collect interfaces
          const interfaces = sourceFile.getInterfaces().map(interface => {
            const properties = interface.getProperties().map(prop => ({
              name: prop.getName(),
              kind: prop.getKindName(),
              isOptional: prop.isOptional(),
              typeText: prop.getType().getText()
            }));
            
            return {
              name: interface.getName(),
              kind: interface.getKindName(),
              isExported: interface.isExported(),
              location: {
                filePath: sourceFile.getFilePath(),
                line: interface.getStartLineNumber(),
                column: interface.getStartColumnNumber()
              },
              properties
            };
          });
          
          // Collect classes
          const classes = sourceFile.getClasses().map(cls => {
            const properties = cls.getProperties().map(prop => ({
              name: prop.getName(),
              kind: prop.getKindName(),
              isOptional: prop.isOptional(),
              typeText: prop.getType().getText()
            }));
            
            return {
              name: cls.getName(),
              kind: cls.getKindName(),
              isExported: cls.isExported(),
              location: {
                filePath: sourceFile.getFilePath(),
                line: cls.getStartLineNumber(),
                column: cls.getStartColumnNumber()
              },
              properties
            };
          });
          
          // Collect type aliases
          const typeAliases = sourceFile.getTypeAliases().map(typeAlias => ({
            name: typeAlias.getName(),
            kind: typeAlias.getKindName(),
            isExported: typeAlias.isExported(),
            location: {
              filePath: sourceFile.getFilePath(),
              line: typeAlias.getStartLineNumber(),
              column: typeAlias.getStartColumnNumber()
            }
          }));
          
          return JSON.stringify([...interfaces, ...classes, ...typeAliases]);
        } catch (e) {
          return JSON.stringify({ error: e.message });
        }
      }
      
      analyze("${filePath}");
    """).asInstanceOf[String]
  } catch {
    case e: Exception => 
      write(Map("error" -> e.getMessage))
  }
}

@JSExportTopLevel("parseTypeScript")
def parseTypeScript(sourceCode: String, fileName: String): String = {
  try {
    // Use direct JavaScript for the entire operation
    js.Dynamic.global.eval(s"""
      function parse(sourceCode, fileName) {
        const { Project } = require("ts-morph");
        
        try {
          const project = new Project();
          const sourceFile = project.createSourceFile(fileName, sourceCode, { overwrite: true });
          
          // Collect interfaces
          const interfaces = sourceFile.getInterfaces().map(interface => {
            const properties = interface.getProperties().map(prop => ({
              name: prop.getName(),
              kind: prop.getKindName(),
              isOptional: prop.isOptional(),
              typeText: prop.getType().getText()
            }));
            
            return {
              name: interface.getName(),
              kind: interface.getKindName(),
              isExported: interface.isExported(),
              location: {
                filePath: sourceFile.getFilePath(),
                line: interface.getStartLineNumber(),
                column: interface.getStartColumnNumber()
              },
              properties
            };
          });
          
          return JSON.stringify(interfaces);
        } catch (e) {
          return JSON.stringify({ error: e.message });
        }
      }
      
      parse(`${sourceCode}`, "${fileName}");
    """).asInstanceOf[String]
  } catch {
    case e: Exception => 
      write(Map("error" -> e.getMessage))
  }
}

@JSExportTopLevel("generateTypeScript")
def generateTypeScript(definitionJson: String): String = {
  try {
    // Generate TypeScript code using direct JavaScript
    js.Dynamic.global.eval(s"""
      function generate(definitions) {
        try {
          const parsed = JSON.parse(definitions);
          let result = "";
          
          // Process interfaces and classes
          parsed.forEach(def => {
            if (!def.name) return; // Skip invalid definitions
            
            // Add export if needed
            if (def.isExported) {
              result += "export ";
            }
            
            // Determine if it's a class or interface
            const isClass = def.kind === "Class" || (def.implementsField && def.implementsField.length > 0);
            
            // Class or interface definition
            result += isClass ? `class $${def.name}` : `interface $${def.name}`;
            
            // Extends
            if (def.extendsField) {
              if (isClass && typeof def.extendsField === 'string') {
                result += ` extends $${def.extendsField}`;
              } else if (Array.isArray(def.extendsField) && def.extendsField.length > 0) {
                result += " extends ";
                result += def.extendsField.join(", ");
              }
            }
            
            // Implements (for classes)
            if (isClass && def.implementsField && def.implementsField.length > 0) {
              result += " implements ";
              result += def.implementsField.join(", ");
            }
            
            result += " {\\n";
            
            // Properties
            if (def.properties) {
              def.properties.forEach(prop => {
                // Add documentation if available
                if (prop.documentation) {
                  result += `  /** $${prop.documentation} */\\n`;
                }
                
                // Add readonly if needed
                if (prop.isReadonly) {
                  result += "  readonly ";
                }
                
                // Property definition
                result += `  $${prop.name}`;
                
                // Optional marker
                if (prop.isOptional) {
                  result += "?";
                }
                
                // Type
                result += `: $${prop.typeName};\\n`;
              });
            }
            
            // Methods
            if (def.methods) {
              def.methods.forEach(method => {
                // Add documentation if available
                if (method.documentation) {
                  result += `  /** $${method.documentation} */\\n`;
                }
                
                // Method signature
                result += `  $${method.name}(`;
                
                // Parameters
                const params = method.parameters ? method.parameters.map(param => {
                  const optional = param.isOptional ? "?" : "";
                  const defaultValue = param.defaultValue ? ` = $${param.defaultValue}` : "";
                  return `$${param.name}$${optional}: $${param.typeName}$${defaultValue}`;
                }) : [];
                
                result += params.join(", ");
                
                if (isClass) {
                  // Class method with implementation
                  result += `): $${method.returnType} {\\n    // Implementation goes here\\n  }\\n`;
                } else {
                  // Interface method
                  result += `): $${method.returnType};\\n`;
                }
              });
            }
            
            result += "}\\n\\n";
          });
          
          return result;
        } catch (e) {
          return JSON.stringify({ error: e.message });
        }
      }
      
      generate('${definitionJson.replace("'", "\\'")}');
    """).asInstanceOf[String]
  } catch {
    case e: Exception => 
      write(Map("error" -> e.getMessage))
  }
} 