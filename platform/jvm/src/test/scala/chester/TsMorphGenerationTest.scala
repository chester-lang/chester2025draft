package chester

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import chester.utils.tsmorph.Models._

class TsMorphGenerationTest extends AnyFunSuite with Matchers {

  test("generate TypeScript interface from Scala definition") {
    // Define an interface with various property types
    val properties = List(
      PropertyDefinition(
        name = "id", 
        typeName = "number",
        documentation = Some("Unique identifier")
      ),
      PropertyDefinition(
        name = "name", 
        typeName = "string",
        documentation = Some("Full name")
      ),
      PropertyDefinition(
        name = "email", 
        typeName = "string",
        isOptional = true,
        documentation = Some("Email address")
      ),
      PropertyDefinition(
        name = "active", 
        typeName = "boolean",
        documentation = Some("Whether the user is active")
      ),
      PropertyDefinition(
        name = "createdAt", 
        typeName = "Date",
        isReadonly = true,
        documentation = Some("Creation timestamp")
      ),
      PropertyDefinition(
        name = "roles", 
        typeName = "string[]",
        documentation = Some("User roles")
      )
    )
    
    // Define methods
    val methods = List(
      MethodDefinition(
        name = "isAdmin",
        returnType = "boolean",
        documentation = Some("Check if the user is an admin")
      ),
      MethodDefinition(
        name = "hasPermission",
        returnType = "boolean",
        parameters = List(
          ParameterDefinition(
            name = "permission",
            typeName = "string"
          )
        ),
        documentation = Some("Check if the user has a specific permission")
      )
    )
    
    // Generate the TypeScript interface
    val result = TsMorph.generateInterface(
      name = "User",
      props = properties,
      methods = methods,
      isExported = true
    )
    
    result.isRight shouldBe true
    
    result.foreach { ts =>
      // Check that the generated TypeScript includes all our defined properties
      ts should include("export interface User")
      ts should include("/** Unique identifier */")
      ts should include("id: number;")
      ts should include("/** Email address */")
      ts should include("email?: string;")
      ts should include("/** Creation timestamp */")
      ts should include("readonly createdAt: Date;")
      ts should include("/** User roles */")
      ts should include("roles: string[];")
      
      // Check that methods are included
      ts should include("/** Check if the user is an admin */")
      ts should include("isAdmin(): boolean;")
      ts should include("/** Check if the user has a specific permission */")
      ts should include("hasPermission(permission: string): boolean;")
    }
  }
  
  test("generate TypeScript class") {
    // Define properties for a Product class
    val properties = List(
      PropertyDefinition(
        name = "id", 
        typeName = "number",
        documentation = Some("Product ID")
      ),
      PropertyDefinition(
        name = "name", 
        typeName = "string",
        documentation = Some("Product name")
      ),
      PropertyDefinition(
        name = "price", 
        typeName = "number",
        documentation = Some("Product price")
      ),
      PropertyDefinition(
        name = "inStock", 
        typeName = "boolean",
        documentation = Some("Whether the product is in stock")
      )
    )
    
    // Define methods
    val methods = List(
      MethodDefinition(
        name = "calculateTax",
        returnType = "number",
        parameters = List(
          ParameterDefinition(
            name = "taxRate",
            typeName = "number",
            defaultValue = Some("0.1")
          )
        ),
        documentation = Some("Calculate tax based on the product price")
      ),
      MethodDefinition(
        name = "toString",
        returnType = "string",
        documentation = Some("Convert product to string representation")
      )
    )
    
    // Generate the TypeScript class
    val result = TsMorph.generateClass(
      name = "Product",
      props = properties,
      methods = methods,
      isExported = true,
      implements_ = List("IProduct")
    )
    
    result.isRight shouldBe true
    
    result.foreach { ts =>
      // Check that the generated TypeScript includes all our defined properties
      ts should include("export class Product implements IProduct")
      ts should include("/** Product ID */")
      ts should include("id: number;")
      ts should include("/** Product name */")
      ts should include("name: string;")
      
      // Check that methods are included
      ts should include("/** Calculate tax based on the product price */")
      ts should include("calculateTax(taxRate: number = 0.1): number {")
      ts should include("/** Convert product to string representation */")
      ts should include("toString(): string {")
    }
  }
  
  test("generate complex interface with extends") {
    // Define an interface that extends another
    val properties = List(
      PropertyDefinition(
        name = "additionalInfo", 
        typeName = "string",
        documentation = Some("Additional user information")
      ),
      PropertyDefinition(
        name = "loginCount", 
        typeName = "number",
        documentation = Some("Number of times the user has logged in")
      )
    )
    
    // Generate the TypeScript interface
    val result = TsMorph.generateInterface(
      name = "AdminUser",
      props = properties,
      extends_ = List("User", "IPermissions")
    )
    
    result.isRight shouldBe true
    
    result.foreach { ts =>
      // Check that the interface extends the specified interfaces
      ts should include("interface AdminUser extends User, IPermissions")
      ts should include("/** Additional user information */")
      ts should include("additionalInfo: string;")
      ts should include("/** Number of times the user has logged in */")
      ts should include("loginCount: number;")
    }
  }
  
  test("generate class with extends and implements") {
    // Define a class that extends another and implements interfaces
    val properties = List(
      PropertyDefinition(
        name = "adminLevel", 
        typeName = "number",
        documentation = Some("Admin privilege level")
      )
    )
    
    // Generate the TypeScript class
    val result = TsMorph.generateClass(
      name = "AdminUser",
      props = properties,
      extends_ = Some("User"),
      implements_ = List("IAdmin", "IPermissionsManager")
    )
    
    result.isRight shouldBe true
    
    result.foreach { ts =>
      // Check that the class extends and implements correctly
      ts should include("class AdminUser extends User implements IAdmin, IPermissionsManager")
      ts should include("/** Admin privilege level */")
      ts should include("adminLevel: number;")
    }
  }
} 