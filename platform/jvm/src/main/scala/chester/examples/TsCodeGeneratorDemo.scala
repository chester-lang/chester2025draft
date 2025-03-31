package chester.examples

import chester.TsMorph
import chester.utils.tsmorph.Models._
import java.nio.file.{Files, Paths}
import java.nio.charset.StandardCharsets

/**
 * Demo application showing how to generate TypeScript from Scala model definitions.
 */
object TsCodeGeneratorDemo {
  def main(args: Array[String]): Unit = {
    println("TypeScript Code Generator Demo")
    println("==============================")
    
    // Generate a complete TypeScript model
    generateTypeScriptModels()
  }
  
  def generateTypeScriptModels(): Unit = {
    println("\nGenerating TypeScript model interfaces...\n")
    
    // Define our domain model types
    val userInterface = generateUserInterface()
    val userRoleEnum = generateUserRoleEnum()
    val addressInterface = generateAddressInterface()
    val productInterface = generateProductInterface()
    val orderInterface = generateOrderInterface()
    
    // Print the generated TypeScript
    println("// User.ts")
    println(userInterface)
    println()
    
    println("// UserRole.ts")
    println(userRoleEnum)
    println()
    
    println("// Address.ts")
    println(addressInterface)
    println()
    
    println("// Product.ts")
    println(productInterface)
    println()
    
    println("// Order.ts")
    println(orderInterface)
    println()
    
    // Save to files (optional)
    val outputDir = Paths.get("ts-output")
    if (!Files.exists(outputDir)) {
      Files.createDirectory(outputDir): Unit
    }
    
    Files.write(outputDir.resolve("User.ts"), userInterface.getBytes(StandardCharsets.UTF_8))
    Files.write(outputDir.resolve("UserRole.ts"), userRoleEnum.getBytes(StandardCharsets.UTF_8))
    Files.write(outputDir.resolve("Address.ts"), addressInterface.getBytes(StandardCharsets.UTF_8))
    Files.write(outputDir.resolve("Product.ts"), productInterface.getBytes(StandardCharsets.UTF_8))
    Files.write(outputDir.resolve("Order.ts"), orderInterface.getBytes(StandardCharsets.UTF_8))
      
    println(s"TypeScript files generated in ${outputDir.toAbsolutePath}")
  }
  
  def generateUserInterface(): String = {
    val properties = List(
      PropertyDefinition(
        name = "id",
        typeName = "number",
        documentation = Some("Unique user ID")
      ),
      PropertyDefinition(
        name = "username",
        typeName = "string",
        documentation = Some("The user's login name")
      ),
      PropertyDefinition(
        name = "email",
        typeName = "string",
        documentation = Some("The user's email address")
      ),
      PropertyDefinition(
        name = "firstName",
        typeName = "string",
        documentation = Some("The user's first name")
      ),
      PropertyDefinition(
        name = "lastName",
        typeName = "string",
        documentation = Some("The user's last name")
      ),
      PropertyDefinition(
        name = "role",
        typeName = "UserRole",
        documentation = Some("The user's role")
      ),
      PropertyDefinition(
        name = "active",
        typeName = "boolean",
        documentation = Some("Whether the user account is active")
      ),
      PropertyDefinition(
        name = "address",
        typeName = "Address",
        isOptional = true,
        documentation = Some("The user's address information")
      ),
      PropertyDefinition(
        name = "createdAt",
        typeName = "Date",
        isReadonly = true,
        documentation = Some("When the user was created")
      ),
      PropertyDefinition(
        name = "lastLogin",
        typeName = "Date | null",
        isOptional = true,
        documentation = Some("When the user last logged in")
      )
    )
    
    val methods = List(
      MethodDefinition(
        name = "getFullName",
        returnType = "string",
        documentation = Some("Get the user's full name")
      ),
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
    
    TsMorph.generateInterface(
      name = "User",
      props = properties,
      methods = methods
    )
  }
  
  def generateUserRoleEnum(): String = {
    // Since we don't have direct enum support, we'll create a type union
    val code = """
    /**
     * User role in the system
     */
    export type UserRole = 'admin' | 'manager' | 'user' | 'guest';
    
    /**
     * Helper object with all available roles
     */
    export const UserRoles = {
      ADMIN: 'admin' as UserRole,
      MANAGER: 'manager' as UserRole,
      USER: 'user' as UserRole,
      GUEST: 'guest' as UserRole
    };
    """
    
    code
  }
  
  def generateAddressInterface(): String = {
    val properties = List(
      PropertyDefinition(
        name = "street",
        typeName = "string",
        documentation = Some("Street address")
      ),
      PropertyDefinition(
        name = "city",
        typeName = "string",
        documentation = Some("City name")
      ),
      PropertyDefinition(
        name = "state",
        typeName = "string",
        documentation = Some("State or province")
      ),
      PropertyDefinition(
        name = "postalCode",
        typeName = "string",
        documentation = Some("Postal code or ZIP")
      ),
      PropertyDefinition(
        name = "country",
        typeName = "string",
        documentation = Some("Country name")
      )
    )
    
    TsMorph.generateInterface(
      name = "Address",
      props = properties
    )
  }
  
  def generateProductInterface(): String = {
    val properties = List(
      PropertyDefinition(
        name = "id",
        typeName = "number",
        documentation = Some("Unique product ID")
      ),
      PropertyDefinition(
        name = "name",
        typeName = "string",
        documentation = Some("Product name")
      ),
      PropertyDefinition(
        name = "description",
        typeName = "string",
        documentation = Some("Product description")
      ),
      PropertyDefinition(
        name = "price",
        typeName = "number",
        documentation = Some("Product price")
      ),
      PropertyDefinition(
        name = "category",
        typeName = "string",
        documentation = Some("Product category")
      ),
      PropertyDefinition(
        name = "tags",
        typeName = "string[]",
        documentation = Some("Product tags")
      ),
      PropertyDefinition(
        name = "inStock",
        typeName = "boolean",
        documentation = Some("Whether the product is in stock")
      ),
      PropertyDefinition(
        name = "imageUrl",
        typeName = "string",
        isOptional = true,
        documentation = Some("URL to the product image")
      )
    )
    
    TsMorph.generateInterface(
      name = "Product",
      props = properties
    )
  }
  
  def generateOrderInterface(): String = {
    val properties = List(
      PropertyDefinition(
        name = "id",
        typeName = "number",
        documentation = Some("Unique order ID")
      ),
      PropertyDefinition(
        name = "userId",
        typeName = "number",
        documentation = Some("ID of the user who placed the order")
      ),
      PropertyDefinition(
        name = "products",
        typeName = "Array<{ productId: number, quantity: number }>",
        documentation = Some("Products in the order with quantities")
      ),
      PropertyDefinition(
        name = "totalAmount",
        typeName = "number",
        documentation = Some("Total order amount")
      ),
      PropertyDefinition(
        name = "status",
        typeName = "'pending' | 'processing' | 'shipped' | 'delivered' | 'cancelled'",
        documentation = Some("Current order status")
      ),
      PropertyDefinition(
        name = "shippingAddress",
        typeName = "Address",
        documentation = Some("Shipping address")
      ),
      PropertyDefinition(
        name = "billingAddress",
        typeName = "Address",
        documentation = Some("Billing address")
      ),
      PropertyDefinition(
        name = "createdAt",
        typeName = "Date",
        isReadonly = true,
        documentation = Some("When the order was created")
      ),
      PropertyDefinition(
        name = "updatedAt",
        typeName = "Date",
        documentation = Some("When the order was last updated")
      )
    )
    
    TsMorph.generateInterface(
      name = "Order",
      props = properties
    )
  }
} 