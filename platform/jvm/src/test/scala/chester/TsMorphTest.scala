package chester

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import java.nio.file.{Files, Paths}
import java.nio.charset.StandardCharsets
import java.io.File
import chester.utils.tsmorph.Models.{TypeInfo, PropertyInfo, Location}

class TsMorphTest extends AnyFunSuite with Matchers {

  test("parse TypeScript source code") {
    // Sample TypeScript interface
    val sampleTs = """
      export interface User {
        id: number;
        name: string;
        email?: string;
        roles: string[];
      }
      
      export class UserService {
        getUser(id: number): User {
          return { id, name: "Test User", roles: [] };
        }
      }
    """
    
    // Test parsing directly from source
    val result = TsMorph.parseTypeScript(sampleTs, "test.ts")
    
    result.isRight shouldBe true
    
    result.foreach { types =>
      // Should find both our interface and class
      types.length should be >= 2
      
      // Find the User interface
      val userInterface = types.find(_.name == "User")
      userInterface shouldBe defined
      
      userInterface.foreach { user =>
        user.isExported shouldBe true
        user.kind shouldBe "Interface"
        
        // Check properties
        user.properties.length shouldBe 4
        
        // Find the optional email property
        val emailProp = user.properties.find(_.name == "email")
        emailProp shouldBe defined
        emailProp.foreach { email =>
          email.isOptional shouldBe true
          email.typeText shouldBe "string"
        }
      }
      
      // Find the UserService class
      val userServiceClass = types.find(_.name == "UserService")
      userServiceClass shouldBe defined
      
      userServiceClass.foreach { service =>
        service.isExported shouldBe true
        service.kind shouldBe "Class"
      }
    }
  }
  
  test("handle TypeScript parsing errors") {
    // Invalid TypeScript with syntax error
    val invalidTs = """
      export interface Broken {
        id: number
        // Missing semicolon and other syntax issues
        name: string:
        complexType: {; // Invalid syntax
      }
    """
    
    val result = TsMorph.parseTypeScript(invalidTs, "invalid.ts")
    
    // Should return an error
    result.isLeft shouldBe true
  }
  
  test("create sample TypeScript file and analyze it") {
    // Only run this test if we're not in a CI environment to avoid file system issues
    assume(System.getenv("CI") == null, "Skipping file-based test in CI environment")
    
    // Create a temporary TypeScript file
    val tempDir = Files.createTempDirectory("tsmorph-test")
    val tempFile = tempDir.resolve("sample.ts")
    
    val tsContent = """
      export interface Product {
        id: number;
        name: string;
        price: number;
        description?: string;
        categories: string[];
      }
      
      export class ProductManager {
        private products: Product[] = [];
        
        addProduct(product: Product): void {
          this.products.push(product);
        }
        
        getProduct(id: number): Product | undefined {
          return this.products.find(p => p.id === id);
        }
      }
    """
    
    Files.write(tempFile, tsContent.getBytes(StandardCharsets.UTF_8))
    
    // Analyze the file
    val result = TsMorph.analyzeTsFile(tempFile.toString)
    
    result.isRight shouldBe true
    
    result.foreach { types =>
      // Should find both our interface and class
      types.length should be >= 2
      
      // Find the Product interface
      val productInterface = types.find(_.name == "Product")
      productInterface shouldBe defined
      
      // Find the ProductManager class
      val managerClass = types.find(_.name == "ProductManager")
      managerClass shouldBe defined
    }
    
    // Clean up
    Files.delete(tempFile)
    Files.delete(tempDir)
  }
  
  test("analyze sample.ts from resources") {
    // Get the sample.ts file from resources
    val url = getClass.getResource("/sample.ts")
    url should not be null
    
    val file = new File(url.toURI)
    file.exists should be (true)
    
    // Analyze the file
    val result = TsMorph.analyzeTsFile(file.getAbsolutePath)
    
    result.isRight shouldBe true
    
    result.foreach { types =>
      // Should find all our exported types
      types.length should be >= 5
      
      // Find the User interface
      val userInterface = types.find(_.name == "User")
      userInterface shouldBe defined
      
      userInterface.foreach { user =>
        user.isExported shouldBe true
        user.kind shouldBe "Interface"
        
        // Check that we have all properties
        user.properties.length shouldBe 6
        
        // Check optional email property
        val emailProp = user.properties.find(_.name == "email")
        emailProp shouldBe defined
        emailProp.foreach { email =>
          email.isOptional shouldBe true
          email.typeText shouldBe "string"
        }
        
        // Check the union type property
        val statusProp = user.properties.find(_.name == "status")
        statusProp shouldBe defined
        statusProp.foreach { status =>
          status.typeText should include ("active")
          status.typeText should include ("inactive")
          status.typeText should include ("suspended")
        }
      }
      
      // Find the UserService class
      val userServiceClass = types.find(_.name == "UserService")
      userServiceClass shouldBe defined
      
      // Find UserRole type
      val userRoleType = types.find(_.name == "UserRole")
      userRoleType shouldBe defined
      
      // Find PaginatedResult interface
      val paginatedResult = types.find(_.name == "PaginatedResult")
      paginatedResult shouldBe defined
    }
  }
} 