package chester.tyck

import scala.tools.asm._
import scala.tools.asm.tree._
import scala.jdk.CollectionConverters._
import java.nio.file.{Files, Paths}

object ClassFileInspector {
  def inspectClassFile(classFilePath: String): Unit = {
    // Read the class file bytes
    val classBytes = Files.readAllBytes(Paths.get(classFilePath))

    // Create a ClassReader to parse the class bytes
    val classReader = new ClassReader(classBytes)

    // Use a ClassNode to hold the parsed class
    val classNode = new ClassNode()

    // Accept the ClassNode to populate it with data
    classReader.accept(classNode, 0)

    // Print class information
    println(s"Class: ${classNode.name.replace('/', '.')}")

    // Fields
    println("Fields:")
    for (field <- classNode.fields.asInstanceOf[java.util.List[FieldNode]].asScala) {
      val access = field.access
      val isPublic = (access & Opcodes.ACC_PUBLIC) != 0
      if (isPublic) {
        println(s"  ${field.name}: ${field.desc}")
      }
    }

    // Methods
    println("Methods:")
    for (method <- classNode.methods.asInstanceOf[java.util.List[MethodNode]].asScala) {
      val access = method.access
      val isPublic = (access & Opcodes.ACC_PUBLIC) != 0
      if (isPublic) {
        println(s"  ${method.name}${method.desc}")
      }
    }
  }
}
