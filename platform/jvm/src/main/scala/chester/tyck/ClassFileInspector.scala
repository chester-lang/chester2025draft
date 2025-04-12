package chester.tyck

import scala.tools.asm.*
import scala.tools.asm.tree.*
import scala.jdk.CollectionConverters.*
import java.nio.file.{Files, Paths}
import chester.i18n.*

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
    println(t"Class: ${classNode.name.replace('/', '.')}")

    // Fields
    println("Fields:")
    for (field <- classNode.fields.asInstanceOf[java.util.List[FieldNode]].asScala) {
      val access = field.access
      val isPublic = (access & Opcodes.ACC_PUBLIC) != 0
      if (isPublic) {
        println(t"  ${field.name}: ${field.desc}")
      }
    }

    // Methods
    println("Methods:")
    for (method <- classNode.methods.asInstanceOf[java.util.List[MethodNode]].asScala) {
      val access = method.access
      val isPublic = (access & Opcodes.ACC_PUBLIC) != 0
      if (isPublic) {
        println(t"  ${method.name}${method.desc}")
      }
    }
  }
}
