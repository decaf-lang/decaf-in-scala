package decaf.backend.jvm

import java.io.{BufferedOutputStream, FileOutputStream}

case class JVMClass(name: String, code: Array[Byte]) {
  def writeFile(fileName: String = name + ".class"): Unit = {
    val stream = new BufferedOutputStream(new FileOutputStream(fileName))
    stream.write(code)
    stream.close()
  }
}
