package decaf.jvm

import java.io.{BufferedOutputStream, FileOutputStream}
import java.nio.file.Path

case class JVMClass(name: String, code: Array[Byte]) {
  def writeFile(dir: Path): Unit = {
    val path = dir.resolve(name + ".class")
    new BufferedOutputStream(new FileOutputStream(path.toFile)) {
      write(code)
      close()
    }
  }
}
