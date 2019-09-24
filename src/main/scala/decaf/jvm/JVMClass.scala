package decaf.jvm

import java.io.{BufferedOutputStream, FileOutputStream}

import scala.reflect.io.Path

case class JVMClass(name: String, code: Array[Byte]) {

  def writeFile(dir: Path): Unit = {
    val path = dir / name + ".class"
    new BufferedOutputStream(new FileOutputStream(path)) {
      write(code)
      close()
    }
  }
}
