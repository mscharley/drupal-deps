import java.nio.charset.Charset
import java.nio.file.{Files, Path}

import scala.collection.JavaConverters._

object DrupalModule {
  private val utf8 = Charset forName "UTF-8"

  def fromFile(file: Path): DrupalModule = {
    val lines = Files.readAllLines(file, utf8).asScala
    val config: Map[String, IndexedSeq[String]] = lines.filter { l =>
      !l.startsWith(";") && l.contains("=")
    }.map { l =>
      l.split("=", 2).map { x => x.trim() }
    }.foldLeft(Map[String, Vector[String]]().withDefault(_ => Vector())) { (m, l) =>
      val v =
        if (l(1).length > 2 && l(1)(0) == '\"') l(1).substring(1, l(1).length - 1)
        else l(1)
      m + (l(0) -> (m(l(0)) :+ v))
    }

    val filename = file.getFileName().toString()
    val slug = filename.substring(0, filename.length - 5)
    val name = config.getOrElse("name", Vector(slug))(0)
    val project =
      if (config.keys.map { _ startsWith "regions" }.foldLeft(false) { _ || _ }) "theme"
      else if (slug.startsWith("e_") || slug.contains("equiem")) "equiem"
      else config.getOrElse("project", Vector(slug))(0)
    val dependencies = config.getOrElse("dependencies[]", Vector()) map { dep =>
      dep.split(" ", 2)(0)
    }

    DrupalModule(slug, name, project, dependencies)
  }
}

case class DrupalModule(slug: String, name: String, project: String, dependencies: IndexedSeq[String])
