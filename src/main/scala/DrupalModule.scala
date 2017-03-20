import java.nio.charset.Charset
import java.nio.file.{Files, Path}

import scala.collection.JavaConverters._

object DrupalModule {
  private val utf8 = Charset forName "UTF-8"

  def fromFile(file: Path): DrupalModule[String] = {
    val lines = Files.readAllLines(file, utf8).asScala
    val filename = file.getFileName.toString
    val slug = filename.substring(0, filename.length - 5)

    fromLines(slug, lines)
  }

  def fromString(slug: String, contents: String): DrupalModule[String] =
    fromLines(slug, contents.lines.toSeq)

  def fromLines(slug: String, lines: Seq[String]): DrupalModule[String] = {
    val config = parseConfig(lines)
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

  private def parseConfig(lines: Seq[String]): Map[String, IndexedSeq[String]] =
    (filterComments andThen parseLines)(lines)

  private val filterComments: (Seq[String]) => Seq[String] =
    _.filter { l =>
      !l.startsWith(";") && l.contains("=")
    }

  private val parseLines: (Seq[String]) => Map[String, IndexedSeq[String]] =
    _.map { l =>
      l.split("=", 2).map { x => x.trim() }
    }.foldLeft(Map[String, Vector[String]]().withDefault(_ => Vector())) { (m, l) =>
      val v =
        if (l(1).length > 2 && l(1)(0) == '\"') l(1).substring(1, l(1).length - 1)
        else l(1)
      m + (l(0) -> (m(l(0)) :+ v))
    }
}

case class DrupalModule[T](slug: String, name: String, project: String, dependencies: IndexedSeq[T]) {
  def mapDependencies[U](f: T => U): DrupalModule[U] =
    copy(dependencies = dependencies.map(f))
}
