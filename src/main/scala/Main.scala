import java.nio.file._

import scala.collection.JavaConverters._

object Main {
  def main(args: Array[String]): Unit = {
    val start =
      if (args.length > 0) Paths.get(args(0))
      else Paths.get("./")

    val modules = Files.walk(start).iterator().asScala.filter { f =>
      f.getFileName().toString.endsWith(".info")
    }.map { DrupalModule fromFile _ }.foldLeft(Map[String, DrupalModule]()) { (m, dm) =>
      m + (dm.slug -> dm)
    }

    println("digraph {")
    val unfound = modules.flatMap {
      _._2.dependencies
    }.toSet.filter { m => !modules.isDefinedAt(m) }
    modules foreach { case ((m, dm)) =>
      val slug = m.replace("\"", "\\\"")
      val name = dm.name.replace("\"", "\\\"")
      val project = dm.project.replace("\"", "\\\"")
      val deps = dm.dependencies map { _.replace("\"", "\\\"") }

      println(s"""  "${slug}" [label="${name}",URL="https://drupal.org/project/${project}"]""")
      deps foreach { d => println(s"""  "${slug}" -> "${d}" """) }
    }
    println("  subgraph uninstalled {")
    // println("""    [label="Uninstalled"]""")
    unfound foreach { u =>
      val slug = u.replace("\"", "\\\"")
      println(s"""    "${slug}" [URL="https://drupal.org/project/${slug}"]""")
    }
    println("  }")
    println("}")
  }
}
