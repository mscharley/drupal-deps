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

    val uninstalledDependencies = modules.flatMap {
      _._2.dependencies
    }.toSet.filter { m => !modules.isDefinedAt(m) }
    val projects = modules.foldLeft(Map[String, Map[String, DrupalModule]]().withDefault(_ => Map())) {
      case (m: Map[String, Map[String, DrupalModule]], (name, dm)) =>
        m + (dm.project -> (m(dm.project) + (name -> dm)))
    }

    println("digraph {")
    projects foreach { case ((project, mods)) =>
      println(s"  subgraph cluster_${project} {")
      println(s"""    label="${project}"""")
      println("""    graph [style="solid,filled",fillcolor="#DADADA"]""")

      val deps = mods flatMap { case ((m, dm)) =>
        val slug = m.replace("\"", "\\\"")
        val name = dm.name.replace("\"", "\\\"")
        val project = dm.project.replace("\"", "\\\"")
        val deps = dm.dependencies map { _.replace("\"", "\\\"") }

        println(s"""    "${slug}" [label="${name}",URL="https://drupal.org/project/${project}"]""")
        deps map { d =>
          val targetProject = modules.get(d).map(_.project).getOrElse("uninstalled")
          if (project == targetProject) s"""  ${slug} -> "${d}""""
          else s"""  "cluster_${project}" -> "cluster_${targetProject}""""
        }
      }
      println("  }")
      deps.toSet foreach { println _ }
    }
    println("  subgraph cluster_uninstalled {")
    println("""    label="Uninstalled"""")
    println("""    graph [style="solid,filled",fillcolor="#DADADA"]""")
    uninstalledDependencies foreach { u =>
      val slug = u.replace("\"", "\\\"")
      println(s"""    "${slug}" [URL="https://drupal.org/project/${slug}"]""")
    }
    println("  }")
    println("}")
  }
}
