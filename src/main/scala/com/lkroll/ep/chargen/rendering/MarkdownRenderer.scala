package com.lkroll.ep.chargen.rendering

class MarkdownRenderer(
  val prelude: String = "",
  val ending:  String = "") extends Renderer {

  override type Output = String;

  private val sb = new StringBuilder();
  private var finalised = false;

  private def checkFinalised(): Unit = {
    if (finalised) {
      throw new RuntimeException("Renderer was finalised already.")
    }
  }

  override def beginStages(numberOfStages: Int): Unit = {
    ensureEmptyLine();
    sb.append("## Stages\n");
  }
  override def beginResult(index: Option[Int] = None): Unit = {
    ensureEmptyLine();
    index match {
      case Some(i) => sb.append(s"# Result ${i}\n");
      case None    => sb.append("# Result\n");
    }
  }
  override def beginCharacter(name: String): Unit = {
    ensureEmptyLine();
    sb.append(s"### ${name}\n");
  }

  override def stage(s: String): Unit = {
    ensureEmptyLine();
    sb.append(s"### $s\n");
  }
  override def value(s: String): Unit = {
    if (!s.isEmpty()) {
      sb.append(s"*${s}*");
    }
  }

  override def text(s: String): Unit = {
    val parts = s.split("""\R""");
    ensureEmptyLine();
    parts.foreach(p => sb.append(s"\t${p}\n"));
    sb.append(s"\n");
  }

  override def note(s: String): Unit = {
    sb.append(s" (${s})");
  }

  override def kv(l: List[(String, String)]): Unit = {
    ensureEmptyLine();
    l.foreach {
      case (k, v) => sb.append(s"- **${k}** $v\n")
    }
  }

  override def section(headline: String): Unit = {
    ensureEmptyLine();
    sb.append(s"#### ${headline}\n");
  }

  override def subsection(headline: String): Unit = {
    ensureEmptyLine();
    sb.append(s"##### ${headline}\n");
  }

  override def labelled(label: String, s: String): Unit = sb.append(s"*${label}*: ${s}");
  override def raw(s: String): Unit = sb.append(s);
  override def field(s: String): Unit = sb.append(s": ${s}");

  override def table(tab: List[List[String]]): Unit = {
    ensureEmptyLine();
    tab match {
      case row :: Nil => {
        val header = row.zipWithIndex.map(_._2.toString());
        sb.append(s"${header.mkString(" | ")}\n");
        val align = row.zipWithIndex.map(_ => ":---");
        sb.append(s"${align.mkString(" | ")}\n");
        sb.append(s"${row.mkString(" | ")}\n");
        sb.append('\n');
      }
      case header :: body => {
        sb.append(s"${header.mkString(" | ")}\n");
        val align = header.zipWithIndex.map(_ => ":---");
        sb.append(s"${align.mkString(" | ")}\n");
        body.foreach { row =>
          sb.append(s"${row.mkString(" | ")}\n");
        }
        sb.append('\n');
      }
      case Nil => // nothing
    }
  }

  override def list(l: List[String]): Unit = {
    ensureEmptyLine();
    l.foreach(s => sb.append(s"- $s\n"));
  }

  override def newline(): Unit = sb.append('\n');

  override def result: Output = {
    checkFinalised();
    finalised = true;
    sb.append(ending);
    sb.result()
  }

  private def ensureEmptyLine(): Unit = {
    if (!sb.isEmpty && sb.charAt(sb.length - 1) != '\n') {
      sb.append('\n');
    }
    sb.append('\n')
  }

}
