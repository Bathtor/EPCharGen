package com.lkroll.ep.chargen.rendering

trait Renderer {
  type Output;

  def beginStages(numberOfStages: Int): Unit;
  def endStages(): Unit = {};

  def stage(s: String): Unit;
  def value(s: String): Unit;
  def text(s: String): Unit;
  def note(s: String): Unit;
  def kv(l: List[(String, String)]): Unit;
  def section(headline: String): Unit;
  def subsection(headline: String): Unit;
  def labelled(label: String, s: String): Unit;
  def raw(s: String): Unit;
  def field(s: String): Unit;
  def table(tab: List[List[String]]): Unit;
  def list(l: List[String]): Unit;
  def newline(): Unit;

  def beginResult(index: Option[Int] = None): Unit;
  def endResult(): Unit = {};

  def beginCharacter(name: String): Unit;
  def endCharacter(): Unit = {};

  @throws(classOf[FinalisedException])
  def result: Output;
}

class FinalisedException(message: String) extends Exception(message) {

  def this(message: String, cause: Throwable) = {
    this(message)
    initCause(cause)
  }

  def this(cause: Throwable) = {
    this(Option(cause).map(_.toString).orNull, cause)
  }

  def this() = {
    this("The Renderer was already finalised. No actions are allowed after calling the result method.")
  }
}
