package com.lkroll.ep.chargen

import com.lkroll.ep.chargen.character.{ CharGenCharacter, Skill }
import com.lkroll.ep.chargen.rendering.Renderer

trait StageResult {
  def render(renderer: Renderer): Unit;
}

case class CreationResult(character: CharGenCharacter, stages: List[StageResult]) {
  def render(renderer: Renderer): Unit = {
    renderer.beginCharacter(character.name);
    character.render(renderer);
    renderer.endCharacter();
    renderer.beginStages(stages.size);
    stages.foreach(_.render(renderer));
    renderer.endStages();
  }

  def withName(name: String, source: String): CreationResult = {
    val newChar = character.copy(name = name);
    val newStages = stages :+ RandomName(name, source);
    CreationResult(newChar, newStages)
  }

  def findNativeLanguage(): Skill = {
    stages.foreach {
      case lifepath.Stages.NativeLangTable(nativeLang) => return nativeLang
      case _ => // ignore
    }
    return lifepath.LanguageTable.pick(99)
  }
}

case class SystemResult(system: CreationSystem, note: Option[String] = None) extends StageResult {
  def render(renderer: Renderer): Unit = {
    renderer.stage("Creation System");
    renderer.value(system.label);
    note.foreach(renderer.note(_));
  }
}

case class RandomName(name: String, source: String) extends StageResult {
  def render(renderer: Renderer): Unit = {
    renderer.stage("Random Name");
    renderer.value(name);
    renderer.note(s"from $source");
  }
}

trait CreationSystem {
  def label: String;
  def randomCharacter(rand: Random): CreationResult;
}
