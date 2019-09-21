package com.lkroll.ep.chargen.character

import com.lkroll.ep.chargen._
import com.lkroll.ep.chargen.utils._
import com.lkroll.ep.compendium._
import com.lkroll.ep.compendium.data._

object Disorders extends Table {
  override type Result = EPTrait;

  val data: RollTable[String] = RollTable(
    (1 to 8) -> "Addiction",
    (9 to 11) -> "Alien Behaviour Disorder",
    (12 to 14) -> "Alien Sensory Disorder",
    (15 to 17) -> "Atavism",
    (18 to 21) -> "ADHD",
    (22 to 24) -> "Autophagy",
    (25 to 30) -> "Bipolar Disorder",
    (31 to 34) -> "Body Dysmorphia",
    (35 to 38) -> "Borderline Personality Disorder",
    (39 to 41) -> "Cosmic Anxiety Disorder",
    (42 to 47) -> "Depression",
    (48 to 51) -> "Fugue",
    (52 to 57) -> "General Anxiety Disorder",
    (58 to 61) -> "Hypochondria",
    (62 to 67) -> "Impulse Control Disorder",
    (68 to 71) -> "Insomnia",
    (72 to 74) -> "Megalomania",
    (75 to 79) -> "Multiple Personality Disorder",
    (80 to 84) -> "Obsessive Compulsive Disorder",
    (85 to 88) -> "Phobia",
    (89 to 94) -> "Post-Traumatic Stress Disorder",
    (95 to 97) -> "Schizophrenia",
    (98 to 100) -> "Species Dysmorphia"
  );

  override def label: String = "Disorders";
  override def source: String = "Transhuman p.49";
  override def roll(rand: Random): Result = {
    val name = data.randomElement(rand).get;
    TraitsNegativeEP.mentalDisorder.copy(name = s"Mental Disorder ($name)")
  }
}
