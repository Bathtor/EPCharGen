package com.lkroll.ep.chargen.lifepath

import com.lkroll.ep.chargen._
import com.lkroll.ep.chargen.utils._

object StoryEvent extends Table with Pickable {
  override type Result = String;

  val data: RollTable[String] = RollTable(
    (1 to 2) -> "After a long stretch of bad, you hit bottom. No way left to go but up.",
    (3 to 4) -> "You participate as a test subject in a research project. You suffer no ill effects ... that you can tell.",
    (5 to 6) -> "A prominent journalist befriends you as a source and occasional confidante.",
    (7 to 8) -> "You hear from an unknown source that Oversight has taken an interest in your affairs.",
    (9 to 10) -> "You have an unfortunate run-in with Jovian Republic troops, but manage to extricate yourself.",
    (11 to 12) -> "After years, you finally get a chance to inflict revenge on someone. Do you take it or walk away?",
    (13 to 14) -> "You witness/survive a major disaster, such as a habitat failure, ship collision, terrorist attack, or a freak but deadly accident.",
    (15 to 16) -> "Circumstances force you to move from one end of the solar system to the other.",
    (17 to 18) -> "Your habitat goes through a regime change. Which side are you on?",
    (19 to 20) -> "You are falsely accused of a crime but then cleared.",
    (21 to 22) -> "You develop a long-term rival. The relationship is complex and non-dangerous, but it does occasionally interfere or consume your attention.",
    (23 to 24) -> "You develop a long-term life-partner relationship.",
    (25 to 26) -> "You suffer through the failure of a major long-term relationship.",
    (27 to 28) -> "You enter into a convenience-based contract-defined romantic relationship.",
    (29 to 30) -> "You develop an ongoing polyamorous relationship with a group of friends.",
    (31 to 32) -> "You are pursued by an irritating but (mostly) harmless suitor/ stalker.",
    (33 to 34) -> "You are recruited to secretly help some faction. Randomly determine that faction from the Factions table (p. 41).",
    (35 to 36) -> "You are re-united with a lover/relative/friend thought lost during the Fall.",
    (37 to 38) -> "Political upheaval in your local habitat/polity throws your life into turmoil.",
    (39 to 40) -> "You are the only survivor of a deadly accident on board a ship or small hab, which raises some suspicion ...",
    (41 to 42) -> "Your life has been blissfully serene and untroubled. Your friends may secretly hate you.",
    (43 to 44) -> "You find out that one or both of your parents weren’t really your parents.",
    (45 to 46) -> "You pursue a period of self-isolation and introspection.",
    (47 to 48) -> "You catch an authority figure doing something illicit, but you don’t have the means to prove it.",
    (49 to 50) -> "You take a sabbatical with the Solarians, ringers, or other space-faring clade.",
    (51 to 52) -> "You have an affair.",
    (53 to 54) -> "You are privileged enough to meet a Factor.",
    (55 to 56) -> "You discover an unknown and intriguing or devastating secret about your family’s past.",
    (57 to 58) -> "An unfortunate accident leaves you stuck in a healing vat for a couple of weeks.",
    (59 to 60) -> "While traveling by spacecraft, a malfunction takes you months off course.",
    (61 to 62) -> "You use someone to get ahead.",
    (63 to 64) -> "Someone uses you to get ahead.",
    (65 to 66) -> "You unexpectedly make a close friend with someone from a rival or even hostile faction.",
    (67 to 68) -> "You have a falling out with a formerly close friend.",
    (69 to 70) -> "You are forced into a thankless position of heavy responsibility.",
    (71 to 72) -> "Facing unwanted responsibilities, you pack up and move on.",
    (73 to 74) -> "You are persecuted for your nature or beliefs.",
    (75 to 76) -> "You finalize a particularly good research paper, work of art, commercial enterprise, or similar achievement.",
    (77 to 78) -> "Someone close to you opts for a real, final death.",
    (79 to 80) -> "You discover a new subculture to embed yourself in.",
    (81 to 82) -> "You befriend a brinker with some interesting ideas and unbelievable stories. Well, almost unbelievable.",
    (83 to 84) -> "You find repeat evidence that someone has you under close surveillance—but why?",
    (85 to 86) -> "You are fairly certain that your new friend is secretly a singularity seeker.",
    (87 to 88) -> "You come across an interesting surveillance blind-spot in your local habitat.",
    (89 to 90) -> "A string of disappearances in your habitat has everyone on edge.",
    (91 to 92) -> "Someone you know has come across some disturbing information on a powerful entity, and they are considering blowing the whistle.",
    (93 to 94) -> "You don’t have what it takes, and your current job/prospect ends in a washout.",
    (95 to 96) -> "Your inquisitive nature leads you to discover a secret that could get you into trouble.",
    (97 to 98) -> "You receive a wake-up call that challenges your current priorities.",
    (99 to 100) -> "Your current job/pursuits take you somewhere dangerous."
  );

  override def label: String = "Story Event";
  override def source: String = "Transhuman p.73";
  override def roll(rand: Random): Result = {
    data.randomElement(rand).get
  }
  override def numRows: Int = 100;
  override def pick(i: Int): Result = data.get(i).get;
}
