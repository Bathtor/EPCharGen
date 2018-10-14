package com.lkroll.ep.chargen.character

import com.lkroll.common.macros.Macros
import com.lkroll.ep.compendium.{ Aptitude, AptitudeValues }
import com.lkroll.ep.chargen.rendering.Renderer
import com.lkroll.ep.chargen._
import com.lkroll.ep.chargen.creationpackages.PackageContent

case class Skill(skillDef: Skills.Skill, ranks: Int, specs: List[String] = Nil) {
  import Implicits._;

  def name: String = skillDef.name;
  def field: Option[String] = skillDef.field;
  def apt: Aptitude = skillDef.apt;

  def withField(s: String): Skill = this.copy(skillDef = skillDef.copy(field = Some(s)));

  def render(renderer: Renderer, apts: AptitudeValues): Unit = {
    val total = apts.valueFor(apt) + ranks;
    renderer.value(name);
    field.foreach { f =>
      renderer.field(f);
    }
    renderer.raw(" (");
    renderer.value(apt.label);
    renderer.raw(") ");
    renderer.labelled("Total", total.toString);
    renderer.raw(" (");
    renderer.labelled("Ranks", ranks.toString);
    renderer.raw(") - ");
    renderer.value(specs.mkString(", "));
  }

  def render(renderer: Renderer): Unit = {
    renderer.value(name);
    field.foreach { f =>
      renderer.field(f);
    }
    renderer.raw(" ");
    renderer.labelled("Total", s"$ranks + ${apt.label}");
    renderer.raw(" - ");
    renderer.value(specs.mkString(", "));
  }
}

sealed trait SkillFilter {
  def matches(skill: Skills.Skill): Boolean;
  def matches(skill: Skill): Boolean = matches(skill.skillDef);
  def render: String;
}
object SkillFilter {
  case class SkillClass(cls: Skills.SkillClass.SkillClass) extends SkillFilter {
    override def matches(skill: Skills.Skill): Boolean = skill.cls == cls;
    override def render: String = cls.toString;
  }
  case class Category(cat: Skills.SkillCategory.SkillCategory) extends SkillFilter {
    override def matches(skill: Skills.Skill): Boolean = skill.category == cat;
    override def render: String = cat.toString;
  }
  case class Name(name: String) extends SkillFilter {
    override def matches(skill: Skills.Skill): Boolean = skill.name.equalsIgnoreCase(name);
    override def render: String = name;
  }
  case class Not(filter: SkillFilter) extends SkillFilter {
    override def matches(skill: Skills.Skill): Boolean = !filter.matches(skill);
    override def render: String = s"not ${filter.render}";
  }
}

object Skills {
  import Implicits._;

  object SkillClass extends Enumeration {
    type SkillClass = Value;

    val Active, Knowledge = Value;
  }

  object SkillCategory extends Enumeration {
    type SkillCategory = Value;

    val Combat, Mental, Physical, Psi, Social, Technical, Vehicle, NA = Value;
  }

  def chooseAny(ranks: Int): PackageContent.Skill = PackageContent.Skill(Right(PackageContent.SkillChoice.PickAny), Right(PackageContent.SkillChoice.PickAny), ranks);
  def chooseOnly(ranks: Int, filters: SkillFilter*): PackageContent.Skill = PackageContent.Skill(Right(PackageContent.SkillChoice.PickOnly(filters.toList)), Right(PackageContent.SkillChoice.PickAny), ranks);
  def modAny(rand: Random, mod: Int): CharacterMod.SkillMod = CharacterMod.SkillMod(Right(CharacterMod.SkillChoice.PickAny(rand)), Right(CharacterMod.SkillChoice.PickAny(rand)), mod);
  def modOnly(rand: Random, mod: Int, filters: SkillFilter*): CharacterMod.SkillMod = CharacterMod.SkillMod(Right(CharacterMod.SkillChoice.PickOnly(rand, filters.toList)), Right(CharacterMod.SkillChoice.PickAny(rand)), mod);
  def oneOf(skills: Skill*): PackageContent.SkillChoice.OneOf = PackageContent.SkillChoice.OneOf(skills.map(_.name).toList);
  def specializeAny(rand: Random): CharacterMod.SkillMod = CharacterMod.SkillMod(Right(CharacterMod.SkillChoice.PickAny(rand)), Right(CharacterMod.SkillChoice.PickAny(rand)), 0, Some(Right(CharacterMod.SkillChoice.PickAny(rand))));

  case class Skill(
    name:         String,
    field:        Option[String],
    cls:          SkillClass.SkillClass,
    category:     SkillCategory.SkillCategory,
    apt:          Aptitude,
    noDefaulting: Boolean                     = false,
    sampleFields: Option[Array[String]]       = None,
    sampleSpecs:  Array[String]               = Array("Pick-A-Spec")) {

    def instance(ranks: Int = 0): com.lkroll.ep.chargen.character.Skill = {
      com.lkroll.ep.chargen.character.Skill(this, ranks)
    }
    def withField(s: String): Skill = this.copy(field = Some(s));
    def randomField(rand: Random): Option[Skill] = {
      for {
        samples <- sampleFields;
        field <- samples.randomElement(rand)
      } yield this.withField(field)
    }
    def +(mod: Int): CharacterMod.SkillMod = CharacterMod.SkillMod(Left(name), Left(field), mod);
    def -(mod: Int): CharacterMod.SkillMod = CharacterMod.SkillMod(Left(name), Left(field), -mod);
    def modAnyField(rand: Random, mod: Int): CharacterMod.SkillMod = CharacterMod.SkillMod(Left(name), Right(CharacterMod.SkillChoice.PickAny(rand)), mod);
    def gainSpecialization(spec: String): CharacterMod.SkillMod = CharacterMod.SkillMod(Left(name), Left(field), 0, Some(Left(spec)));
    def anySpecialization(rand: Random): CharacterMod.SkillMod = CharacterMod.SkillMod(Left(name), Left(field), 0, Some(Right(CharacterMod.SkillChoice.PickAny(rand))));

    def at(ranks: Int): PackageContent.Skill = PackageContent.Skill(Left(name), Left(field), ranks);
    def anyField(ranks: Int): PackageContent.Skill = PackageContent.Skill(Left(name), Right(PackageContent.SkillChoice.PickAny), ranks);
    def oneOf(fields: String*): PackageContent.Skill = PackageContent.Skill(Left(name), Right(PackageContent.SkillChoice.OneOf(fields.toList)), 0);
  }

  private def strArray(s: String): Array[String] = s.split(",").map(_.trim());

  object Defaults {
    import Aptitude._;
    import SkillClass._;
    import SkillCategory._;

    val academics = Skill("Academics", Some("???"), Knowledge, NA, COG,
      sampleFields = Some(strArray("Archeology, Astrobiology, Astronomy, Astrophysics, Astrosociology, Biochemistry, Biology, Botany, Computer Science,Cryptography, Economics, Engineering, Genetics, Geology, Linguistics, Mathematics, Memetics, Nanotechnology, Old Earth History, Physics, Political Science, Psychology, Sociology, Xeno-archeology, Xenolinguistics, Zoology")));
    val animalHandling = Skill("Animal Handling", None, Active, Social, SAV,
      sampleSpecs = Array("smart dogs, smart monkeys, horses, smart rats"));
    val art = Skill("Art", Some("???"), Knowledge, NA, INT,
      sampleFields = Some(strArray("Architecture, Criticism, Dance, Drama, Drawing, Painting, Performance, Sculpture, Simulspace Design, Singing, Speech, Writing")));
    val beamWeapons = Skill("Beam Weapons", None, Active, Combat, COO,
      sampleSpecs = strArray("Lasers, Microwave Weapons, Particle Beam Weapons, Plasma Rifles"));
    val blades = Skill("Blades", None, Active, Combat, SOM,
      sampleSpecs = strArray("Axes, Implant Blades, Knives, Swords"));
    val climbing = Skill("Climbing", None, Active, Physical, SOM,
      sampleSpecs = strArray("Assisted, Freehand, Rappelling"));
    val clubs = Skill("Clubs", None, Active, Combat, SOM,
      sampleSpecs = strArray("Batons, Hammers, Staffs"));
    val control = Skill("Control", None, Active, Psi, WIL, true);
    val deception = Skill("Deception", None, Active, Social, SAV,
      sampleSpecs = strArray("Acting, Bluffing, Fast Talk"));
    val demolitions = Skill("Demolitions", None, Active, Technical, COG, true,
      sampleSpecs = strArray("Commercial Explosives, Disarming, Improvised Explosives"));
    val disguise = Skill("Disguise", None, Active, Physical, INT,
      sampleSpecs = Array("Cosmetic", "Theatrical"));
    val exoticMeleeWeapon = Skill("Exotic Melee Weapon", Some("???"), Active, Combat, SOM,
      sampleFields = Some(strArray("Morning Star, Spear, Whip")));
    val exoticRangedWeapon = Skill("Exotic Ranged Weapon", Some("???"), Active, Combat, COO,
      sampleFields = Some(strArray("Blowgun, Crossbow, Slingshot")));
    val flight = Skill("Flight", None, Active, Physical, SOM,
      sampleSpecs = Array("Diving", "Landing", "Takeoff"));
    val fray = Skill("Fray", None, Active, Combat, REF,
      sampleSpecs = strArray("Blades, Clubs, Full Defense, Unarmed"));
    val freeFall = Skill("Free Fall", None, Active, Physical, REF,
      sampleSpecs = Array("Microgravity", "Parachuting", "Vacsuits"));
    val freerunning = Skill("Freerunning", None, Active, Physical, SOM,
      sampleSpecs = strArray("Balance, Gymnastics, Jumping, Running"));
    val gunnery = Skill("Gunnery", None, Active, Combat, INT,
      sampleSpecs = Array("Artillery", "Missiles"));
    val hardware = Skill("Hardware", Some("???"), Active, Technical, COG,
      sampleFields = Some(strArray("Aerospace, Armorer, Electronics, Groundcraft, Implants, Industrial, Nautical, Robotics")));
    val impersonation = Skill("Impersonation", None, Active, Social, SAV,
      sampleSpecs = Array("Avatar", "Face-to-Face", "Verbal"));
    val infiltration = Skill("Infiltration", None, Active, Physical, COO,
      sampleSpecs = Array("Blending In", "Hiding", "Shadowing", "Sneaking"));
    val infosec = Skill("Infosec", None, Active, Technical, COG, true,
      sampleSpecs = strArray("Brute-Force Hacking, Decryption, Probing, Security, Sniffing, Spoofing"));
    val interest = Skill("Interest", Some("???"), Knowledge, NA, COG,
      sampleFields = Some(strArray("Ancient Sports, Celebrity Gossip, Conspiracies, Factor Trivia, Gambling, Hypercorp Politics, Lunar Habitats, Martian Beers, Old Earth Nation-States, Reclaimer Blogs, Science Fiction, Scum Drug Dealers, Spaceship Models, Triad Economics, Underground XP")));
    val interfacing = Skill("Interfacing", None, Active, Technical, COG,
      sampleSpecs = Array("Forgery", "Scanning", "Stealthing"));
    val intimidation = Skill("Intimidation", None, Active, Social, SAV,
      sampleSpecs = Array("Interrogation", "Physical", "Verbal"));
    val investigation = Skill("Investigation", None, Active, Mental, INT,
      sampleSpecs = strArray("Evidence Analysis, Logical Deductions, Physical Investigation, Physical Tracking"));
    val kinesics = Skill("Kinesics", None, Active, Social, SAV,
      sampleSpecs = Array("Judge Intent", "Nonvocal Communication"));
    val kineticWeapons = Skill("Kinetic Weapons", None, Active, Combat, COO,
      sampleSpecs = strArray("Assault Rifles, Machine Guns, Pistols, Sniper Rifles, Submachine Guns"));
    val language = Skill("Language", Some("???"), Knowledge, NA, INT,
      sampleFields = Some(Languages.list.map(_.name).toArray));
    val medicine = Skill("Medicine", Some("???"), Active, Technical, COG,
      sampleFields = Some(strArray("Biosculpting, Exotic Biomorphs, Gene Therapy, General Practice, Implant Surgery, Nanomedicine, Paramedic, Pods, Psychiatry, Remote Surgery, Trauma Surgery, Uplifts, Veterinary")));
    val navigation = Skill("Navigation", None, Active, Mental, INT,
      sampleSpecs = Array("Astrogation", "Map Making", "Map Reading"));
    val networking = Skill("Networking", Some("???"), Active, Social, SAV,
      sampleFields = Some(Array("Autonomists", "Criminals", "Ecologists", "Firewall", "Hypercorps", "Media", "Scientists", "Ultimates", "Gatecrashers")));
    //    val networkingAutonomists = Skill("Networking", Some("Autonomists"), Active, Social, SAV);
    //    val networkingCriminals = Skill("Networking", Some("Criminals"), Active, Social, SAV);
    //    val networkingEcologists = Skill("Networking", Some("Ecologists"), Active, Social, SAV);
    //    val networkingFirewall = Skill("Networking", Some("Firewall"), Active, Social, SAV);
    //    val networkingHypercorps = Skill("Networking", Some("Hypercorps"), Active, Social, SAV);
    //    val networkingMedia = Skill("Networking", Some("Media"), Active, Social, SAV);
    //    val networkingScientists = Skill("Networking", Some("Scientists"), Active, Social, SAV);
    //    val networkingUltimates = Skill("Networking", Some("Ultimates"), Active, Social, SAV);
    //    val networkingGatecrashers = Skill("Networking", Some("Gatecrashers"), Active, Social, SAV);
    val palming = Skill("Palming", None, Active, Physical, COO,
      sampleSpecs = Array("Pickpocketing", "Shoplifting", "Tricks"));
    val perception = Skill("Perception", None, Active, Mental, INT,
      sampleSpecs = strArray("Aural, Olfactory, Tactile, Taste, Visual"));
    val persuasion = Skill("Persuasion", None, Active, Social, SAV,
      sampleSpecs = strArray("Diplomacy, Morale Boosting, Negotiating, Seduction"));
    val pilot = Skill("Pilot", Some("???"), Active, Vehicle, REF,
      sampleFields = Some(strArray("Aircraft, Anthroform, Exotic Vehicle, Groundcraft, Spacecraft, Watercraft")));
    val profession = Skill("Profession", Some("???"), Knowledge, NA, COG,
      sampleFields = Some(strArray("Accounting, Appraisal, Asteroid Prospecting, Banking, Cool Hunting, Con Schemes, Distribution, Forensics, Lab Technician, Mining, Police Procedures, Psychotherapy, Security Ops, Smuggling Tricks, Social Engineering, Squad Tactics, Viral Marketing, XP Production")));
    val programming = Skill("Programming", None, Active, Technical, COG, true);
    val protocol = Skill("Protocol", None, Active, Social, SAV,
      sampleSpecs = strArray("Anarchist, Brinker, Criminal, Factor, Hypercorp, Infomorph, Mercurial, Reclaimer, Preservationist, Scum, Ultimate"));
    val psiAssault = Skill("Psi Assault", None, Active, Psi, WIL, true);
    val psychosurgery = Skill("Psychosurgery", None, Active, Technical, INT,
      sampleSpecs = Array("Memory Manipulation", "Personality Editing", "Psychotherapy"));
    val research = Skill("Research", None, Active, Technical, COG,
      sampleSpecs = Array("Tracking"));
    val scrounging = Skill("Scrounging", None, Active, Mental, INT,
      sampleSpecs = strArray("Bazaars, Forests, Habitats, Ruins"));
    val seekerWeapons = Skill("Seeker Weapons", None, Active, Combat, COO,
      sampleSpecs = strArray("Armband, Pistol, Rifle, Underbarrel"));
    val sense = Skill("Sense", None, Active, Psi, INT, true);
    val sprayWeapons = Skill("Spray Weapons", None, Active, Combat, COO,
      sampleSpecs = strArray("Buzzer, Freezer, Shard, Shredder, Torch"));
    val swimming = Skill("Swimming", None, Active, Physical, SOM,
      sampleSpecs = Array("Diving", "Freestyle", "Underwater Diving"));
    val throwingWeapons = Skill("Throwing Weapons", None, Active, Combat, COO,
      sampleSpecs = Array("Grenades", "Knives", "Rocks"));
    val unarmedCombat = Skill("Unarmed Combat", None, Active, Combat, SOM,
      sampleSpecs = Array("Implant Weaponry", "Kick", "Punch", "Subdual"));

    val list: List[Skill] = Macros.memberList[Skill];
  }
}
