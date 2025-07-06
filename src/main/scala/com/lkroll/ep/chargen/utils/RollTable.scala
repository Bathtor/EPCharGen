package com.lkroll.ep.chargen.utils

import com.lkroll.ep.chargen.Random

trait RollTableLike[V] {
  def randomElement(rand: Random): Option[V];
  def entries: Iterable[V];
}

object Dice {
  val `1d10` = Die(1, 10);
  val `1d100` = Die(1, 100);
}

case class Die(num: Int, size: Int) extends RollTableLike[Int] {
  override def randomElement(rand: Random): Option[Int] = {
    val rolls = (1 to num).map(_ => rand.nextInt(size) + 1);
    Some(rolls.sum)
  }
  override def entries: Iterable[Int] = {
    (num to (num * size))
  }

  def *(i: Int): DieExpression[Int] = DieExpression(this, _ * i);
  def +(i: Int): DieExpression[Int] = DieExpression(this, _ + i);
  def expr[V](f: Int => V): DieExpression[V] = DieExpression(this, f);
}

case class DieExpression[V](d: Die, f: Int => V) extends RollTableLike[V] {
  def randomElement(rand: Random): Option[V] = {
    d.randomElement(rand).map(f)
  }
  def entries: Iterable[V] = d.entries.map(f);

  def *(i: Int)(implicit ev: V =:= Int): DieExpression[Int] = DieExpression(d, res => f(res) * i);
  def +(i: Int)(implicit ev: V =:= Int): DieExpression[Int] = DieExpression(d, res => f(res) + i);
}

case class RollConstant[V](v: V) extends RollTableLike[V] {
  override def randomElement(rand: Random): Option[V] = Some(v);
  override def entries: Iterable[V] = List(v);
}

case class RollSubTables[V](tables: RollTable[RollTableLike[V]]) extends RollTableLike[V] {
  override def randomElement(rand: Random): Option[V] = {
    tables.randomElement(rand).flatMap { t =>
      t.randomElement(rand)
    }
  }
  override def entries: Iterable[V] = {
    tables.entries.foldLeft(List.empty[V]) { (acc, t) =>
      t.entries.toList ::: acc
    }
  }
}

class RollTable[V] extends RollTableLike[V] {
  import scala.jdk.CollectionConverters._

  private val jmap = new java.util.TreeMap[Int, V];
  private var ceiling = 0;

  override def randomElement(rand: Random): Option[V] = {
    if (jmap.isEmpty()) {
      None
    } else {
      val roll = rand.nextInt(ceiling) + 1;
      val entry = jmap.floorEntry(roll);
      if (entry != null) {
        Some(entry.getValue)
      } else {
        Some(jmap.firstEntry().getValue)
      }
    }
  }

  override def entries: Iterable[V] = {
    jmap.values().asScala
  }

  def apply(index: Int): V = get(index).get;

  def get(index: Int): Option[V] = {
    if (jmap.isEmpty()) {
      None
    } else {
      val entry = jmap.floorEntry(index);
      if (entry != null) {
        Some(entry.getValue)
      } else {
        Some(jmap.firstEntry().getValue)
      }
    }
  }
}

object RollTable {
  def apply[V](data: (Range, V)*): RollTable[V] = {
    val table = new RollTable[V]();
    var ceil = 0;
    val sorted = data.sortBy(_._1.start);
    var prevRange: Range = (-1 to 0);
    data.foreach {
      case (r, v) => {
        require(prevRange.end < r.start, "RollTable ranges should not overlap!");
        table.jmap.put(r.start, v);
        ceil = Math.max(ceil, r.end);
        prevRange = r;
      }
    }
    table.ceiling = ceil;
    table
  }
}
