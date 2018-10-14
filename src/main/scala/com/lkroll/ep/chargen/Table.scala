package com.lkroll.ep.chargen

trait Table {
  type Result;

  def label: String;
  def source: String;
  def roll(rand: Random): Result;

  def roll(): Result = roll(new Random());
}

trait Pickable extends Table {
  def numRows: Int;
  def pick(i: Int): Result;
}
