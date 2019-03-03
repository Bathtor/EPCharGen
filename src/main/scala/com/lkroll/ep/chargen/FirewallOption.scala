package com.lkroll.ep.chargen

sealed trait FirewallOption;
object FirewallOption {
  case object Skip extends FirewallOption;
  case object Allow extends FirewallOption;
  case object Always extends FirewallOption;
}
