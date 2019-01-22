#!/bin/bash
java -Dconfig.file=./application.conf -jar "target/scala-2.12/EP Character Generator-assembly-1.1.0-SNAPSHOT.jar" $@ 1>&2

