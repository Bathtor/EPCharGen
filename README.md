EP Character Generator
======================

This tool uses tables from [Transhuman](http://eclipsephase.com/releases/transhuman), among other things, to automatically generate Eclipse Phase characters.


Requirements
-------

- Java 8 or newer
- An API key from [Behind the Name](https://www.behindthename.com/api/)
- A local published build of the [EP Compendium](https://github.com/Bathtor/EPCompendium)

Building
-------
Run `sbt assembly`.

Running
-------
Before you can run the tool, you need a file called `application.conf` in the same folder as `run.sh`. In this file you need to specify your Behind the Name API key like `chargen.behindthename.api-key = "<API KEY HERE>"`.

Then simply call `./run.sh --help` from bash to see the available options.


License
-------
The material is based on *Eclipse Phase* by (Posthuman Studios)[http://eclipsephase.com] and is published under Creative Commons (BY-NC-SA) 3.0 (license)[https://creativecommons.org/licenses/by-nc-sa/3.0/] as is the original material.