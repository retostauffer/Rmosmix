
TODO
----

* Loading all nodes is memory inefficient, try to find a way
  to loop along an xpath.
* Colors for the plotting function.
* Documentation.
* Testing.

Version 0.1
-----------

I've just set up a small R package for downloading handling
and accessing DWD MOSMOX forecasts. Current features:

* Selective download of MOSMIX-L and MOSMIX-S forecasts for
   user-defined set of locations/stations (see details below)
* `get_files_avail` checks the available files on the server.
* `tests/automation.R` contains a small script which might can
   be used for automation. Will, at some point, be put into
   an automation function for handy use. The script
   is checking available files on the server and only processes
   the files which have not yet been downloaded, parsed, and
   stored. This script can be used fur an _archive script_.
   We use it to store some forecasts from the live rolling
   archive for later analysis.

**Some information about the data**: the German weather service
(DWD) provides some DWD MOSMIX forecasts (statistically corrected
forecasts based on the ICON/ECMWF numerical weather prediction model)
on their [opendata server](https://opendata.dwd.de). Two products
are available: 

* MOSMIX-L: produced 4 times a day up to 240 hours ahead, provides
  up to 115 parameters (location dependent).
* MOSMIX-S: produced every hour up to 240 hours ahead. Provides
  up to 40 parameters (location dependent).

MOSMIX-L forecasts are available as individual files, one file
per station. Thus, downloading these forecasts is rather efficient.
For MOSMIX-S one single file is available on the server containing
all locations (several thousand different locations). To be able
to extract one station the whole file has to be downloaded. Thus, the
MOSMIX-M takes much longer to process and needs some more bandwidth,
however, works like a charm!

If you use these data sets please read the DWD terms and conditions
regarding their open data sets! More information:


* [DWD open data server](https://opendata.dwd.de)
* [DWD copyright information](https://www.dwd.de/copyright)
* [DWD MOSMIX product information](https://www.dwd.de/EN/ourservices/met_application_mosmix/met_applicati
on_mosmix.html)


