

R Package for Downloading and Handling DWD MOSMIX Forecasts
===========================================================

The german weather service (DWD) publishes a set of automated
weather forecasts (MOS forecasts) as publicly available data
sets.

* [DWD open data server](https://opendata.dwd.de)
* [DWD copyright information](https://www.dwd.de/copyright)
* [DWD MOSMIX product information](https://www.dwd.de/EN/ourservices/met_application_mosmix/met_application_mosmix.html)

This small working package allows to easily download and decode
these forecasts. The forecasts become available several times
a day, XML format. The `mosmix` R package provides the following
features:

* Check files available on the server
* Automatically download and parse forecasts for a set of user-defined
  locations
* Returns convenient time series objects
* Write (subsetted) data sets to local ASCII files

Allows to download _MOSMIX-L_ (4x daily, 115 parameters, up to 240 hours ahead)
and _MOSMIX-S_ (24x daily, 40 parameters, up to 240 hours ahead).

For the decoding of the data sets this package depends on the
[XML](https://cran.r-project.org/package=XML) package.
Plesae note that, to be able to install the R package, `libxml` has to
be installed. The forecasts are time series data wherefore this package also
depends on the R package [zoo](https://cran.r-project.org/package=zoo).


Package Status
==============

Please note that this is a **working package** and was written in less
then a day! Should be seen as an early beta version. Feel free to
use, reuse, improve and contribute!

All best,

_Reto_

Package Installation
====================

`mosmix` can be installed using the _R_ package `githubinstall` (or `devtools`,
or simply clone and install locally).

## with devtools via https (`devtools::install_github`)

Currently the package is only available via [github](https://github.com).
One convenient way to install packages from [github](https://github.com) is
is via `devtools` (requires _R_ package `devtools` to be installed; if not
use `install.packages("devtools")`).

The following snippet installes the latest _development_ version of the
mosmix package from [github.com/retostauffer/Rmosmix](https://github.com/retostauffer/Rmosmix):

```
# Load devtools
library("devtools")

# Install package, should also resolve the dependencies
install_github("retostauffer/Rmosmix")
```

**Note**: If you do not like experimental versions feel free to install one
of our release candidates, e.g., `v0.0-2`, an early alpha release from
December 2018. To install a release candidate provide the additional `ref`
argument. A list of available
[release candidates](https://github.com/retostauffer/Rmosmix/releases)
can be found on the [github release page](https://github.com/retostauffer/Rmosmix/releases).

```
# Load devtools
library("devtools")

# Install package, should also resolve the dependencies
devtools::install_github("retostauffer/Rmosmix", ref = "v0.0-2")
```

## devtools by cloning the repository

You can also clone the repository and install the package based
on the checkout. For linux users:

```
# Change directory
cd <somewhere/on/your/local/disc>

# Clone repository
git clone https://github.com/retostauffer/Rmosmix.git

# Start R
R
```

As soon as you are in the interactive _R_ shell:

```
# Loading the devtools library
library("devtools")

# Install package
devtools::install("Rmosmix")
```

## Via console

Or of course via console:

```
# Change directory
cd <somewhere/on/your/local/disc>

# Clone repository
git clone https://github.com/retostauffer/Rmosmix.git

# Feel free to use on of our release candidates which
# might miss some features, but might also be more stable.
# A list of release candidates can be found on:
# - https://github.com/retostauffer/Rmosmix/releases
(cd Rmosmix && git checkout tags/<tagname> && cd ..)

# Install directly
R CMD INSTALL Rmosmix

# OR
R CMD build Rmosmix
R CMD INSTALL mosmix_<version>.tar.gz
```


