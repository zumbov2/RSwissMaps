[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/RSwissMaps)](https://cran.r-project.org/package=RSwissMaps)
[![Build Status](https://travis-ci.org/zumbov2/RSwissMaps.svg?branch=master)](https://travis-ci.org/zumbov2/RSwissMaps)
[![Licence](https://img.shields.io/badge/licence-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)
# RSwissMaps

RSwissMaps allows to link thematic data to Swiss administrative divisions (municipalities, districts, cantons) and to plot it on a map. The maps can be customised (to some degree) and saved in different resolutions and formats. The package also allows to generate tailored templates for data collection. The geodata used is publicly available on the [Swiss Federal Statistical Office website](https://www.bfs.admin.ch/bfs/de/home/dienstleistungen/geostat/geodaten-bundesstatistik/administrative-grenzen/generalisierte-gemeindegrenzen.html). 

## Data availability
Geodata of Swiss municipalities, districts, and cantons is currently available for the following reference dates: 2001-1-1, 2002-1-1, 2003-1-1, 2004-1-1, 2005-1-1, 2006-1-1, 2007-1-1, 2008-1-1, 2009-1-1, 2010-1-1, 2010-31-12, 2011-1-1, 2011-31-12, 2012-1-1, 2012-31-12, 2013-1-1, 2013-31-12, 2014-1-1, 2014-31-12, 2015-1-1, 2015-31-12, 2016-1-1, 2016-31-12, 2017-1-1.

## Coordinate reference system
CH1903/LV03 (EPSG:21781). For more information, please see: [swisstopo](https://www.swisstopo.admin.ch/en/knowledge-facts/surveying-geodesy/reference-systems/switzerland.html) and [Spatial Reference](http://spatialreference.org/ref/epsg/21781/) 

## Commercial use
For commercial use of the data, please see: [GEOSTAT, BFS](https://www.bfs.admin.ch/bfs/de/home/dienstleistungen/geostat/nutzungsbedingungen.html)

# Installation
The version 0.1.0 is on CRAN, and you can install it by:
```
install.packages("RSwissMaps")
```
For regularly updated version (latest: 0.1.3) with all geodata pre-installed, install from GitHub:
```
install.packages("devtools")
devtools::install_github("zumbov2/RSwissMaps", subdir = "allinone")
```

# Examples
## Example 1 (with code)
![municipalities](https://github.com/zumbov2/RSwissMaps/blob/master/plots/mun_plot.png)
```
gemeinden <- mun.template(2017)

for(i in 1:nrow(gemeinden)){
  gemeinden$values[i] <- sample(c(300:700), 1)/1000
}

mun.plot(gemeinden$bfs_nr, gemeinden$values, 2017,
         color_continuous = c("#c7e9c0", "#006d2c"),
         boundaries_size = 0.2,
         title = "Random data",
         subtitle = "Schweizer Gemeiden, 2017",
         caption = "Plotted with RSwissMaps",
         save = T,
         dpi = 1000)
```

## Example 2 (with code)
![municipalities2](https://github.com/zumbov2/RSwissMaps/blob/master/plots/mun_plot2.png)
```
gemeinden <- mun.template(2017, cantons = c("AG", "ZH"))

for(i in 1:nrow(gemeinden)){
  
  gemeinden$values[i] <- sample(c(300:700), 1)/1000
  
}

mun.plot(gemeinden$bfs_nr, gemeinden$values, 2017,
         cantons = c("AG", "ZH"),
         lakes = c("Hallwilersee", "Zürichsee", "Greifensee"),
         boundaries = c("m", "c"), boundaries_color =  c("white", "white"),
         boundaries_size = c(0.2, 1),
         title = "Random data",
         subtitle = "Aargauer und Zürcher Gemeiden, 2017",
         caption = "Plotted with RSwissMaps",
         save = T,
         filename = "mun_plot2.png",
         dpi = 1000)
```

## Example 3 (real data)
![districts](https://github.com/zumbov2/RSwissMaps/blob/master/plots/dis_plot.png)

## Example 4 (real data)
![cantons](https://github.com/zumbov2/RSwissMaps/blob/master/plots/can_plot.png)
