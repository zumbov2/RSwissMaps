## ----installGithub, eval=FALSE-------------------------------------------
#  install.packages("devtools")
#  devtools::install_github("zumbov2/RSwissMaps", subdir = "allinone")

## ---- eval=FALSE---------------------------------------------------------
#  install.packages("RSwissMaps")

## ------------------------------------------------------------------------
library(RSwissMaps)

## ------------------------------------------------------------------------
dt <- mun.template(2016)
for(i in 1:nrow(dt)){dt$values[i] <- sample(c(400:650), 1)/1000}

## ------------------------------------------------------------------------
mun.plot(dt$bfs_nr, dt$values, 2016, 
         boundaries = "") # boundaries are switched off due to poor resolution   

## ------------------------------------------------------------------------
mun.plot(dt$bfs_nr, dt$values, 2016,
         cantons = c("AG", "ZH"), # cantons of Aargau and Zurich
         lakes = c(9040, 9050, 9172), # corresponding lake IDs
         boundaries = "c", # display canton boundaries
         boundaries_size = 1,
         color_continuous = c("#c7e9c0", "#006d2c"), # some green
         title = "Title 1", 
         subtitle = "Subtitle 1", 
         caption = "Caption 1", 
         legend_title = "share")

## ------------------------------------------------------------------------
dt2 <- dis.template(2016)
for(i in 1:nrow(dt2)){dt2$values[i] <- sample(c(200:400), 1)/1000}

## ------------------------------------------------------------------------
dis.plot(dt2$bfs_nr, dt2$values, 2016, 
         boundaries = "") # boundaries are switched off due to poor resolution  

## ------------------------------------------------------------------------
dis.plot(dt2$bfs_nr, dt2$values, 2016,
         cantons = c("BE"), # canton of Bern
         lakes = c("Bielersee", "Brienzersee", "Thunersee"), # corresponding lakes
         color_continuous = c("#f0f0f0", "#252525"), # black/white
         title = "title goes here", 
         subtitle = "subtitle goes here", 
         caption = "caption goes here", 
         legend_title = "share")

## ------------------------------------------------------------------------
dt3 <- can.template(2016)
for(i in 1:nrow(dt3)){dt3$values[i] <- sample(c(450:700), 1)/1000}

## ------------------------------------------------------------------------
can.plot(dt3$bfs_nr, dt3$values, 2016)

## ------------------------------------------------------------------------
can.plot(dt3$bfs_nr, dt3$values, 2016,
         color_continuous = c("#bcbddc", "#54278f"), # purple
         title = "title goes here", 
         subtitle = "subtitle goes here", 
         caption = "caption goes here", 
         legend_title = "share")

