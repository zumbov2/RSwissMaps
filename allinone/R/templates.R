# FUNCTION TO GENERATE MUNICIPALITY TEMPLATES
#' Generate templates for municipality-level data
#'
#' \code{mun.template} offers the possbility to generate Swiss municipality-level data templates in
#'     the form of data frames. The GitHub version of \code{RSwissMaps} comes with all available geodata
#'     (\code{2001-2017}). Hence, the \code{map.load} function is not part of this version. The templates
#'     contain the municipality identification numbers as used by the Swiss Federal Statistical Office
#'     and the municipality names.
#'
#' @param year a numeric value. Available with GitHub version of package: \code{2001-2017}.
#' @param endofyear if \code{FALSE}, municipalities as of January 1 are outputted. If \code{TRUE},
#'     municipalities as by December 31 are used. End-of-year data is available for \code{year > 2010}.
#' @param cantons a numeric (canton identification numbers) or a character (two-letter abbreviations)
#'     vector to create canton-specific templates.
#' @param districts a numeric (district identification numbers) or a character (district name)
#'     vector to create district-specific templates.
#' @param municipalities a numeric (municipality identification numbers) or a character
#'     (municipality names) vector to create tailor-made templates.
#' @examples
#' # Generating template for 2016:
#' mun.template(2016)
#'
#' # Generating template for the municipalities of the canton of Aargau:
#' mun.template(2016, cantons = c("AG"))
#' @export
mun.template <- function(year, endofyear = FALSE, cantons = NULL, districts = NULL, municipalities = NULL){

  if(!is.logical(endofyear)) stop("endofyear: TRUE or FALSE needed")
  if(year<2001|year>2017) stop("year: no data available")

  if(year == 2017) dt <- RSwissMaps::mapCH2017
  if(year == 2016) dt <- RSwissMaps::mapCH2016
  if(year == 2015) dt <- RSwissMaps::mapCH2015
  if(year == 2014) dt <- RSwissMaps::mapCH2014
  if(year == 2013) dt <- RSwissMaps::mapCH2013
  if(year == 2012) dt <- RSwissMaps::mapCH2012
  if(year == 2011) dt <- RSwissMaps::mapCH2011
  if(year == 2010) dt <- RSwissMaps::mapCH2010
  if(year == 2009) dt <- RSwissMaps::mapCH2009
  if(year == 2008) dt <- RSwissMaps::mapCH2008
  if(year == 2007) dt <- RSwissMaps::mapCH2007
  if(year == 2006) dt <- RSwissMaps::mapCH2006
  if(year == 2005) dt <- RSwissMaps::mapCH2005
  if(year == 2004) dt <- RSwissMaps::mapCH2004
  if(year == 2003) dt <- RSwissMaps::mapCH2003
  if(year == 2002) dt <- RSwissMaps::mapCH2002
  if(year == 2001) dt <- RSwissMaps::mapCH2001

  if(endofyear == TRUE){

    data_cantons <- dt[dt$year==year&dt$type=="canton"&dt$endofyear==1,]
    data_districts <- dt[dt$year==year&dt$type=="district"&dt$endofyear==1,]
    data_municipalities <- dt[dt$year==year&dt$type=="municipality"&dt$endofyear==1,]

    if(nrow(data_municipalities)==0){

      data_municipalities <- dt[dt$year==year&dt$type=="municipality"&dt$endofyear==0,]
      message(paste0("no data found for endofyear = TRUE; automatically applied endofyear = FALSE; map dates from 01.01.", year))

    }

    if(nrow(data_cantons)==0) data_cantons <- dt[dt$year==year&dt$type=="canton"&dt$endofyear==0,]
    if(nrow(data_districts)==0) data_districts <- dt[dt$year==year&dt$type=="district"&dt$endofyear==0,]


  } else {

    data_cantons <- dt[dt$year==year&dt$type=="canton"&dt$endofyear==0,]
    data_districts <- dt[dt$year==year&dt$type=="district"&dt$endofyear==0,]
    data_municipalities <- dt[dt$year==year&dt$type=="municipality"&dt$endofyear==0,]

  }

  # Apply CANTONS on data ------------------------
  dt_c <- NULL

  if(!missing(cantons)){

    cantons_name <- unique(as.character(data_cantons$name))
    cantons_id <- unique(as.numeric(data_cantons$id))

    if(!is.numeric(cantons)){

      # checking non-numeric input
      wrong_cantons <- setdiff(tolower(cantons), tolower(cantons_name))
      if(length(wrong_cantons) > 0) stop(paste0("unknown canton id: ", wrong_cantons, collapse = ", "))

      # building data
      for(i in 1:length(cantons_name)){
        if(length(intersect(tolower(cantons_name[i]), tolower(cantons))) > 0) dt_c <- rbind.data.frame(dt_c, data_municipalities[data_municipalities$can==cantons_id[i],])
      }

    }
    if(is.numeric(cantons)){

      # checking numeric input
      wrong_cantons <- setdiff(cantons, cantons_id)
      if(length(wrong_cantons) > 0) stop(paste0("unknown canton id: ", wrong_cantons, collapse = ", "))

      # building data
      for(i in 1:length(cantons_id)){
        if(length(intersect(cantons_id[i], cantons)) > 0) dt_c <- rbind.data.frame(dt_c, data_municipalities[data_municipalities$can==cantons_id[i],])

      }

    }

    rm(cantons_name, cantons_id, wrong_cantons, data_cantons)

  }

  # Apply DISTRICTS on data ------------------------
  dt_d <- NULL

  if(!missing(districts)){

    districts_name <- unique(as.character(data_districts$name))
    districts_id <- unique(as.numeric(data_districts$id))

    if(!is.numeric(districts)){

      # checking non-numeric input
      wrong_districts <- setdiff(tolower(districts), tolower(districts_name))
      if(length(wrong_districts) > 1) stop(paste0("unknown district: ", wrong_districts, collapse = ", "))

      # building data
      for(i in 1:length(districts_name)){
        if(length(intersect(tolower(districts_name[i]), tolower(districts))) > 0) dt_d <- rbind.data.frame(dt_d, data_municipalities[data_municipalities$dis==districts_id[i],])
      }

    }
    if(is.numeric(districts)){

      # checking numeric input
      wrong_districts <- setdiff(districts, districts_id)
      if(length(wrong_districts) > 1) stop(paste0("unknown district: ", wrong_districts, collapse = ", "))

      # building data
      for(i in 1:length(districts_id)){
        if(length(intersect(districts_id[i], districts)) > 0) dt_d <- rbind.data.frame(dt_d, data_municipalities[data_municipalities$dis==districts_id[i],])

      }

    }

    rm(districts_name, districts_id, wrong_districts, data_districts)

  }
  # Apply MUNICIPALITY on data ------------------------
  dt_m <- NULL

  if(!missing(municipalities)){

    municipalities_name <- unique(data_municipalities$name)
    municipalities_id <- unique(data_municipalities$id)

    if(!is.numeric(municipalities)){

      # checking non-numeric input
      wrong_municipalities <- setdiff(tolower(municipalities), tolower(municipalities_name))
      if(length(wrong_municipalities) == 1) stop(paste0("unknown municipality: ", wrong_municipalities, collapse = ", "))

      # building data
      for(i in 1:length(municipalities_name)){
        if(length(intersect(tolower(municipalities_name[i]), tolower(municipalities))) > 0) dt_m <- rbind.data.frame(dt_m, data_municipalities[data_municipalities$id==municipalities_id[i],])
      }

    }
    if(is.numeric(municipalities)){

      # checking numeric input
      wrong_municipalities <- setdiff(municipalities, municipalities_id)
      if(length(wrong_municipalities) == 1) stop(paste0("unknown municipality: ", wrong_municipalities, collapse = ", "))

      # building data
      for(i in 1:length(municipalities_id)){
        if(length(intersect(municipalities_id[i], municipalities)) > 0) dt_m <- rbind.data.frame(dt_m, data_municipalities[data_municipalities$id==municipalities_id[i],])

      }

    }

    rm(municipalities_name, municipalities_id, wrong_municipalities, data_municipalities)

  }

  # Joining CANTONS, DISTRICTS and MUNICIPALITY ------------------------
  if(!is.null(dt_c) | !is.null(dt_d) | !is.null(dt_m)){

    bfs_nr <- c(dt_c$id, dt_d$id, dt_m$id)
    name <- c(as.character(dt_c$name), as.character(dt_d$name), as.character(dt_m$name))
    values <- NA
    dt <- cbind.data.frame(bfs_nr, name, values)
    dt <- dt[!duplicated(dt$bfs_nr),]

  } else {

    bfs_nr <- data_municipalities$id
    name <- as.character(data_municipalities$name)
    values <- NA
    dt <- cbind.data.frame(bfs_nr, name, values)
    dt <- dt[!duplicated(dt$bfs_nr),]

  }

  dt <- dt[order(dt$bfs_nr),]
  row.names(dt) <- NULL
  rm(dt_c, dt_d, dt_m, bfs_nr, name, values)

  dt$bfs_nr <- as.numeric(dt$bfs_nr)
  dt$name <- as.character(dt$name)
  dt$values <- as.numeric(dt$values)

  dt
}

# FUNCTION TO GENERATE DISTRICT TEMPLATES
#' Generate templates for district-level data
#'
#' \code{dis.template} offers the possbility to generate Swiss district-level data templates in
#'     the form of data frames. The GitHub version of \code{RSwissMaps} comes with all available geodata
#'     (\code{2001-2017}). Hence, the \code{map.load} function is not part of this version. The templates
#'     contain the district identification numbers as used by the Swiss Federal Statistical Office
#'     and the district names.
#'
#' @param year a numeric value. Available with GitHub version of package: \code{2001-2017}.
#' @param endofyear if \code{FALSE}, districts as of January 1 are outputted. If \code{TRUE},
#'     districts as by December 31 are used. End-of-year data is available for \code{year > 2010}.
#' @param cantons a numeric (canton identification numbers) or a character (two-letter abbreviations)
#'     vector to create canton-specific templates.
#' @param districts a numeric (districts identification numbers) or a character
#'     (districts names) vector to create tailor-made templates.
#' @examples
#' # Generating template for 2016:
#' dis.template(2016)
#'
#' # Generating template for the municipalities of the canton of Aargau:
#' dis.template(2016, cantons = c("AG"))
#' @export
dis.template <- function(year, endofyear = FALSE, cantons = NULL, districts = NULL){

  # Loading map data based on YEAR and ENDOFYEAR ------------------------
  if(!is.logical(endofyear)) stop("endofyear: TRUE or FALSE needed")
  if(year<2001|year>2017) stop("year: no data available")

  if(year == 2017) dt <- RSwissMaps::mapCH2017
  if(year == 2016) dt <- RSwissMaps::mapCH2016
  if(year == 2015) dt <- RSwissMaps::mapCH2015
  if(year == 2014) dt <- RSwissMaps::mapCH2014
  if(year == 2013) dt <- RSwissMaps::mapCH2013
  if(year == 2012) dt <- RSwissMaps::mapCH2012
  if(year == 2011) dt <- RSwissMaps::mapCH2011
  if(year == 2010) dt <- RSwissMaps::mapCH2010
  if(year == 2009) dt <- RSwissMaps::mapCH2009
  if(year == 2008) dt <- RSwissMaps::mapCH2008
  if(year == 2007) dt <- RSwissMaps::mapCH2007
  if(year == 2006) dt <- RSwissMaps::mapCH2006
  if(year == 2005) dt <- RSwissMaps::mapCH2005
  if(year == 2004) dt <- RSwissMaps::mapCH2004
  if(year == 2003) dt <- RSwissMaps::mapCH2003
  if(year == 2002) dt <- RSwissMaps::mapCH2002
  if(year == 2001) dt <- RSwissMaps::mapCH2001

  if(endofyear == TRUE){

    data_cantons <- dt[dt$year==year&dt$type=="canton"&dt$endofyear==1,]
    data_districts <- dt[dt$year==year&dt$type=="district"&dt$endofyear==1,]

    if(nrow(data_districts)==0){

      data_districts <- dt[dt$year==year&dt$type=="district"&dt$endofyear==0,]
      message(paste0("no data found for endofyear = TRUE; automatically applied endofyear = FALSE; map dates from 01.01.", year))

    }

    if(nrow(data_cantons)==0) data_cantons <- dt[dt$year==year&dt$type=="canton"&dt$endofyear==0,]

  } else {

    data_cantons <- dt[dt$year==year&dt$type=="canton"&dt$endofyear==0,]
    data_districts <- dt[dt$year==year&dt$type=="district"&dt$endofyear==0,]

  }

  # Apply CANTONS on data ------------------------
  dt_c <- NULL

  if(!missing(cantons)){

    cantons_name <- unique(data_cantons$name)
    cantons_id <- unique(data_cantons$id)

    if(!is.numeric(cantons)){

      # checking non-numeric input
      wrong_cantons <- setdiff(tolower(cantons), tolower(cantons_name))
      if(length(wrong_cantons) > 0) stop(paste0("unknown canton id: ", wrong_cantons, collapse = ", "))

      # building data
      for(i in 1:length(cantons_name)){
        if(length(intersect(tolower(cantons_name[i]), tolower(cantons))) > 0) dt_c <- rbind.data.frame(dt_c, data_districts[data_districts$can==cantons_id[i],])
      }

    }
    if(is.numeric(cantons)){

      # checking numeric input
      wrong_cantons <- setdiff(cantons, cantons_id)
      if(length(wrong_cantons) > 0) stop(paste0("unknown canton id: ", wrong_cantons, collapse = ", "))

      # building data
      for(i in 1:length(cantons_id)){
        if(length(intersect(cantons_id[i], cantons)) > 0) dt_c <- rbind.data.frame(dt_c, data_districts[data_districts$can==cantons_id[i],])

      }

    }

    rm(cantons_name, cantons_id, wrong_cantons, data_cantons)

  }

  # Apply DISTRICTS on data ------------------------
  dt_d <- NULL

  if(!missing(districts)){

    districts_name <- unique(data_districts$name)
    districts_id <- unique(data_districts$id)

    if(!is.numeric(districts)){

      # checking non-numeric input
      wrong_districts <- setdiff(tolower(districts), tolower(districts_name))
      if(length(wrong_districts) > 1) stop(paste0("unknown district: ", wrong_districts, collapse = ", "))

      # building data
      for(i in 1:length(districts_name)){
        if(length(intersect(tolower(districts_name[i]), tolower(districts))) > 0) dt_d <- rbind.data.frame(dt_d, data_districts[data_districts$id==districts_id[i],])
      }

    }
    if(is.numeric(districts)){

      # checking numeric input
      wrong_districts <- setdiff(districts, districts_id)
      if(length(wrong_districts) > 1) stop(paste0("unknown district: ", wrong_districts, collapse = ", "))

      # building data
      for(i in 1:length(districts_id)){
        if(length(intersect(districts_id[i], districts)) > 0) dt_d <- rbind.data.frame(dt_d, data_districts[data_districts$id==districts_id[i],])

      }

    }

    rm(districts_name, districts_id, wrong_districts, data_districts)

  }
  # Joining CANTONS, DISTRICTS and MUNICIPALITY ------------------------
  if(!is.null(dt_c) | !is.null(dt_d)){

    bfs_nr <- c(dt_c$id, dt_d$id)
    name <- c(as.character(dt_c$name), as.character(dt_d$name))
    values <- NA
    dt <- cbind.data.frame(bfs_nr, name, values)
    dt <- dt[!duplicated(dt$bfs_nr),]

  } else {

    bfs_nr <- data_districts$id
    name <- as.character(data_districts$name)
    values <- NA
    dt <- cbind.data.frame(bfs_nr, name, values)
    dt <- dt[!duplicated(dt$bfs_nr),]

  }

  dt <- dt[order(dt$bfs_nr),]
  row.names(dt) <- NULL
  rm(dt_c, dt_d, bfs_nr, name, values)

  dt

}

# FUNCTION TO GENERATE CANTON TEMPLATES
#' Generate templates for canton-level data
#'
#' \code{can.template} offers the possbility to generate Swiss canton-level data templates in
#'     the form of data frames. The GitHub version of \code{RSwissMaps} comes with all available geodata
#'     (\code{2001-2017}). Hence, the \code{map.load} function is not part of this version. The templates
#'     contain the canton identification numbers as used by the Swiss Federal Statistical Office
#'     and the canton two-letter abbreviations.
#'
#' @param year a numeric value. Available with GitHub version of package: \code{2001-2017}.
#' @param endofyear if \code{FALSE}, cantons as of January 1 are outputted. If \code{TRUE},
#'     cantons as by December 31 are used. End-of-year data is available for \code{year > 2010}.
#' @param cantons a numeric (cantons identification numbers) or a character
#'     (two-letter abbreviations) vector to create tailor-made templates.
#' @examples
#' # Generating template for 2016:
#' can.template(2016)
#' @export
can.template <- function(year, endofyear = FALSE, cantons = NULL){

  if(!is.logical(endofyear)) stop("endofyear: TRUE or FALSE needed")
  if(year<2001|year>2017) stop("year: no data available")

  if(year == 2017) dt <- RSwissMaps::mapCH2017
  if(year == 2016) dt <- RSwissMaps::mapCH2016
  if(year == 2015) dt <- RSwissMaps::mapCH2015
  if(year == 2014) dt <- RSwissMaps::mapCH2014
  if(year == 2013) dt <- RSwissMaps::mapCH2013
  if(year == 2012) dt <- RSwissMaps::mapCH2012
  if(year == 2011) dt <- RSwissMaps::mapCH2011
  if(year == 2010) dt <- RSwissMaps::mapCH2010
  if(year == 2009) dt <- RSwissMaps::mapCH2009
  if(year == 2008) dt <- RSwissMaps::mapCH2008
  if(year == 2007) dt <- RSwissMaps::mapCH2007
  if(year == 2006) dt <- RSwissMaps::mapCH2006
  if(year == 2005) dt <- RSwissMaps::mapCH2005
  if(year == 2004) dt <- RSwissMaps::mapCH2004
  if(year == 2003) dt <- RSwissMaps::mapCH2003
  if(year == 2002) dt <- RSwissMaps::mapCH2002
  if(year == 2001) dt <- RSwissMaps::mapCH2001

  if(endofyear == TRUE){

    data_cantons <- dt[dt$year==year&dt$type=="canton"&dt$endofyear==1,]

    if(nrow(data_cantons)==0){

      data_cantons <- dt[dt$year==year&dt$type=="canton"&dt$endofyear==0,]
      message(paste0("no data found for endofyear = TRUE; automatically applied endofyear = FALSE; map dates from 01.01.", year))

    }

  } else {

    data_cantons <- dt[dt$year==year&dt$type=="canton"&dt$endofyear==0,]

  }

  # Apply CANTONS on data ------------------------
  dt_c <- NULL

  if(!missing(cantons)){

    cantons_name <- unique(data_cantons$name)
    cantons_id <- unique(data_cantons$id)

    if(!is.numeric(cantons)){

      # checking non-numeric input
      wrong_cantons <- setdiff(tolower(cantons), tolower(cantons_name))
      if(length(wrong_cantons) > 0) stop(paste0("unknown canton id: ", wrong_cantons, collapse = ", "))

      # building data
      for(i in 1:length(cantons_name)){
        if(length(intersect(tolower(cantons_name[i]), tolower(cantons))) > 0) dt_c <- rbind.data.frame(dt_c, data_cantons[data_cantons$id==cantons_id[i],])
      }

    }
    if(is.numeric(cantons)){

      # checking numeric input
      wrong_cantons <- setdiff(cantons, cantons_id)
      if(length(wrong_cantons) > 0) stop(paste0("unknown canton id: ", wrong_cantons, collapse = ", "))

      # building data
      for(i in 1:length(cantons_id)){
        if(length(intersect(cantons_id[i], cantons)) > 0) dt_c <- rbind.data.frame(dt_c, data_cantons[data_cantons$id==cantons_id[i],])

      }

    }

    rm(cantons_name, cantons_id, wrong_cantons, data_cantons)

  }

  # Joining CANTONS, DISTRICTS and MUNICIPALITY ------------------------
  if(!is.null(dt_c)){

    bfs_nr <- c(dt_c$id)
    name <- c(as.character(dt_c$name))
    values <- NA
    dt <- cbind.data.frame(bfs_nr, name, values)
    dt <- dt[!duplicated(dt$bfs_nr),]

  } else {

    bfs_nr <- data_cantons$id
    name <- as.character(data_cantons$name)
    values <- NA
    dt <- cbind.data.frame(bfs_nr, name, values)
    dt <- dt[!duplicated(dt$bfs_nr),]

  }

  dt <- dt[order(dt$bfs_nr),]
  row.names(dt) <- NULL
  rm(dt_c, bfs_nr, name, values)

  dt

}
