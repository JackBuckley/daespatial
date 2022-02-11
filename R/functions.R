# -------------------------------------------------------------------------
#
# Title: Functions
# Purpose: Functions to help people working with spatial data
# Author: Jack Buckley
# Date: 08/02/2022
#
# -------------------------------------------------------------------------

if(!require("pacman", character.only = T)) install.packages("pacman")

pacman::p_load(
  tidyverse,
  xml2,
  XML,
  httr,
  magrittr,
  sp,
  sf,
  janitor,
  roxygen2
)

# remove old files
rm(list=ls())
gc()



# Assign data points to regions in a shapefile ------------------------

#' Find Region
#'
#' Find the region (e.g. postcode, SA2, electorate) for a series of points identified by their longitude and latitude (lon, lat) coordinates.
#' This tool is useful when you are interested in combining regional level data
#' with a database of information. For example, you could be looking to add information
#' on regional population and employment trends to a database of business locations.
#'
#' It is also helpful when you are looking to aggregated granular data to the regional level.
#' For example, you have scrapped information on the location of companies operating in a
#' certain industry and now want to summarise the information you collected at the Statistical
#' Area 2 (SA2) level, but your data does not have this information.
#' @param shp A shapefile containing the polygons of the regions of interest.
#' This should be a sf object, such as that obtained from using st_read() to load
#' an ESRI shapefile. Shapefiles for common ABS geographies can be found at
#' https://www.abs.gov.au/websitedbs/d3310114.nsf/home/digital+boundaries
#' @param data A tibble/data.frame containing the data to be assigned to regions.
#' If assignment is to be conducted using coordinates (i.e. type = "coords"), the supplied data should include two
#' variables 'long' (longitude) and 'lat' (latitude), which will be used to assign each point to a region.
#' If assignment is to be conducted using an address (i.e. type = "address"), the supplied data should include a
#' variable "addr", which contains the address to use, which will first be geocoded using the tidygeocoder package.
#' @param type Determines the method used to assign points to each regions. Options
#' are either "coords" (default), where assignment is based on supplied longitude and latitude data,
#' or "address", where assignment is based on a supplied address for each point.
#' @return A tibble/data.frame containing the original data as well as the data related
#' to the shapefile polygon each point belongs to. The default is to not
#' return the geometry column (i.e. the polygons are dropped).
#' @examples
#' # Read in a shapefile of SA2 regions (available from the ABS using the link above)
#'
#' sa2_shp <- st_read("SA2_2016_AUST.shp")
#'
#' # Get the SA2 regions that Sydney Airport and Uluru belong to
#'
#' test_data_coords <- tibble(
#'   name = c("Sydney Airport", "Uluru"),
#'   long = c(151.174211, 131.03841670),
#'   lat = c(-33.932302, -25.3440074 ),
#' )
#'
#' output_coords <- find_region(shp = sa2_shp, data = test_data, type = ="coords")
#'
#' # Get the SA2 regions that Sydney Airport and Uluru based on their addresses
#'
#' test_data_addr <- tibble(
#'   name = c("Sydney Airport", "Uluru"),
#'   addr = c("Keith Smith Ave, Mascot NSW 2020", "Uluru Rd, Petermann NT 0872")
#' )
#'
#' output_addr <- find_region(shp = sa2_shp, data = test_data, type = ="coords")
#'
#' @export

find_region <- function(data, shp, type = "coords"){

  # check that a shapefile has been included
  if(!("sf" %in% attributes(shp)$class)) {
    stop("Input shp must be of class sf (shapefile), use st_read() to import a .shp file")
  }

  # check we have the valid data type for
  if(type == "coords"){

    if(!"lat" %in% names(data) | !"long" %in% names(data)) {
      stop("Input data must contain columns titled 'lat' and 'long'")
    }

  } else if(type == "address"){

    if(!"addr" %in% names(data)) {
      stop("Input data must contain a column of address in a column name 'addr'")
    }

    data %<>% tidygeocoder::geocode(addr, method = "arcgis")

  } else {

    stop("Input 'type' must be one of 'coords' or 'address'!")
  }

  # create spatial points from the data
  data %<>%
    dplyr::mutate(dplyr::across(c(long, lat), ~as.numeric(.))) %>%
    sf::st_as_sf(coords = c("long", "lat"), crs = sf::st_crs(4283)) %>%
    sf::st_buffer(1e-10)

  # buffer shapes to account for invalid geometries
  shp %<>% st_buffer(1e-4)
  data %<>% st_buffer(1e-4)

  # transform to the same CRS
  data %<>% sf::st_transform(32617)
  shp %<>% sf::st_transform(32617)

  testthat::expect_true(sf::st_crs(shp) == sf::st_crs(data))

  intersection <- dplyr::as_tibble(sf::st_intersection(data, shp))

  intersection$area_crossover_sqm <- sf::st_area(intersection$geometry)

  intersection %<>%
    dplyr::mutate(area_crossover_sqm = area_crossover_sqm %>% as.double()) %>%
    dplyr::filter(area_crossover_sqm > 0) %>%
    dplyr::as_tibble() %>%
    dplyr::select(-geometry, -area_crossover_sqm)

  data %<>% left_join(intersection) %>% select(-geometry)

  return(data)
}



# Get region overlap --------------------------------------------------

#' Create Concordance
#'
#' This function is designed to create a spatial concordance between two different
#' sets of regions (e.g. postcode to federal electorate, or postcode to SA2),
#' based on the spatial information contained in two supplied shapefiles
#' (one for each region).
#'
#' This is particularly helpful if you are interested in creating a
#' concordance between regions, or allocating smaller regions to a larger
#' region (e.g. postcode to SA3) where there is an imperfect relationship and
#' publicly available concordances are out of date (e.g. ABS spatial concordances
#' are limited and have not been updated since 2011, even though regions have changed).
#'
#'
#' @param region_from A shapefile containing the polygons of the first region type of interest (e.g. SA2).
#' This should be a sf object, such as that obtained from using st_read() to load
#' an ESRI shapefile (.shp). Shapefiles for common ABS geographies can be found at
#' https://www.abs.gov.au/websitedbs/d3310114.nsf/home/digital+boundaries
#' @param region_to A shapefile containing the polygons of the second region type of interest (e.g. postcode).
#' This should be a sf object, such as that obtained from using st_read() to load
#' an ESRI shapefile (.shp). Shapefiles for common ABS geographies can be found at
#' https://www.abs.gov.au/websitedbs/d3310114.nsf/home/digital+boundaries
#' @param id_from An optional parameter which identifies an ID variable for the first
#' region (e.g. "sa2_main16"). If supplied along with id_to, these variables will be
#' used to calculate a clean concordance between the two regions. Otherwise,
#' only the overlap between regions (in sqm) will be returned, which will require
#' further manipulation to create a concordance.
#' @param id_to See id_from description, but for the regions contained within region_to
#' @return If id_from and id_to are both supplied (and correct), the function will return a
#' tibble with the ID variables and a concordance from region 1 (region_from), to region 2 (region_to).
#' If no ID variables are supplied, the function will return a tibble with all
#' variables in region_from and region_to and the overlap between the two regions in sqm.
#' @examples
#' # Generate a concordance between Australian Postcodes and SA3 regions
#' (these shapefiles are available for download from here: https://www.abs.gov.au/websitedbs/d3310114.nsf/home/digital+boundaries)
#'
#' sa3_shp <- st_read("SA3_2021_AUST_GDA2020.shp")
#'
#' poa_shp <- st_read("POA_2021_AUST_GDA2020.shp")
#'
#' poa_to_sa3 <-
#'   create_concordance(
#'     region_from = poa_shp,
#'     region_to = sa3_shp,
#'     id_from = "POA_CODE21",
#'     id_to = "SA3_NAME21"
#'   )
#'
#' @export

# get a spatial concordance between two different shapefiles - i.e. the overlap between regions
create_concordance <- function(region_from, region_to, id_from = NULL, id_to = NULL){

  # check that both are shapefiles
  if(!("sf" %in% attributes(region_from)$class) | !("sf" %in% attributes(region_to)$class)) {

    stop("Input files must be sf class shapefiles, use st_read() to import two .shp files")
  }

  region_from %<>% sf::st_transform(32617)
  region_to %<>% sf::st_transform(32617)

  # buffer shapes to account for invalid geometries
  region_from %<>% sf::st_buffer(1e-11)
  region_to %<>% sf::st_buffer(1e-11)

  # remove non-spatial units from the shapefiles
  region_from %<>% dplyr::mutate(temp_id = dplyr::row_number())

  test_1 <- region_from %>%
    sf::st_centroid() %>%
    sf::st_transform(4283) %>%
    dplyr::mutate(lon = sf::st_coordinates(geometry)[,1]) %>%
    dplyr::filter(!is.na(lon))

  region_from %<>% dplyr::filter(temp_id %in% test_1$temp_id) %>% dplyr::select(-temp_id)

  region_to %<>% dplyr::mutate(temp_id = dplyr::row_number())

  test_2 <- region_to %>%
    sf::st_centroid() %>%
    sf::st_transform(4283) %>%
    dplyr::mutate(lon = sf::st_coordinates(geometry)[,1]) %>%
    dplyr::filter(!is.na(lon))

  region_to %<>% dplyr::filter(temp_id %in% test_2$temp_id) %>% dplyr::select(-temp_id)

  # check that the Coordinate Reference Systems (CRS) match - the intersection calculation won't work otherwise
  if(sf::st_crs(region_from) != sf::st_crs(region_to)) {

    stop("Error converting Coordinate Reference Systems to the same format!")
  }

  intersection <- dplyr::as_tibble(sf::st_intersection(region_from, region_to))

  intersection$area_crossover_sqm <- sf::st_area(intersection$geometry)

  intersection %<>%
    dplyr::as_tibble() %>%
    dplyr::select(-geometry)

  intersection %<>%
    dplyr::mutate(area_crossover_sqm = area_crossover_sqm %>% as.double()) %>%
    dplyr::filter(area_crossover_sqm > 1e-5)

  # If IDs have been supplied, then provide a summarised concordance
  if(!is.null(id_from) & !is.null(id_to)){

    # check that the id columns are valid
    if(!id_from %in% names(intersection) | !id_from %in% names(region_from) |
       !id_to %in% names(intersection) | !id_to %in% names(region_to)){

      stop("Invalid IDs! Please ensure that ID_from is a variable in region_from and ID_to is variable in region_to.")
    }

    conc <- intersection %>%
      dplyr::group_by(!!dplyr::sym(id_from)) %>%
      dplyr::mutate(tot_overlap = sum(area_crossover_sqm))

    conc %<>%
      dplyr::group_by(!!dplyr::sym(id_from), !!dplyr::sym(id_to)) %>%
      dplyr::summarise(ratio = area_crossover_sqm / tot_overlap)

    conc %<>%
      dplyr::mutate(ratio = round(ratio, 3)) %>%
      dplyr::filter(ratio > 0)

  } else {

    conc <- intersection
  }

  return(conc)
}
