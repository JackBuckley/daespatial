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
#' @return A tibble containing the original data as well as the data related
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
