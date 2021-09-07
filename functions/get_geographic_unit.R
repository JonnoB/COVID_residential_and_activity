#' Get geographic unit
#' 
#' This function matches the latitude and longitude of a point to it's geographic unit id
#' 
#' 
#' @param data a data frame containing the 
#' @param shape_files an sf object dataframe containing the geographic unit shape files
#' @param block_size an integer. Used to prevent memory allocation crashes, determines the number of observations converted in
#' each pass. This makes the conversion process longer but more reliable. The default is 100,000.
#' @param unit_id A character string. The column name of the geographic ID's, it is these values that will be returned.
#' @param position_names A character vector. the names that the latitude and longitude are stored under in the data. defaults to
#' "lat" and "lon"
#' 
#' @details A lot of GIS data comes in the form of latitudes and longitudes, however, sometimes this data is too detailed and the data
#' can be grouped into discrete geographies. In the UK these geographies are often LSOA, MSOA or local authority.
#' Such groupings reduce the data to a discrete set and allow for much easier processing. This function is designed to facilitate the
#' conversion to such a form
#' 
#' 
#' @return a vector of unit id's names in the order of the original data
#' 
#' @export


get_geographic_unit <- function(data, shape_files, block_size = 1e5, unit_id, position_names = c("lat", "lon")){

remainder_block_size <- nrow(data)-floor(nrow(data)/block_size)*block_size


iter_vect <- c(rep(1:floor(nrow(data)/block_size), each = block_size), 
               rep(ceiling(nrow(data)/block_size), each = remainder_block_size))


unit_vect <- unique(iter_vect) %>%
  map(~{
    print(.x)
    temp_df <- data[iter_vect==.x,] %>%
      st_as_sf(., coords = c(position_names[2], position_names[1]), crs = st_crs(shape_files)) 
    
    #create the spatial interaction list
    spatial_intersection_list <- st_intersects(temp_df$geometry, shape_files)
    
    #check to see if any point fall on the border and into two geographies
    number_elements <- spatial_intersection_list %>%map_dbl(length)
    #total number
    more_thank_one <- sum(number_elements>1)
    
    #If falls into more than 1 take the first of the spaces and discard the other/s
    #these evens are incredibly rare less than 1 in 10^4
    if(more_thank_one>0){
      
      spatial_intersection_list[number_elements>1] <- spatial_intersection_list[number_elements>1] %>%
        map(~{
          .x[1]
        })
      
    } 
    
    #return the unit names
    unit_id_vect_inner <- shape_files[, unit_id][[1]]
    unit_id_vect_inner[as.integer(spatial_intersection_list)]
  }) %>%
  unlist

}
