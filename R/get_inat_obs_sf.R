#' Helper for rinat_get_obs to query with sf polygon object
#' @description
#' Helper written for rinat::get_inat_obs, since it usually takes a vector of coords
#' rather than an sf object. Often, want to query/clip by sf object for the
#' returned results, rather than a simple bounding box. 
#' 
#' for data, or just filter results by a subset of what is offered by the API.
#' @param query Query string for a general search.
#' @param quality The quality grade to be used.  Must be either "casual" or "research".  If left
#' blank both will be returned.
#' @param taxon_name Filter by iNat taxon name. Note that this will also select observations of
#' descendant taxa. Note that names are not unique, so if the name matches multiple taxa, no
#' observations may be returned.
#' @param taxon_id Filter by iNat taxon ID. Note that this will also select observations of descendant taxa.
#' @param place_id Filter by iNat place ID.
#' @param geo Flag for returning only results that are georeferenced, TRUE will exclude
#' non-georeferenced results, but they cannot be excluded.
#' @param annotation Filter by annotation. Vector of length 2, the first 
#' element being the term ID (e.g. "1" for life stage) and the second 
#' element being the value ID (e.g. "2" for adult). Refer to the 
#' \href{https://forum.inaturalist.org/t/how-to-use-inaturalists-search-urls-wiki-part-2-of-2/18792#heading--annotations}{annotation documentation} to know which values to use.
#' @param year Return observations only in that year (can only be one year, not a range of years).
#' @param month Return observations only by month, must be numeric, 1...12
#' @param day Return observations only on a given day of the month,  1...31
#' @param area Supply an sf or sp object from which the bounding box will
#' be derived.
#' @param maxresults The maximum number of results to return. Should not be
#' a number higher than 10000.
#' @param meta (logical) If TRUE, the output of this function is a list with metadata on the output
#' and a data.frame of the data. If FALSE (default), just the data.frame.
#' @note Filtering doesn't always work with the query parameter for some reason (a problem on
#' the API end).  If you want to filter by time, it's best to use the scientific name and put it
#' in the 'taxa' field, and not in the query field.  Another issue is that the query parameter
#' will search the entire entry, so it is possible to get unintended results.  Depending on your
#' use case it may be advisable to use the "taxon" field instead of the query field.
#' @return A sf dataframe of the number of observations requested.
#' @export
#'
get_inat_obs_sf <- function(query = NULL,
                            taxon_name = NULL,
                            taxon_id = NULL,
                            place_id = NULL,
                            quality = NULL,
                            geo = NULL,
                            annotation = NULL,
                            year = NULL,
                            month = NULL,
                            day = NULL,
                            area = NULL,
                            maxresults = 100,
                            meta = FALSE
) {
  # check if area param is sf or SpatVector
  if (!is(area, "sf")) {
    stop("The `area` parameter is not a `sf` object.")
  }
  # Bounding coordinates for iNaturalist query
  bounds <- area %>%
    sf::st_transform(crs = 4326) %>% # rinat needs coord in latlong
    sf::st_bbox(area)
  # need to reorder vector
  bounds <- c(bounds$ymin, bounds$xmin, bounds$ymax, bounds$xmax)
  
  # ---- QUERY INAT OBSERVATIONS BY BOUNDING BOX AND SPECIES LIST ----
  message_start <- stringr::str_glue(
    "Querying iNaturalist for {taxon_name} within the bounds of {ymin}, {xmin} and {ymax}, {xmax}",
    ymin = bounds['ymin'],
    xmin = bounds['xmin'],
    ymax = bounds['ymax'],
    xmax = bounds['xmax']
  )
  message(message_start)
  rinat_results_df <- rinat::get_inat_obs(
    bounds = bounds,
    taxon_name = taxon_name,
    quality = quality,
    maxresults = maxresults
  )
  # TODO: logic for no observations returned
  message_inat_results <- stringr::str_glue(
    "Finished iNaturalist query. Returned {n_obs} observations.",
    n_obs = nrow(rinat_results_df)
  )
  message(message_inat_results)
  rinat_results_sf <- rinat_results_df %>% 
    tidyr::unnest(geojson) %>% 
    tidyr::unnest_wider(coordinates, names_sep = "") %>% 
    dplyr::rename(longitude = coordinates1, latitude = coordinates2) %>%
    sf::st_as_sf(coords = c("longitude", "latitude")) %>%
    sf::st_set_crs(4326) %>% 
    sf::st_transform(sf::st_crs(area)) %>% # transform iNat results to match area crs
    sf::st_intersection(area) # clip iNat results by area polygon
  message_inat_crop <- stringr::str_glue(
    "Cropped iNaturalist observations to area. Returned {n_obs} within `area`.",
    n_obs = nrow(rinat_results_sf)
  )
  message(message_inat_crop)
  rinat_results_sf
}