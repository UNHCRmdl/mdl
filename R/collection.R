#' Get list of collection
#'
#' Returns list of all collections
#'
#' @return API call response.
#'
#'
#' @export
mdl_collection_list <- function(){

    url <- paste(mdl_api_get_url(), 'collections', sep = "/")

    httpResponse <- httr::GET(url,
                              httr::add_headers("X-API-KEY" = mdl_api_get_key())
    )

    response_content <- httr::content(httpResponse, "text")

    if(httpResponse$status_code!=200){
        warning(response_content)
    }

    output <- jsonlite::fromJSON(response_content)

    return (output)
}


#' Get one collection
#'
#' Returns a single collection
#'
#' @return API call response.
#'
#' @param collection_id The ID of the collection. To see an up to date list of collections call mdl_collection_list(). You can also use the corresponding enumerator, e.g.: mdl_enum_collection$WestAfrica
#'
#' @export
mdl_collection_get <- function(collection_id){

    url <- paste(mdl_api_get_url(), 'collections', collection_id, sep = "/")

    httpResponse <- httr::GET(url,
                              httr::add_headers("X-API-KEY" = mdl_api_get_key())
    )

    response_content <- httr::content(httpResponse, "text")

    if(httpResponse$status_code!=200){
        warning(response_content)
    }

    output <- jsonlite::fromJSON(response_content)

    return (output)
}




