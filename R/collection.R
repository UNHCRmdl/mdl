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
                              ,httr::user_agent(mdl::mdl_api_get_user_agent())
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
                              ,httr::user_agent(mdl::mdl_api_get_user_agent())
    )

    response_content <- httr::content(httpResponse, "text")

    if(httpResponse$status_code!=200){
        warning(response_content)
    }

    output <- jsonlite::fromJSON(response_content)

    return (output)
}


#' List datasets by collections
#'
#' List all datasets with collection owner and all linked collections.
#' There is one row for each linked collection included the main collection, meaning that datasets repeat.
#'
#' @return API call response.
#'
#' @param limit Max number of datasets to get.
#' @param offset Point from which the query will start, to be used if you want to used paging. Default is 0.
#'
#' @export
mdl_datasets_by_collection <- function(limit = 500, offset = 0){

    url <- paste(mdl_api_get_url(), 'datasets', 'collections', sep = "/")
    url <- paste(url, paste0("limit=", limit), sep = "?")
    url <- paste(url, paste0("offset=", offset), sep = "&")

    httpResponse <- httr::GET(url,
                              httr::add_headers("X-API-KEY" = mdl_api_get_key())
                              ,httr::user_agent(mdl::mdl_api_get_user_agent())
    )

    response_content <- httr::content(httpResponse, "text")

    if(httpResponse$status_code!=200){
        warning(response_content)
    }

    output <- jsonlite::fromJSON(response_content)

    return (output)
}


