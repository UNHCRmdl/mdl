#' Open MDL APIs documentation in browser
#'
#' Prints the url to the APIs documentation webpage and opens it in the browser.
#'
#' @return Does not return any value
#' @examples
#' # Simply call the function to see the documentation
#' mdl_api_open_documentation()
#' @export
mdl_api_open_documentation <- function(){
    doc_url <- "https://microdata.unhcr.org/api-documentation/catalog-admin/index.html"
    print(doc_url)
    browseURL(doc_url)
}

#' Set API key
#'
#' Sets the API key. The key can be found in your MDL profile after you log-in (https://microdata.unhcr.org/index.php/auth/profile).
#'
#' @param api_key The API key.
#' @export
mdl_api_set_key <-  function(api_key){
    Sys.setenv("MDL_API_KEY" = api_key)
}

#' Set API URL
#'
#' Sets the API base endpoint URL. You can specify any of the different versions on the platform (production or user testing) and call the function again for switching from one to another.
#'
#' @param enum_api_url The API base endpoint URL. We recommend to use the related enumerator, e.g.: mdl_enum_api_url$uat
#' @export
mdl_api_set_url <- function(enum_api_url){
    Sys.setenv("MDL_API_URL" = enum_api_url)
}


# Get API key
#' @export
mdl_api_get_key <-  function(){
    api_key <- Sys.getenv("MDL_API_KEY")
    if(is.null(api_key) || api_key == ""){
        stop("API key was not set, use mdl_api_set_key()")
        return(NULL)
    }
    return(api_key)
}

# Get API url
#' @export
mdl_api_get_url <-  function(){
    api_url <- Sys.getenv("MDL_API_URL")
    if(is.null(api_url) || api_url == ""){
        stop("API URL was not set, use mdl_api_set_url()")
        return(NULL)
    }
    return(api_url)
}


# Get User Agent to pass through firewall
#' @export
mdl_api_get_user_agent <-  function(){
    a_user_agent = "CurationTeam"
    return(a_user_agent)
}





