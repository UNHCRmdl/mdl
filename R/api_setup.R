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
    nadar::set_api_key(api_key)
}

#' Set API URL
#'
#' Sets the API base endpoint URL. You can specify any of the different versions on the platform (production or user testing) and call the function again for switching from one to another.
#'
#' @param enum_api_url The API base endpoint URL. We recommend to use the related enumerator, e.g.: mdl_enum_api_url$uat
#' @export
mdl_api_set_url <- function(enum_api_url){
    nadar::set_api_url(enum_api_url)
}


