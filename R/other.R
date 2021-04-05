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

