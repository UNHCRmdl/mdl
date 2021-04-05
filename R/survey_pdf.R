
# Create the documentation pdf from the survey metadata and link a resource to it
#' @export
mdl_survey_generate_pdf <- function(survey_idno,
                                    resource_title = "Documentation",
                                    variable_list = 1,
                                    variable_description = 1,
                                    external_resource = 0)
{
    # generate pdf
    survey_generate_pdf_response <- survey_generate_pdf(
        survey_idno = survey_idno,
        variable_toc = variable_list,
        variable_description = variable_description,
        include_resources = external_resource,
        language = "en"
    )

    # if successful, link a resource
    if(survey_generate_pdf_response$status_code == 200){
        survey_resource_add (
            survey_idno = survey_idno,
            filename = basename(survey_generate_pdf_response$response$output),
            dctype = "doc/tec",
            title = resource_title,
            dcformat = "application/pdf",
            description = NULL
        )
    }else {
        return(survey_generate_pdf_response)
    }
}

# Create the documentation pdf from the survey metadata
survey_generate_pdf <- function(
    survey_idno,
    variable_toc = 0,
    variable_description = 0,
    include_resources = 0,
    language = "en", # "fr", "ar"
    api_key = NULL,
    api_base_url = NULL
){

    # get api key and url if not specified
    if(is.null(api_key)){
        api_key=nadar::get_api_key();
    }
    if(is.null(api_base_url)){
        api_base_url=nadar::get_api_url();
    }

    # specify call options
    options <- list(
        variable_toc = variable_toc,
        variable_description = variable_description,
        include_resources = include_resources,
        language = language
    )

    # specify url
    url <-  paste(api_base_url, "datasets", "generate_pdf", survey_idno, sep = "/")

    # call API
    httpResponse <- httr::POST(url,
                               httr::add_headers("X-API-KEY" = api_key),
                               body = options,
                               encode = "json"
    )

    # print error if any
    if(httpResponse$status_code!=200){
        warning(httr::content(httpResponse, "text"))
    }


    # return output
    output <- list(
        status_code = httpResponse$status_code,
        response = jsonlite::fromJSON(httr::content(httpResponse,"text"))
    )

    return(output)
}

## test
# survey_generate_pdf("TEST_DATASET", 1, 1, 1)
# #$response$output  "ddi-documentation-english-303.pdf"
# mdl_survey_generate_pdf("TEST_DATASET")
