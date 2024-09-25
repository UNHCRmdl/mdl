# TODO: true and false nstead of 1 and 0
#' Create documentation PDF
#'
#' This function creates the documentation PDF from the survey metadata.
#'
#' @return API call response.
#'
#' @param survey_idno The identifier of the survey for which you want to generate the PDF, e.g.: UNHCR_ETH_2020_SENS_v2.1
#' @param variable_list Specifies if the PDF should include the variable list: 1 for yes, 0 for no.
#' @param variable_description Specifies if the PDF should include the variable description: 1 for yes, 0 for no.
#' @param external_resource Specifies if the PDF should include the external resources: 1 for yes, 0 for no.
#'
#' @export
mdl_survey_generate_pdf <- function(survey_idno,
                                    #resource_title = "Documentation",
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

    return(survey_generate_pdf_response)

    # # if successful, link a resource
    # if(identical(survey_generate_pdf_response$status, "success")){
    #     mdl_resource_add (
    #         survey_idno = survey_idno,
    #         filename = basename(survey_generate_pdf_response$output),
    #         dctype = "doc/tec",
    #         title = resource_title,
    #         dcformat = "application/pdf",
    #         description = NULL,
    #         overwrite = "yes"
    #     )
    # }else {
    #
    # }
}

# Create the documentation pdf from the survey metadata.
survey_generate_pdf <- function(
    survey_idno,
    variable_toc = 0,
    variable_description = 0,
    include_resources = 0,
    language = "en" # "fr", "ar"
){

    # specify call options
    options <- list(
        variable_toc = variable_toc,
        variable_description = variable_description,
        include_resources = include_resources,
        language = language
    )

    # specify url
    url <-  paste(mdl_api_get_url(), "datasets", "generate_pdf", survey_idno, sep = "/")

    # call API
    httpResponse <- httr::POST(url,
                               httr::add_headers("X-API-KEY" = mdl_api_get_key()),
                               httr::user_agent(mdl::mdl_api_get_user_agent()),
                               body = options,
                               encode = "json"
    )

    response_content <- httr::content(httpResponse, "text")

    output <- jsonlite::fromJSON(response_content)
    if(!is.list(output)){
        output <- list(output)
    }

    if(httpResponse$status_code!=200){
        warning(response_content)
    }

    return (output)
}
