#' Delete a file
#'
#' Deletes a file from a survey.
#'
#' @return API call response.
#'
#' @param survey_idno The unique identifier of an existing survey, e.g.: UNHCR_ETH_2020_SENS_v2.1
#' @param resource_id File ID. Use mdl_resource_file_list() to see the IDs

# TODO: this does not work, it deletes the whole survey!!!
# mdl_resource_file_delete <- function(survey_idno, file_id){
#
#
#     # specify url
#     url <-  paste(mdl_api_get_url(), "datasets", survey_idno, "files", file_id, sep = "/")
#
#     # call API
#     httpResponse <- httr::DELETE(url,
#                                  httr::add_headers("X-API-KEY" = mdl_api_get_key()),
#                                  encode = "json"
#     )
#
#     response_content <- httr::content(httpResponse, "text")
#
#     output <- jsonlite::fromJSON(response_content)
#     if(!is.list(output)){
#         output <- list(output)
#     }
#
#     if(httpResponse$status_code!=200){
#         warning(response_content)
#     }
#
#     return (output)
# }




#' Get list of files
#'
#' Given a survey identifier, provides list of files
#'
#' @return API call response.
#'
#' @param survey_idno The unique identifier of an existing survey, e.g.: UNHCR_ETH_2020_SENS_v2.1
#'
#' @export
mdl_resource_file_list <- function(survey_idno){


    # specify url
    url <-  paste(mdl_api_get_url(), "datasets", survey_idno, "files", sep = "/")

    # call API
    httpResponse <- httr::GET(url,
                              httr::add_headers("X-API-KEY" = mdl_api_get_key()),
                              httr::user_agent(mdl::mdl_api_get_user_agent()),
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


#' Get list of resources
#'
#' Given a survey identifier, provides list of resources
#'
#' @return API call response.
#'
#' @param survey_idno The unique identifier of an existing survey, e.g.: UNHCR_ETH_2020_SENS_v2.1
#'
#' @export
mdl_resource_list <- function(survey_idno){


    # specify url
    url <-  paste(mdl_api_get_url(), "datasets", survey_idno, "resources", sep = "/")

    # call API
    httpResponse <- httr::GET(url,
                               httr::add_headers("X-API-KEY" = mdl_api_get_key()),
                               httr::user_agent(mdl::mdl_api_get_user_agent()),
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

#' Delete a resource
#'
#' Deletes a resource from a survey.
#'
#' @return API call response.
#'
#' @param survey_idno The unique identifier of an existing survey, e.g.: UNHCR_ETH_2020_SENS_v2.1
#' @param resource_id Resource ID. Use mdl_resource_list() to see the IDs
#'
#' @export
mdl_resource_delete <- function(survey_idno, resource_id){


    # specify url
    url <-  paste(mdl_api_get_url(), "datasets", survey_idno, "resources", resource_id, sep = "/")

    # call API
    httpResponse <- httr::DELETE(url,
                              httr::add_headers("X-API-KEY" = mdl_api_get_key()),
                              httr::user_agent(mdl::mdl_api_get_user_agent()),
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

#' Upload a file and create a resource linked to it
#'
#' This function first uploads a file for a given survey, then creates a resource and link it to the uploaded file.
#' The name of the file works as an identifier, meaning that uploading another file with the same name will overwrite the file and the resource.
#'
#' @return API call response.
#'
#' @param survey_idno The unique identifier of an existing survey, e.g.: UNHCR_ETH_2020_SENS_v2.1
#' @param file_path Path to the file to upload, e.g.: C:/path/to/file.pdf
#' @param enum_resource_type The type of resource. We recommend to use the corresponding enumerator, e.g.: mdl_enum_resource_type$doc_report
#' @param title Title of the resource as shown in the platform to the final user.
#' @param description Brief description of the resource as shown in the platform to the final user.
#' @param enum_resource_format Resource file format. We recommend to use the corresponding enumerator, e.g.: mdl_enum_resource_format$Document_PDF
#' @param author Author name
#' @param dcdate Date using YYYY-MM-DD format
#' @param country Country name
#' @param language Language or Language code
#' @param contributor Contributor name
#' @param publisher Publisher name
#' @param rights Rights
#' @param abstract Resource abstract
#' @param toc Table of contents
#' @param overwrite Boolean, specifies if the file/resource should be overwritten if already present
#'
#' @export
mdl_resource_upload_file_and_link <- function(survey_idno,
                                              file_path,
                                              enum_resource_type,
                                              title,
                                              description = NULL,
                                              enum_resource_format = NULL,
                                              author = NULL,
                                              dcdate = NULL,
                                              country = NULL,
                                              language = NULL,
                                              contributor = NULL,
                                              publisher = NULL,
                                              rights = NULL,
                                              abstract = NULL,
                                              toc = NULL,
                                              overwrite = FALSE){

    if(identical(overwrite, TRUE)  || identical(overwrite, "yes")){
        overwrite <- "yes"
    }else{
        overwrite <- "no"
    }

    # check date
    is_date(dcdate)

    # specify call options
    options <- list(
        idno = survey_idno,
        file = httr::upload_file(file_path),
        dctype = enum_resource_type,
        title = title,
        description = description,
        overwrite = overwrite,
        dcformat = enum_resource_format,
        author = author,
        dcdate = dcdate,
        country = country,
        language = language,
        contributor = contributor,
        publisher = publisher,
        rights = rights,
        abstract = abstract,
        toc = toc
    )

    # Rights is set to default as NA when fetching the metadata
    if(identical(options$rights, NA)){
        options$rights <- NULL
    }

    # specify url
    url <-  paste(mdl_api_get_url(), "datasets", survey_idno, "resources", sep = "/")

    # call API
    httpResponse <- httr::POST(url,
                               httr::add_headers("X-API-KEY" = mdl_api_get_key()),
                               httr::user_agent(mdl::mdl_api_get_user_agent()),
                               body = options
                               #encode = "json"
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



#' ## OLDER VERSION: use mdl_resource_upload_file_and_link instead
#' # TODO: check if creating a resource twice will duplicate the resource
#' #' Upload a file and create a resource linked to it
#' #'
#' #' This function first uploads a file for a given survey, then creates a resource and link it to the uploaded file.
#' #'
#' #' @return API call response.
#' #'
#' #' @param survey_idno The unique identifier of an existing survey, e.g.: UNHCR_ETH_2020_SENS_v2.1
#' #' @param file_path Path to the file to upload, e.g.: C:/path/to/file.pdf
#' #' @param enum_resource_type The type of resource. We recommend to use the corresponding enumerator, e.g.: mdl_enum_resource_type$doc_report
#' #' @param resource_title Title of the resource as shown in the platform to the final user.
#' #' @param resource_description Brief description of the resource as shown in the platform to the final user.
#' #'
#' mdl_resource_upload_file_and_link_old <- function(survey_idno, file_path, enum_resource_type, resource_title, resource_description = NULL){
#'
#'     # upload file
#'     upload_file_response <- mdl_resource_upload_file(survey_idno = survey_idno, file_path = file_path)
#'
#'     # if successful, create a resource
#'     if(identical(upload_file_response$status, "success")){
#'         mdl_resource_add (
#'             survey_idno = survey_idno,
#'             filename = upload_file_response$uploaded_file_name,
#'             dctype = enum_resource_type,
#'             title = resource_title,
#'             #dcformat = "",
#'             description = resource_description
#'         )
#'     }else {
#'         return(upload_file_response)
#'     }
#' }


#####
# Instead of using mdl_resource_upload_file and mdl_resource_add we can use directly mdl_resource_upload_file_and_link instead
#####

# # upload file for a survey
# mdl_resource_upload_file <- function(
#     survey_idno,
#     file_path,
#     resource_id = NULL
#     ){
#
#     url <- paste(mdl_api_get_url(), "datasets", survey_idno, "files", sep = "/")
#
#     options <- list(
#         "file" = httr::upload_file(file_path)
#     )
#
#     httpResponse <- httr::POST(url,
#                                httr::add_headers("X-API-KEY" = mdl_api_get_key()),
#                                body = options
#                                #encode = "json"
#                                )
#
#     response_content <- httr::content(httpResponse, "text")
#
#     output <- jsonlite::fromJSON(response_content)
#     if(!is.list(output)){
#         output <- list(output)
#     }
#
#     if(httpResponse$status_code!=200){
#         warning(response_content)
#     }
#
#     return (output)
# }
#
#
# # Create a resource linked to a previously uploaded file
# mdl_resource_add <- function(
#     survey_idno,
#     filename,
#     dctype,
#     title,
#     dcformat = NULL,
#     description = NULL,
#     author = NULL,
#     dcdate = NULL,
#     country = NULL,
#     language = NULL,
#     contributor = NULL,
#     publisher = NULL,
#     rights = NULL,
#     abstract = NULL,
#     toc = NULL,
#     overwrite = "no"
# ){
#
#     # specify call options
#     options <- list(
#         idno = survey_idno,
#         dctype = dctype,
#         dcformat = dcformat,
#         title = title,
#         author = author,
#         dcdate = dcdate,
#         country = country,
#         language = language,
#         contributor = contributor,
#         publisher = publisher,
#         rights = rights,
#         description = description,
#         abstract = abstract,
#         filename = filename,
#         toc = toc,
#         overwrite = overwrite
#     )
#
#     # specify url
#     url <-  paste(mdl_api_get_url(), "datasets", survey_idno, "resources", sep = "/")
#
#     # call API
#     httpResponse <- httr::POST(url,
#                                httr::add_headers("X-API-KEY" = mdl_api_get_key()),
#                                body = options,
#                                encode = "json"
#     )
#
#     response_content <- httr::content(httpResponse, "text")
#
#     output <- jsonlite::fromJSON(response_content)
#     if(!is.list(output)){
#         output <- list(output)
#     }
#
#     if(httpResponse$status_code!=200){
#         warning(response_content)
#     }
#
#     return (output)
# }

