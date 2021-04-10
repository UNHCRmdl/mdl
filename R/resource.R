# TODO check if creating a resource twice will duplicate the resource
#' Upload a file and create a resource linked to it
#'
#' This function first uploads a file for a given survey, than creates a resource and link it to the uploaded file.
#'
#' @return API call response.
#'
#' @param survey_idno The unique identifier of an existing survey, e.g.: UNHCR_ETH_2020_SENS_v2.1
#' @param file_path Path to the file to upload, e.g.: C:/path/to/file.pdf
#' @param enum_resource_type The type of resource. We recommend to use the corresponding enumerator, e.g.: mdl_enum_resource_type$doc_report
#' @param resource_title Title of the resource as shown in the platform to the final user.
#' @param resource_description Brief description of the resource as shown in the platform to the final user.
#'
#' @export
mdl_resource_upload_file_and_link <- function(survey_idno, file_path, enum_resource_type, resource_title, resource_description = NULL){

    # upload file
    upload_file_response <- mdl_resource_upload_file(survey_idno = survey_idno, file_path = file_path)

    # if successful, create a resource
    if(identical(upload_file_response$status, "success")){
        mdl_resource_add (
            survey_idno = survey_idno,
            filename = upload_file_response$uploaded_file_name,
            dctype = enum_resource_type,
            title = resource_title,
            #dcformat = "",
            description = resource_description
        )
    }else {
        return(upload_file_response)
    }
}

# upload file for a survey
mdl_resource_upload_file <- function(
    survey_idno,
    file_path,
    resource_id = NULL
    ){

    url <- paste(mdl_api_get_url(), "datasets", survey_idno, "files", sep = "/")

    options <- list(
        "file" = httr::upload_file(file_path)
    )

    httpResponse <- httr::POST(url,
                               httr::add_headers("X-API-KEY" = mdl_api_get_key()),
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

# Create a resource linked to a previously uploaded file
mdl_resource_add <- function(
    survey_idno,
    filename,
    dctype,
    title,
    dcformat = NULL,
    description = NULL,
    author = NULL,
    dcdate = NULL,
    country = NULL,
    language = NULL,
    contributor = NULL,
    publisher = NULL,
    rights = NULL,
    abstract = NULL,
    toc = NULL
){

    # specify call options
    options <- list(
        idno = survey_idno,
        dctype = dctype,
        dcformat = dcformat,
        title = title,
        author = author,
        dcdate = dcdate,
        country = country,
        language = language,
        contributor = contributor,
        publisher = publisher,
        rights = rights,
        description = description,
        abstract = abstract,
        filename = filename,
        toc = toc
    )

    # specify url
    url <-  paste(mdl_api_get_url(), "datasets", survey_idno, "resources", sep = "/")

    # call API
    httpResponse <- httr::POST(url,
                               httr::add_headers("X-API-KEY" = mdl_api_get_key()),
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


##### TEST
# nadar::external_resources_add(idno = "TEST_DATASET", title = "a title", dctype = "doc/adm", file_path = "test.txt", overwrite = "yes")
# xxx <- nadar::external_resources_upload(dataset_idno = "TEST_DATASET", resource_id = "xxxx_file2", file = "test2.txt")
# external_resources_add2(idno = "TEST_DATASET", title = "a title 5", dctype = "doc/tec", overwrite = "yes")
# external_resources_add
#
# mdl_survey_upload_resource_response <- mdl_survey_upload_resource(survey_idno = "TEST_DATASET", file_path = "test5.txt", resource_type_enum = "doc/tec", resource_title = "test upload and res 1 call", resource_description = "testin 1 call", resource_format_enum = NULL)
#
#
# survey_resource_add(survey_idno = "TEST_DATASET",
#                     dctype = "doc/tec",
#                     title = "new title33",
#                     filename = "test3.txt")
