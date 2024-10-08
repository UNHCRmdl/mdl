# checks if data is in correct format
is_date <-  function(x, format = "%Y-%m-%d") {
    formatted = try(as.Date(x, format), silent = TRUE)
    is_date = as.character(formatted) == x & !is.na(formatted)  # valid and identical to input
    is_date[is.na(x)] = NA  # Insert NA for NA in x

    if(! is_date %in% c(TRUE) && ! x %in% c( "", NA) && ! is.null(x) ) {
        error_message <- (paste0("The date provided is not correct (", x, "). Please use the format yyyy-mm-dd and make sure that the date is correct."))
        stop(error_message)
    }
}

# is_date("")
# is_date(NA)
# is_date(NULL)
# is_date("12/12/2022")
# is_date("2023-02-29")
# is_date("2024-02-29")



#' Create metadata list to create a survey
#'
#' Creates a list to be passed as an argument to the mdl_survey_create function to create a survey in the MDL.
#'
#' @return A list containing the metadata to be passed to the mdl_survey_create function.
#'
#' @param survey_idno The unique identifier of the survey you want to create, e.g.: UNHCR_ETH_2020_SENS_v2.1
#' @param title Title of the survey. Should not include the name of the country, e.g.: Socioeconomic Survey of Refugees in Kakuma, 2019
#' @param country_ISO_alpha3_codes A vector with the list of countries ISO 3166-1 alpha-3 where the survey took place, e.g.: c("LBN", "SYR"). Full list of codes can be found here: https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3
#' @param enum_survey_study_type The type of study. For consistency, we recommend to use the corresponding enumerator, e.g.: mdl_enum_survey_study_type$'Price Survey \[hh/prc\]'
#' @param series_information If the survey is part of a series, you can provide some info about it.
#' @param abstract The abstract of the survey. Use \\n to start a new line in the text.
#' @param enum_survey_kind_of_data Kind of data. For consistency, we recommend to use the corresponding enumerator, e.g.: mdl_enum_survey_kind_of_data$'Sample survey data \[ssd\]'
#' @param unit_of_analysis Unit of analysis, e.g.: Household and individual
#' @param version_description Description of the version, e.g.: v2.1: Edited, anonymous dataset for licensed distribution
#' @param version_date Date of the version in the format yyyy-mm-dd, eg: 2020-01-30
#' @param scope_description Scope description. Use \\n to start a new line in the text.
#' @param enum_survey_topics A vector with the topics. For consistency, we recommend to use the corresponding enumerator, e.g.: c(mdl_enum_survey_topics$Health, mdl_enum_survey_topics$Protection)
#' @param keywords A vector with custom keywords, e.g.: c("Asylum Seekers", "Forced Displacement", "Conflict")
#' @param geographic_coverage Geographic coverage of the survey.
#' @param universe Universe of the survey.
#' @param primary_investigators A vector listing the main organizations that managed the study, e.g.: c("UNCHR", "WFP")
#' @param other_producers A vector listing other organizations involved, e.g.: c("Org1", "Org2")
#' @param sampling_description Description of the sampling. Use \\n to start a new line in the text.
#' @param weighting_description Description of the weighting. Use \\n to start a new line in the text.
#' @param collection_date_start Date when the collection started in the format yyyy-mm-dd, eg: 2019-02-01
#' @param collection_date_end Date when the collection ended in the format yyyy-mm-dd, eg: 2019-02-25
#' @param enum_survey_collection_mode Collection mode. For consistency, we recommend to use the corresponding enumerator, e.g.: mdl_enum_survey_collection_mode$'Face-to-face \[f2f\]'
#' @param data_collection_notes Notes on the actual data collection.
#' @param data_collectors Vector with the name of the actual data collectors, e.g.: c("National Bureau of Statistics", "Department of Immigration Services")
#' @param questionnaire_description Description of the questionnaire sections. Use \\n to start a new line in the text.
#' @param contacts_list A list with the contacts. Each contact is a list with the following objects: name, affiliation, email; e.g.: list(list(name = "Curation team", affiliation = "UNHCR", email = "microdata@unhcr.org"))
#' @param publication_year Year of publication used in the citation. If not provided, it will be taken from the collection_date_end.
#' @param citation_requirements A string containing the citation requirements. If not specified or NULL is passed, an automatic citation string is created using the other metadata fields. To not include any citation pass an empty string "".
#' @param metadata_producer The metadata producer. By default it is UNHCR
#' @param metadata_production_date The date in which the metadata was produced, in the format yyyy-mm-dd, eg: 2019-04-30. By default, the current date is taken with Sys.Date().
#'
#'
#' @export
mdl_survey_generate_metadata_list <- function(
    survey_idno, # UNHCR_ETH_2020_SENS_V2.1
    title,
    country_ISO_alpha3_codes, # c()
    enum_survey_study_type = "",
    series_information = "",
    abstract,
    enum_survey_kind_of_data = "",
    unit_of_analysis = "",
    version_description = "", #
    version_date = "", # yyyy-mm-dd
    scope_description = "",
    enum_survey_topics = c(),
    keywords = c(""),
    geographic_coverage = "",
    universe = "",
    primary_investigators, # c()
    other_producers = c(""),
    sampling_description = "",
    weighting_description = "",
    collection_date_start,
    collection_date_end,
    enum_survey_collection_mode = "",
    data_collection_notes = "",
    data_collectors = c(),
    questionnaire_description = "",
    contacts_list = list(list(name = "Curation team", affiliation = "UNHCR", email = "microdata@unhcr.org")),
    publication_year = NULL,
    citation_requirements = NULL,
    metadata_producer = "UNHCR",
    metadata_production_date = as.character(Sys.Date())
){
    # check dates
    is_date(version_date)
    is_date(collection_date_start)
    is_date(collection_date_end)
    # check if dates are congruent
    if( collection_date_end < collection_date_start ) {
        error_message <- "Data collection end date is before start date."
        stop(error_message)
    }

    # create countries
    country_ISO_alpha3_codes <- toupper(country_ISO_alpha3_codes)
    country_names <- sapply(country_ISO_alpha3_codes, function(x){return (mdl_enum_country[[x]])})


    # create citation
    # countries
    countries_string <- stringr::str_c(country_names, collapse = ", ")

    # organizations
    investigators_string <- stringr::str_c(primary_investigators, collapse = ", ")
    # publication year
    if(is.null(publication_year) || as.numeric(publication_year) %in% c(NA)){
        publication_year <- substring(collection_date_end, 1, 4)
    }
    # citation string
    a_survey_citation <- paste0(investigators_string, " (", publication_year, "). ", countries_string, ": ", title, ". ", "Accessed from: https://microdata.unhcr.org")
        # paste0(investigators_string, ": ", countries_string, " - ", title, ". ", "UNHCR microdata library, https://microdata.unhcr.org")
        #UNHCR (2021). Kenya: Socio-economic assessment of refugees in Kakuma camp, 2015. Accessed from: https://microdata.unhcr.org.

    if(! is.null(citation_requirements)){
        a_survey_citation <- citation_requirements
    }

    # create metadata list
    a_survey_metadata <- list(

        doc_desc=list(
            idno = survey_idno,
            title = gsub("[^A-Za-z0-9]", "_", title),
            prod_date = as.character(metadata_production_date),
            producers=list(
                list(
                    name = metadata_producer,
                    abbr= ""
                )
            )
        ),

        study_desc = list(
            title_statement = list(
                idno = survey_idno,
                title = title,
                sub_title = "",
                alternate_title = "",
                translated_title = ""
            ),
            authoring_entity = data.frame(
                name = primary_investigators
                #affiliation = c("", "")
            ),
            distribution_statement = list(
                contact = contacts_list
            ),
            production_statement = list(
                producers = data.frame(
                    name = other_producers
                    #affiliation = c("", "", ""),
                    #role = c("", "", "")
                )
            ),
            series_statement = list(
                series_name = enum_survey_study_type,
                series_info = series_information
            ),
            version_statement = list(
                version = version_description,
                version_date = version_date
            ),
            study_info = list(
                abstract = abstract,
                geog_coverage = geographic_coverage,
                analysis_unit = unit_of_analysis,
                universe = universe,
                data_kind = enum_survey_kind_of_data,
                notes = scope_description,
                nation = data.frame(
                    name = as.vector(country_names),
                    abbreviation = country_ISO_alpha3_codes
                ),
                #   list(
                #   list(
                #     name = "Test",
                #     abbreviation = "tst")
                # )
                keywords = data.frame(
                    keyword = keywords
                    #vocab = c("", "", ""),
                    #uri = c("", "", "")
                ),
                topics = data.frame(
                    topic = enum_survey_topics
                    #vocab = c("wbg", "un", "xxx"),
                    #uri = c("prod", "prod2", "prod3")
                ),
                coll_dates = data.frame(
                    start = c(collection_date_start),
                    end = c(collection_date_end)
                    #cycle = c("prod", "prod2", "prod3")
                )
            ),
            method = list(
                data_collection = list(
                    sampling_procedure = sampling_description,
                    coll_mode = enum_survey_collection_mode,
                    research_instrument = questionnaire_description,
                    coll_situation = data_collection_notes,
                    weight = weighting_description,
                    data_collectors = data.frame(
                        name = data_collectors
                        #abbreviation = c("", "", ""),
                        #affiliation= c("", "", "")
                    )
                )
            ),
            data_access = list(
                dataset_use = list(
                    cit_req = a_survey_citation
                    #contact = contacts_list # would appear under ACCESS AUTHORITY
                    #   data.frame(
                    #   name = c("aaa", "bbb"),
                    #   affiliation = c("un", "un"),
                    #   email = c("a@a", "b@b")
                    # )
                )
            )
        )
    )



    # If no keyword was provided, remove it
    if(is.null(keywords) || keywords[1] %in% c(NA, "")){
        a_survey_metadata$study_desc$study_info$keywords <- NULL
    }

    # If no other_producers was provided, remove it
    if(is.null(other_producers) || other_producers[1] %in% c(NA, "")){
        a_survey_metadata$study_desc$production_statement <- NULL
    }

    # if contacts are not provided, remove them
    if(is.null(contacts_list) | identical(contacts_list, NA) | identical(contacts_list, "")){
        a_survey_metadata$study_desc$distribution_statement$contact <- NULL
    }

    # if topics are not provided, remove them
    if(is.null(enum_survey_topics) | identical(enum_survey_topics, NA) | identical(enum_survey_topics, "")){
        a_survey_metadata$study_desc$study_info$topics <- NULL
    }

    # if enum_survey_collection_mode are not provided, remove them
    if(is.null(enum_survey_collection_mode) | identical(enum_survey_collection_mode, NA) | identical(enum_survey_collection_mode, "")){
        a_survey_metadata$study_desc$method$data_collection$coll_mode <- NULL
    }



    # return
    return(a_survey_metadata)
}


#' Download survey DDI file
#'
#' Downloads a DDI XML file.
#'
#' @return File path
#'
#' @param survey_idno Path to the DDI XML file.
#' @param path Path to where you want to save the file, if not specified uses current working directory.
#'
#' @export
mdl_survey_download_ddi <- function(survey_idno, path = NULL){
    idno_url <- paste(mdl_api_get_url(), "catalog", "ddi", survey_idno, sep = "/")
    idno_file <- paste(survey_idno, "xml", sep = ".")
    if(! is.null(path)){
        idno_file <- file.path(path, idno_file)
    }

    utils::download.file(url = idno_url, destfile = idno_file, method = "curl")

    return(idno_file)
}




#' Create survey from DDI file
#'
#' Imports a DDI XML file and its metadata creating a new survey.
#'
#' @return API call response
#'
#' @param xml_file Path to the DDI XML file.
#' @param rdf_file Path to the RDF file.
#' @param enum_collection The ID of the collection where the survey will be created. To see an up to date list of collections call mdl_collection_list().You can also use the corresponding enumerator, e.g.: mdl_enum_collection$WestAfrica
#' @param enum_survey_access_policy Specifies the access level to the data files. You can use the corresponding enumerator, e.g.: mdl_enum_survey_access_policy$'Licensed use files'
#' @param data_remote_url Link to the data files, in case enum_survey_access_policy is set to remote (link to external repository).
#' @param published The survey status: FALSE for draft, TRUE for published.
#' @param overwrite Specifies if the survey will be overwritten in case it already exists: FALSE for not overwriting, TRUE for overwriting. If a survey with the same idno already exists and the argument is set to FALSE, the survey will not change and an error will be returned.
#'
#' @export
mdl_survey_import_ddi <- function(xml_file, rdf_file = NULL, enum_collection, enum_survey_access_policy, data_remote_url = NULL, published = FALSE, overwrite = FALSE){

    if(!is.null(enum_survey_access_policy) && enum_survey_access_policy == mdl_enum_survey_access_policy$`Data available from external repository (link)` && is.null(data_remote_url)){
        stop("enum_survey_access_policy is set to remote, but data_remote_url was not specified.")
    }

    # define parameters
    opt_published <- as.numeric(published)
    opt_overwrite <-  "no"
    if(identical(overwrite, TRUE) || identical(overwrite, "yes")){
        opt_overwrite <- "yes"
    }


    # remove some parts of XML that cause errors
    bug_text <- 'frameUnit isPrimary'
    file_text  <- readLines(xml_file)
    file_text  <- gsub(pattern = bug_text, replacement = "frameUnit XXXXX", x = file_text)
    writeLines(text = file_text, con = xml_file)


    # define options
    options = list(
        file = httr::upload_file(xml_file),
        overwrite = opt_overwrite,
        published = opt_published,
        repositoryid = enum_collection,
        access_policy = enum_survey_access_policy,
        data_remote_url = data_remote_url
    )

    if (!is.null(rdf_file) && file.exists(rdf_file)){
        options$rdf = httr::upload_file(rdf_file)
    }

    # specify url
    url <-  paste(mdl_api_get_url(), "datasets", "import_ddi", sep = "/")

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



#' Create survey from metadata list
#'
#' Creates or overwrites a survey in the MDL starting from a list containing the survey metadata. The survey created will not include the variables metadata, which should be created afterwards.
#'
#' @return API call response
#'
#' @param survey_metadata_list A list containing the metadata. We recommend to create the list using the function mdl_survey_generate_metadata_list.
#' @param enum_collection The ID of the collection where the survey will be created. To see an up to date list of collections call mdl_collection_list().You can also use the corresponding enumerator, e.g.: mdl_enum_collection$WestAfrica
#' @param enum_survey_access_policy Specifies the access level to the data files. You can use the corresponding enumerator, e.g.: mdl_enum_survey_access_policy$'Licensed use files'
#' @param data_remote_url Link to the data files in case enum_survey_access_policy is set to remote (link to external repository). Use the whole address (including https://) to make it work.
#' @param published The survey status: FALSE for draft, TRUE for published.
#' @param overwrite Specifies if the survey will be overwritten in case it already exists: FALSE for not overwriting, TRUE for overwriting. If a survey with the same idno already exists and the argument is set to FALSE, the survey will not change and an error will be returned.
#'
#' @export
mdl_survey_create <- function(survey_metadata_list, enum_collection, enum_survey_access_policy, data_remote_url = NULL, published = FALSE, overwrite = FALSE){

    if(enum_survey_access_policy == mdl_enum_survey_access_policy$`Data available from external repository (link)` && is.null(data_remote_url)){
        stop("enum_survey_access_policy is set to remote, but data_remote_url was not specified.")
    }

    # define parameters
    a_metadata_idno = survey_metadata_list$study_desc$title_statement$idno # this is probably not needed but required in the documentation
    opt_published <- as.numeric(published)
    opt_overwrite <-  "no"
    if(identical(overwrite, TRUE) || identical(overwrite, "yes")){
        opt_overwrite <- "yes"
    }

    # call
    response <- survey_create(type = "survey",
                              idno = a_metadata_idno,
                              metadata = survey_metadata_list,
                              repositoryid = enum_collection,
                              access_policy = enum_survey_access_policy,
                              data_remote_url = data_remote_url,
                              published = opt_published,
                              overwrite = opt_overwrite
                              )
    # return
    return(response)
}

# generic creation function, can be used also for other entities: survey, geospatial, table, document, timeseries
survey_create <- function(
    type,
    idno,
    metadata,
    repositoryid=NULL,
    access_policy=NULL,
    data_remote_url=NULL,
    published=NULL,
    overwrite=NULL,
    thumbnail=NULL
    ){

    api_key <- mdl_api_get_key()

    options <- list(
        "idno"=idno,
        "repositoryid"=repositoryid,
        "access_policy"=access_policy,
        "data_remote_url"=data_remote_url,
        "published"=published,
        "overwrite"=overwrite
    )

    options <- c(options,metadata)

    url <- paste(mdl_api_get_url(), "datasets", "create", type, idno, sep = "/" )

    httpResponse <- httr::POST(url,
                               httr::add_headers("X-API-KEY" = api_key),
                               httr::user_agent(mdl::mdl_api_get_user_agent()),
                         body=options,
                         #httr::content_type_json(),
                         encode="json"
                         )


    #thumbnail_result=NULL

    # #upload thumbnail
    # if(!is.null(thumbnail) && file.exists(thumbnail)) {
    #     thumbnail_result=thumbnail_upload(idno=idno,thumbnail = thumbnail)
    # }
    #
    # #set default thumbnail
    # if(!is.null(thumbnail) && thumbnail == 'default'){
    #     thumbnail_result= thumbnail_delete(idno=idno)
    # }

    response_content <- httr::content(httpResponse, "text")

    if(httpResponse$status_code!=200){
        warning(response_content)
    }

    output <- jsonlite::fromJSON(response_content)

    return (output)
}



# TEST
# a_survey_metadata <-  mdl_survey_generate_metadata_list(
#     survey_idno = "unhcr_test_v2.1", # UNHCR_ETH_2020_SENS_V2.1
#     title = "title of my survey", # include year at the end but not country name
#     country_ISO_alpha3_codes = c("ETH"),
#     enum_survey_study_type = "",
#     series_information = "",
#     abstract = "Abstract of my survey",
#     enum_survey_kind_of_data = "",
#     unit_of_analysis = "hh and ind",
#     version_description = "v2.1: Edited, cleaned and anonymised data.", #
#     version_date = "", # yyyy-mm-dd
#     scope_description = "The scope includes: hh characteristics, dwellings",
#     enum_survey_topics = c("topic1", "topic2"),
#     keywords = c("key1", "key2"),#c("key1", "key2"),
#     geographic_coverage = "",
#     universe = "",
#     primary_investigators = "WFP", # c("UNHCR"), # c()
#     other_producers = c(""),
#     sampling_description = "Simple sampling was applied",
#     weighting_description = "Weight was calculated",
#     collection_date_start = "2020-12-12",
#     collection_date_end = "2021-01-01",
#     enum_survey_collection_mode = "enum collection mode",
#     data_collection_notes = "",
#     data_collectors = c("collector 1", "collector2"),
#     questionnaire_description = "questionaire contained the following ",
#     contacts_list = list(list(name = "Curation team", affiliation = "UNHCR", email = "microdata@unhcr.org"),
#                          list(name = "Curation team2", affiliation = "UNHCR2", email = "microdata2@unhcr.org"))
# )



#' Delete a survey given an idno
#'
#' Given an unique idno, deletes the survey
#'
#' @return API call response.
#'
#' @param survey_idno Survey unique identifier
#'
#' @export
mdl_survey_delete <- function(survey_idno){

    url <- paste(mdl_api_get_url(), 'datasets', survey_idno, sep = "/")

    httpResponse <- httr::DELETE(url,
                              httr::add_headers("X-API-KEY" = mdl_api_get_key()),
                              httr::user_agent(mdl::mdl_api_get_user_agent())
    )

    response_content <- httr::content(httpResponse, "text")

    if(httpResponse$status_code!=200){
        warning(response_content)
    }

    output <- jsonlite::fromJSON(response_content)

    return (output)
}


#' Get url to survey
#'
#' Given an unique idno, it returns the link to that survey. Please note that this will be redirected to another url format that uses an internal id instead of the idno.
#'
#' @return Url to dataset.
#'
#' @param survey_idno Survey unique identifier
#'
#' @export
mdl_survey_url <- function(survey_idno){

    api_url <- mdl_api_get_url()
    home_url <- gsub( "/api", "", api_url)
    dataset_url <- paste(home_url, "catalog", "study", survey_idno, sep = "/")

    return(dataset_url)
}


#' Get survey internal ID
#'
#' Given an unique idno, it returns the internal id.
#'
#' @return Dataset internal ID.
#'
#' @param survey_idno Survey unique identifier
#'
#' @export
mdl_survey_internal_id <- function(survey_idno){

    dataset_url <- mdl::mdl_survey_url(survey_idno)
    dataset_internal_id <- basename(httr::GET(dataset_url, httr::user_agent(mdl::mdl_api_get_user_agent()) )$url)

    return(dataset_internal_id)
}


#' Get a survey given an idno
#'
#' Given an unique idno, it return a survey and its metadata
#'
#' @return API call response.
#'
#' @param survey_idno Survey unique identifier
#'
#' @export
mdl_survey_get <- function(survey_idno){

    url <- paste(mdl_api_get_url(), 'datasets', survey_idno, sep = "/")

    httpResponse <- httr::GET(url,
                              httr::add_headers("X-API-KEY" = mdl_api_get_key()),
                              httr::user_agent(mdl::mdl_api_get_user_agent())
                              )

    response_content <- httr::content(httpResponse, "text")

    if(httpResponse$status_code!=200){
        warning(response_content)
    }

    output <- jsonlite::fromJSON(response_content)

    return (output)
}


#' Get survey list
#'
#' Gets list of surveys with main info
#'
#' @return API call response.
#'
#' @param limit Max number of datasets to get. Default is 50.
#' @param offset Point from which the query will start, to be used if you want to used paging. Default is 0.
#'
#' @export
mdl_survey_list <- function(limit = 50, offset = 0){


    url <- paste(mdl_api_get_url(), 'datasets', sep = "/")
    url <- paste(url, paste0("limit=", limit), sep = "?")
    url <- paste(url, paste0("offset=", offset), sep = "&")

    httpResponse <- httr::GET(url,
                              httr::add_headers("X-API-KEY" = mdl_api_get_key()),
                              httr::user_agent(mdl::mdl_api_get_user_agent()),
                              encode = "json"
    )

    response_content <- httr::content(httpResponse, "text")

    if(httpResponse$status_code!=200){
        warning(response_content)
    }

    output <- jsonlite::fromJSON(response_content)

    return (output)
}


#' Set survey options
#'
#' Can set various survey options with a PUT call.
#' If you want to set linked/secondary collections, it is recommended to use mdl_survey_attach_to_collections().
#'
#' @return API call response.
#'
#' @param survey_idno Dataset IDNo.
#' @param enum_survey_access_policy Access policy. It is recommended to use mdl_enum_survey_access_policy.
#' @param data_remote_url Url to data in case access policiy was set to Data available from external repository ("remote")
#' @param published Set Dataset publish status. 0=draft, 1=published.
#' @param enum_collection Main collection of the dataset. It is recommended to use mdl_enum_collection.
#' @param linked_collections Array containing other secondary collections in which the dataset has to be shown.
#' @param tags Array of string tags.
#' @param aliases Array of strings with dataset aliases
#' @param link_study URL for study website
#' @param link_indicator URL to the indicators website
#'
#'
#' @export
mdl_survey_options <- function(
    survey_idno,
    enum_survey_access_policy = NULL,
    data_remote_url = NULL,
    published = NULL,
    enum_collection = NULL,  #not sure it works
    linked_collections = NULL, #not sure it works
    tags = NULL, # does not work
    aliases = NULL, # does not work
    link_study = NULL,
    link_indicator = NULL
){
    # published to be passed as 0 or 1
    published <- as.numeric(published)

    # specify call options
    options <- list(
        access_policy = enum_survey_access_policy,
        data_remote_url = data_remote_url,
        published = published,
        tags = tags,
        aliases = aliases,
        owner_collection = enum_collection,
        linked_collections = linked_collections,
        link_study = link_study,
        link_indicator = link_indicator
    )

    # specify url
    url <-  paste(mdl_api_get_url(), "datasets", survey_idno, sep = "/")

    # call API
    httpResponse <- httr::PUT(url,
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





#' Attach studies to collections
#'
#' Used to modify the main collection and/or add the secondary collections.
#' In case you only want to change the main collection it is recommended to use mdl_survey_options(), or you can try to set link_collections to the same value of enum_collection.
#'
#' @return API call response.
#'
#' @param survey_idno Dataset IDNo.
#' @param enum_collection Main collection of the dataset (optional). It is recommended to use mdl_enum_collection.
#' @param link_collections Array containing other secondary collections in which the dataset has to be shown (required). It is recommended to use mdl_enum_collection.
#' @param mode Select flag to update or replace existing linked collections for the study: "replace" = replace linked collections for the study with the provided list; "update" = (Default) add/update linked collections list.
#'
#'
#' @export
mdl_survey_attach_to_collections <- function(
    survey_idno,
    enum_collection = NULL,
    link_collections,
    mode = "update"

){

    # specify call options
    options <- list(
        study_idno = survey_idno,
        owner_collection = enum_collection,
        link_collections = as.list(link_collections),
        mode = mode
    )

    # specify url
    url <-  paste(mdl_api_get_url(), "datasets", "collections", sep = "/")

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







