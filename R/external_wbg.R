#' Harvest from external NADA
#'
#' Copies a dataset from another NADA microdata library to the UNHCR MDL.
#' By default, harvests from the WBG library (https://microdata.worldbank.org). Changing the parameters, the harvest should work with other NADA based libraries (given other implementations and versions it may not work though).
#'
#' @return API call response.
#'
#' @param library_url The url of the library, for example: "https://microdata.worldbank.org"
#' @param survey_idno Unique survey identifier in NADA
#' @param base_path The path on your computer where the files will be downloaded, if not specified uses the current working directory
#' @param upload_files TRUE if you want to upload a copy of the documentation files; FALSE if you want just to put a direct link to the files
#' @param idno_prefix A string to be used as prefix of the idno, so that it can be easily recognized as harvested.
#' @param enum_collection The collection where copy the dataset. Please consider using mdl_enum_collection.
#' @param overwrite TRUE if you want to overwrite the dataset in case it already exists; FALSE if you want to avoid to overwrite
#' @param published TRUE if you want to publish directly the dataset; FALSE if you want just to load it in the back-end
#'
#' @details Warning messages about incomplete final line found in readLines is not a concern. Please note that files get downloaded only if not present in the base_path folder: to have up to date files delete them.
#'
#' @export
mdl_harvest_nada <- function(
    library_url = "https://microdata.worldbank.org",
    survey_idno,
    base_path = NULL,
    upload_files = FALSE,
    idno_prefix = "WBG",
    enum_collection = mdl_enum_collection$WorldBank,
    overwrite,
    published
){
    # files with these extensions will not be uploaded anyway
    no_upload_extensions <- c("", "php")

    # set base path
    if(is.null(base_path)){
        base_path <- getwd()
    }

    # start message
    print(paste("Processing", survey_idno))
    idno <- survey_idno

    # Set folders
    survey_path <- file.path(base_path, idno)
    doc_path <- file.path(survey_path, "Doc")
    # Create the folders if they do not exist
    if (file.exists(survey_path)){
        if (!file.exists(doc_path)){
            dir.create(doc_path)
        }
    } else {
        dir.create(survey_path)
        dir.create(doc_path)
    }

    # get the internal dataset id
    dataset_id <- mdl::mdl_survey_internal_id(survey_idno)

    # Download the metadata files (ddi and rdf files + json version of rdf)
    url1 <- paste0(library_url, "/index.php/api/catalog/ddi/", idno)  # alternatively this may work:  url1 <- paste0(library_url, "/index.php/metadata/export/", dataset_id, "/ddi")
    url2 <- paste0(library_url, "/index.php/api/catalog/rdf/", idno)
    url3 <- paste0(library_url, "/index.php/api/catalog/resources/", idno) # alternatively this may work:  url3 <- paste0(library_url, "/index.php/metadata/export/", dataset_id, "/json")
    file1 <- paste0(survey_path, "/", idno, ".xml")
    file2 <- paste0(survey_path, "/", idno, ".rdf")
    file3 <- paste0(survey_path, "/", idno, ".json")
    print(url1)
    if(! file.exists(file1)){
        utils::download.file(url = url1, destfile = file1, method = "curl")
    }
    if(! file.exists(file2)){
        utils::download.file(url = url2, destfile = file2, method = "curl")
    }
    if(! file.exists(file3)){
        utils::download.file(url = url3, destfile = file3, method = "curl")
    }

    print("ready to get info")
    # Extract the dataset access policy
    url <- paste0(library_url, "/index.php/api/catalog/", idno, "?id_format=idno")
    httpResponse <- httr::GET(url, httr::user_agent(mdl::mdl_api_get_user_agent()))
    print("httpResponse") ###
    output = jsonlite::fromJSON(httr::content(httpResponse, "text"))
    print("output") ###
    # If remote access in WB Microdata Library, we link to the repository of origin.
    if(identical(output$dataset$data_access_type, "remote")) {
        remote_url = output$dataset$remote_data_url
    } else {
        # Otherwise, we link to the WB Microdata Library survey page.
        remote_url = paste0(library_url, "/index.php/catalog/study/", idno)
    }
    print("set remote link")###


    # check if should upload files or just link
    rdf_file_path <- NULL
    if(! identical(upload_files, TRUE)){
        rdf_file_path <- file2
    }

    # if the idno does not start with the desired prefix, I will have to modify the XML file
    if(! is.null(idno_prefix) && !is.na(idno_prefix)){
        new_idno <- paste(idno_prefix, idno, sep = "_")
        file_text  <- readLines(file1)
        # modify id if necessary
        if(! sum( grepl(pattern = new_idno, x = file_text, fixed = TRUE), na.rm = TRUE) > 0){
            file_text  <- gsub(pattern = idno, replacement = new_idno, x = file_text)
            writeLines(text = file_text, con = file1)
        }
        idno <- new_idno
    }
    print("prefix to idno was set")###

    # remove some parts of XML that cause errors
    bug_text <- 'frameUnit isPrimary'
    file_text  <- readLines(file1)
    file_text  <- gsub(pattern = bug_text, replacement = "frameUnit XXXXX", x = file_text)
    writeLines(text = file_text, con = file1)

    # Create survey
    create_response <- mdl_survey_import_ddi(xml_file = file1,
                                             rdf_file = rdf_file_path,
                                             enum_collection = enum_collection,
                                             enum_survey_access_policy = mdl_enum_survey_access_policy$`Data available from external repository (link)`,
                                             data_remote_url = remote_url,
                                             published = published,
                                             overwrite = overwrite)

    print("created response")###


    if(!identical(create_response$status, "success")){
        print(create_response)
        message("Error when trying to create: ", idno)
    }

    # create variables list only is below a certain threshold
    list_variables_in_pdf <- 0
    if(!is.null(create_response$survey$varcount) && create_response$survey$varcount < 3000){
        list_variables_in_pdf <- 1
    }

    #add documentation pdf
    generate_pdf_response <- mdl_survey_generate_pdf(idno, variable_list = list_variables_in_pdf, variable_description = list_variables_in_pdf)
    # if error, create it without variables
    if(!identical(generate_pdf_response$status, "success")){
        message("Error generating PDF documentation probably due to too large number of variables. Trying to create the PDF without vars...")
        generate_pdf_response2 <- mdl_survey_generate_pdf(idno, variable_list = 0, variable_description = 0)
        if(identical(generate_pdf_response2$status, "success")){
            message("...success generating the PDF without vars")
        }else{
            message("...failed again generating the PDF")
        }
    }

    # Download the external resources (save in /Doc folder)
    res <- jsonlite::fromJSON(file3)
    if(upload_files && res$total > 0) {  # If there is at least one resource
        for (i in 1:length(res$resources$url)) {
            if(res$resources$dctype[i] != "Microdata File [dat/micro]") {
                res_download_path <-  file.path(doc_path, basename(res$resources$url[i]))
                #print(res_download_path)
                if(!file.exists(res_download_path)) {
                    utils::download.file(res$resources$url[i], res_download_path, method = "curl")
                }
            }
        }
    }

    # upload if at least a file
    if(upload_files && res$total > 0) {
        res_df <- res$resources
        uploaded_files <- c()
        added_resources <- c()

        # iterate over files
        for(i in 1:nrow(res_df)){

            # upload only if not a data file
            if(res_df$dctype[i] != "Microdata File [dat/micro]") {

                file_path <- file.path(doc_path, basename(res_df$url[i]))

                # if not already, create a resource for the file
                if(!file_path %in% uploaded_files){

                    # Check if file exists and its size is greater than 0
                    if(file.exists(file_path) && file.size(file_path) > 0  && ! tools::file_ext(file_path) %in% no_upload_extensions ){

                        print(paste("uploading:", basename(res_df$url[i])))

                        # upload file, and if successful add to list of uploaded files
                        resource_upload_response <- mdl_resource_upload_file_and_link(
                            survey_idno = idno,
                            file_path = file_path,
                            enum_resource_type = res_df$dctype[i],
                            title = res_df$title[i],
                            enum_resource_format = res_df$dcformat[i],
                            description = res_df$description[i],
                            author = res_df$author[i],
                            dcdate = res_df$dctype[i],
                            country = res_df$country[i],
                            language = res_df$language[i],
                            contributor = res_df$contributor[i],
                            publisher = res_df$publisher[i],
                            rights = res_df$rights[i],
                            abstract = res_df$abstract[i],
                            toc = res_df$toc[i],
                            overwrite = TRUE
                        )
                        if(identical(resource_upload_response$status, "success")){
                            uploaded_files <- c(uploaded_files, file_path)
                        }
                    }

                    # Only create a resource with a link if did not managed to upload file
                    if(!file_path %in% uploaded_files){
                        # create resource
                        resource_add_response <- mdl_resource_add(
                            survey_idno = idno,
                            filename = res_df$url[i],
                            dctype = res_df$dctype[i],
                            title = res_df$title[i],
                            dcformat = res_df$dcformat[i],
                            description = res_df$description[i],
                            author = res_df$author[i],
                            dcdate = res_df$dctype[i],
                            country = res_df$country[i],
                            language = res_df$language[i],
                            contributor = res_df$contributor[i],
                            publisher = res_df$publisher[i],
                            rights = res_df$rights[i],
                            abstract = res_df$abstract[i],
                            toc = res_df$toc[i],
                            overwrite = "yes"
                        )
                        if(identical(resource_add_response$status, "success")){
                            uploaded_files <- c(uploaded_files, file_path)
                        }
                    }
                }
            }
        }
    }


    # final message
    print(paste(idno, " added to UNHCR Microdata Library"))

    # Check if country named as in MDL
    for(a_nation in output$dataset$metadata$study_desc$study_info$nation$name){
        if(! a_nation %in% mdl_enum_country){
            message("Country not found in MDL list, please check and create an alias if necessary: ", a_nation," (", idno,")")
        }
    }

    cat("\n\n")

    return(create_response)

}



