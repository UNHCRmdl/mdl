# Get list of data files metadata in a survey.
survey_get_data_file_list <- function(
    survey_idno
){

    # specify url
    url <-  paste(mdl_api_get_url(), "datasets", "datafiles", survey_idno, "data_files", sep = "/")

    # call API
    httpResponse <- httr::GET(url,
                                 httr::add_headers("X-API-KEY" = mdl_api_get_key()),
                                 #body = options,
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

# Delete a data file metadata in a survey.
survey_delete_data_file <- function(
    survey_idno,
    file_id
){
    # # before deleting you may check that it exists
    # data_files_list <- survey_get_data_file_list(survey_idno)
    # if(! file_id %in% names(files_list$data_files_list)){
    #     return()
    # }

    # specify url
    url <-  paste(mdl_api_get_url(), "datasets", "datafiles", survey_idno, file_id, sep = "/")

    # call API
    httpResponse <- httr::DELETE(url,
                                 httr::add_headers("X-API-KEY" = mdl_api_get_key()),
                                 #body = options,
                                 encode = "json"
    )

    # I commented the code to avoid possible errors coming up if the file did not exist
    # response_content <- httr::content(httpResponse, "text")
    #
    # output <- jsonlite::fromJSON(response_content)
    # if(!is.list(output)){
    #     output <- list(output)
    # }
    #
    # if(httpResponse$status_code!=200){
    #     warning(response_content)
    # }
    #
    # return (output)
}


# Create a data file metadata in a survey. Once a file is create, you add variables metadata to it.
survey_create_data_file <- function(
    survey_idno,
    file_id,
    file_name,
    description,
    case_count,
    var_count,
    producer = NULL,
    data_checks = NULL,
    missing_data = NULL,
    version = NULL,
    notes = NULL
){
    # delete, just in case, the data file metadata so that each time this function is called it can create a new data file and overwrite the previouse one
    delete_response <- survey_delete_data_file(survey_idno, file_id)

    # specify call options
    options = list(
        file_id = file_id,
        file_name = file_name,
        description = description,
        case_count = case_count,
        var_count = var_count,
        producer = producer,
        data_checks = data_checks,
        missing_data = missing_data,
        version = version,
        notes = notes
    )

    # specify url
    url <-  paste(mdl_api_get_url(), "datasets", "datafiles", survey_idno, sep = "/")

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

### test
# survey_create_data_file_response <- survey_create_data_file(survey_idno = "UNHCR_ETH_2021_TEST_v2.1",
#                                                             file_id = "file_test_3",
#                                                             file_name = "file name 3",
#                                                             description = "file descr",
#                                                             case_count = 100,
#                                                             var_count = 300)


###############

# Create a variable from a list of metadata. Can create more than one variable at once passing a list of metadata
survey_create_variable <- function(
    survey_idno,
    #file_id,
    var_metadata
){

    # specify call options
    options <- var_metadata


    # specify url
    url <-  paste(mdl_api_get_url(), "datasets", "variables", survey_idno, sep = "/")

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

# ###TEST
# variable_create_options1 <- list(
#     vid = "V1",
#     name = "var_name1",
#     labl = "var label1",
#     file_id = "file_test_3"
# )
#
# variable_create_options2 <- list(
#     vid = "asdasd_dsadas",
#     name = "var_name 5",
#     labl = "var label 2",
#     file_id = "file_test_3"
# )
# variable_create_options3 <- list(
#     vid = "v3",
#     name = "var_name 3",
#     labl = "var label 3"
# )
# variable_create_options_list <- list(variable_create_options1, variable_create_options2, variable_create_options3 )
#
# survey_create_variable_response <- survey_create_variable(    survey_idno = "UNHCR_ETH_2021_TEST_v2.1",
#                                                               #file_id = "file_test_3",
#                                                               var_metadata = variable_create_options_list)


#########

#' Create variables metadata from dta file
#'
#' Starting from a Stata file (.dta), this function will create the variables metadata for a given survey.
#' The function will calculate for each variable the main stats and get the variable labels when available.
#'
#' @return API call response.
#'
#' @param survey_idno The unique identifier of an existing survey, e.g.: UNHCR_ETH_2020_SENS_v2.1
#' @param file_path Path to the dta file, e.g.: C:/path/to/file.dta
#' @param file_id An unique identifier for the file containing the variables metadata. If not provided, the name of the file will be used. If provided, the ID should only include alphanumeric chars. This ID is not shown in the platform, it is mostly used for fetching or modifying the metadata in a second moment.
#' @param file_name The name of the file containing the variables as you want it to be shown in the platform to the final user.
#' @param file_description Brief description of the file, as you want it to be shown in the platform to the final user.
#'
#' @export
mdl_vars_create_from_dta <- function(survey_idno, file_path, file_id = NA, file_name, file_description){

    # if file id not provided, uses the name of the file
    if(is.na(file_id)){
        file_id <- basename(file_path) # get only file name
        file_id <- gsub(pattern = "\\.dta$", "", file_id) # remove extension
        file_id <- gsub("[^A-Za-z0-9]", "_", file_id) # replace special chars with underscore
    }

    data_frame <- haven::read_dta(file_path)
    mdl_vars_create_from_dataframe(survey_idno = survey_idno, data_frame = data_frame, file_id = file_id, file_name = file_name, file_description = file_description)
}


#' Create variables metadata from a data frame
#'
#' Starting from a data frame, this function will create the variables metadata for a given survey.
#' The function will calculate for each variable the main stats and get the variable labels when available.
#' Labels must be set to the data frame using the labelled::var_label function or the data frame must be imported from a Stata file using the haven::read_dta function.
#'
#' @return API call response.
#'
#' @param survey_idno The unique identifier of an existing survey, e.g.: UNHCR_ETH_2020_SENS_v2.1
#' @param data_frame A data frame containing the data.
#' @param file_id An unique identifier for the file containing the variables metadata. If not provided, the name of the file will be used. If provided, the ID should only include alphanumeric chars. This ID is not shown in the platform, it is mostly used for fetching or modifying the metadata in a second moment.
#' @param file_name The name of the file containing the variables as you want it to be shown in the platform to the final user.
#' @param file_description Brief description of the file, as you want it to be shown in the platform to the final user.
#'
#' @export
mdl_vars_create_from_dataframe <- function(survey_idno, data_frame, file_id, file_name, file_description){

    #create data file that will contain the variables
    survey_create_data_file(
        survey_idno = survey_idno,
        file_id = file_id,
        file_name = file_name,
        description = file_description,
        case_count = nrow(data_frame),
        var_count = ncol(data_frame)
    )

    # iterate over variables
    var_options_list <- list()
    for(i in 1:ncol(data_frame)){
        #print(i)

        # get basic info
        a_var <- data_frame[[i]]
        a_var_class <- class(a_var)[1]
        a_var_id <- paste0(file_id, "_V", i)
        a_var_name <- names(data_frame)[i]
        a_var_label <- labelled::var_label(a_var, unlist = TRUE)
        a_var_label <- ifelse(is.null(a_var_label), "", a_var_label)

        # set basic options
        a_var_options <- list(
            vid = a_var_id,
            name = a_var_name,
            labl = a_var_label,
            file_id = file_id
        )

        ##### CHARACTER
        if(a_var_class == "character"){

            a_var_options$var_intrvl = "discrete"
            a_var_options$var_format = list(type = "Character")
            a_var_options$var_sumstat = data.frame(value  = c(sum(!is.na(a_var)), sum(is.na(a_var))), type = c("vald", "invd"))
        }

        ##### FACTOR
        if(a_var_class %in% c("haven_labelled", "factor")){

            # Only difference between "haven_labelled" and "factor" is the value shown: if from haven we got to the actual value
            cat_values <- c()
            if(a_var_class == "haven_labelled"){
                cat_values <- as.character(labelled::val_labels(a_var))
            }else{
                cat_values <- as.character(1:nlevels(a_var))
            }

            # convert in case is from haven
            a_var <- haven::as_factor(a_var)

            # get basic info
            n_missing_values <-  sum(is.na(a_var))
            a_var_options$var_intrvl = "discrete"
            a_var_options$var_format = list(type = "Factor")
            a_var_options$var_sumstat = data.frame(value  = c(length(a_var) - n_missing_values, n_missing_values), type = c("vald", "invd"))
            if(sum(!is.na(a_var), na.rm = TRUE) > 0){ # add min and max if there is at least a value
                a_var_options$var_val_range = list(min = 1, max = nlevels(a_var))
            }

            # define first part of categories
            cat_table <- table(a_var)
            cat_labels <- names(cat_table)
            # should be the same length, if not probably there was a lacking labeling in stata for a var
            if( length(cat_labels) != length(cat_values)) {
                if( n_missing_values != length(a_var) ){
                    print(paste(("There was an issue with a variable values. Probably the value labelling in stata was not complete, not all values were labelled. Check variable: "), names(data_frame)[i]))
                    print(cat_values)
                    print(cat_labels)
                    cat("/n/n")
                }
                # do not show values
                cat_values <- rep("", length(cat_labels))
                }
            # add NAs if present
            cat_is_missing <- rep(NA, nlevels(a_var))
            if(n_missing_values > 0) {
                cat_values <- c(cat_values, "Missing value")
                cat_labels <- c(cat_labels, NA)
                cat_is_missing <- c(cat_is_missing, NA)#"Y")
            }


            # get frequency stats
            cat_freq <- as.vector(cat_table)
            cat_freq <- if(n_missing_values > 0){append(cat_freq, n_missing_values)}else{cat_freq}
            cat_stats <- list()
            for(a_freq in cat_freq){
                cat_stats <- c(cat_stats, list(data.frame(value = a_freq, type = "freq", wgtd = 0)))
            }

            # cat("\n")
            # print(names(data_frame)[i])
            # #print(a_var)
            # print(cat_values)
            # print(cat_labels)
            # print(cat_is_missing)
            # cat("\n\n")

            # create options list
            a_var_options$var_catgry <-
                data.frame(value = cat_values, labl = cat_labels, is_missing = cat_is_missing)
            a_var_options$var_catgry$stats <- cat_stats
        }

        ##### INTEGER
        if(a_var_class == "integer" || (a_var_class == "numeric" && isTRUE(all.equal(a_var, trunc(a_var))))){

            # get basic info
            n_missing_values <-  sum(is.na(a_var))
            a_var_options$var_intrvl = "discrete"
            a_var_options$var_format = list(type = "Integer")
            a_var_options$var_sumstat = data.frame(value  = c(length(a_var) - n_missing_values, n_missing_values), type = c("vald", "invd"))
            if(sum(!is.na(a_var), na.rm = TRUE)){ # add min and max if there is at least a value
                a_var_options$var_val_range = list(min = min(a_var, na.rm = TRUE), max = max(a_var, na.rm = TRUE))
            }

            # if there are only few numbers, then show frequency, otherwise not, it would be too many (for example IDs)
            if(length(unique(a_var)) <= 30) {

                # define first part of categories
                cat_table <- table(a_var)
                cat_values <- names(cat_table)
                cat_labels <- rep(NA, length(cat_values))
                cat_is_missing <- rep(NA, length(cat_values))
                if(n_missing_values > 0) {
                    cat_values <- c(cat_values, "Missing value")
                    cat_labels <- c(cat_labels, NA)
                    cat_is_missing <- c(cat_is_missing, NA)#"Y")
                }

                # get frequency stats
                cat_freq <- as.vector(cat_table)
                cat_freq <- if(n_missing_values > 0){append(cat_freq, n_missing_values)}else{cat_freq}
                cat_stats <- list()
                for(a_freq in cat_freq){
                    cat_stats <- c(cat_stats, list(data.frame(value = a_freq, type = "freq", wgtd = 0)))
                }

                # add frequencies to options
                a_var_options$var_catgry <- data.frame(value = cat_values, labl = cat_labels, is_missing = cat_is_missing)
                a_var_options$var_catgry$stats <- cat_stats
            }

        }

        ##### NUMERIC
        if(a_var_class == "numeric" && !isTRUE(all.equal(a_var, trunc(a_var))) ){

            # number of decimals to show for mean and st dev
            number_of_decimals <- 2

            # get basic info
            n_missing_values <-  sum(is.na(a_var))
            a_var_options$var_intrvl = "contin"
            a_var_options$var_format = list(type = "Numeric")
            if(sum(!is.na(a_var), na.rm = TRUE)){ # add min and max if there is at least a value
                a_var_options$var_val_range = list(min = min(a_var, na.rm = TRUE), max = max(a_var, na.rm = TRUE))
            }
            a_var_options$var_sumstat = data.frame(value  = c(length(a_var) - n_missing_values,
                                                              n_missing_values,
                                                              round(mean(a_var, na.rm = TRUE), digits = number_of_decimals),
                                                              round(sd(a_var, na.rm = TRUE), digits = number_of_decimals)),
                                                   type = c("vald", "invd", "mean", "stdev"))

        }

        ##### LOGICAL
        if(a_var_class == "logical"){

            # get basic info
            n_missing_values <-  sum(is.na(a_var))
            n_true <- sum(a_var, na.rm = TRUE)
            n_false <- length(a_var) - n_missing_values - n_true

            a_var_options$var_intrvl = "discrete"
            a_var_options$var_format = list(type = "Logical")
            a_var_options$var_sumstat = data.frame(value  = c(length(a_var) - n_missing_values, n_missing_values), type = c("vald", "invd"))

            # define first part of categories
            cat_table <- table(a_var)
            cat_values <- c("0", "1")
            cat_labels <- c("FALSE", "TRUE")
            cat_is_missing <- rep(NA, 2)

            # get frequency stats
            cat_stats <- list()
            cat_stats <- c(cat_stats, list(data.frame(value = n_false, type = "freq", wgtd = 0)))
            cat_stats <- c(cat_stats, list(data.frame(value = n_true, type = "freq", wgtd = 0)))
            if(n_missing_values > 0) {
                cat_values <- c(cat_values, "Missing value")
                cat_labels <- c(cat_labels, NA)
                cat_is_missing <- c(cat_is_missing, NA)#"Y")
                cat_stats <- c(cat_stats, list(data.frame(value = n_missing_values, type = "freq", wgtd = 0)))
            }

            #create options list
            a_var_options$var_catgry <- data.frame(value = cat_values, labl = cat_labels, is_missing = cat_is_missing)
            a_var_options$var_catgry$stats <- cat_stats

        }

        # add options to list of all vars options
        var_options_list <- c(var_options_list, list(a_var_options))
    }

    # create all vars
    survey_create_variable(survey_idno = survey_idno,
                           #file_id = file_id,
                           var_metadata = var_options_list)

}


### test

# test_data <- read.csv("test_file_variables_types.csv")
# test_data$factor_var <- as.factor(test_data$factor_var)
#
# vars_response <- mdl_vars_create_from_dataframe(survey_idno = "UNHCR_ETH_2021_TEST_v2.1",
#                                       data_frame = test_data,
#                                       file_id = "hh3",
#                                       file_name = "hh3",
#                                       file_description = "This file contains...")
#
# table(test_data$logical, useNA = "a")


###test
# xxx <- mdl_create_vars_from_dataframe(survey_idno = test_survey_idno,
#                                       data_frame = test_dta,
#                                       file_id = "hh3",
#                                       file_name = "hh3",
#                                       file_description = "This file contains...")
#
#
# mdl_create_vars_from_dta(survey_idno = test_survey_idno, file_path = "UNHCR_KEN_2018_LIS_data_v1.1.dta", file_name = "kenya lis", file_description = "kenya lis data file")
#
#
# xxx2 <- read.csv("test.csv")
# xxx2$var_factor <- as.factor(xxx2$var_factor)
# xxx2$var_integer <- c(1,1,1,2,NA)
# xxx2$var_logical <- c(F,NA,T,F,F)
# labelled::var_label(xxx2$var_integer) <- "integer label"
# xxx3 <- mdl_create_vars_from_dataframe(survey_idno = test_survey_idno,
#                                        data_frame = xxx2,
#                                        file_id = "hh5_v21dta",
#                                        file_name = "hh5 v2.1",
#                                        file_description = "This file contains data from dataframe...")
#
#
#
# test_lis <- haven::read_dta("UNHCR_ZMB_2018_LIS_data_v1.1.dta")
# test_lis_call <- mdl_create_vars_from_dataframe(survey_idno = "test_LIS",
#                                                 data_frame = test_lis,
#                                                 file_id = "hh",
#                                                 file_name = "Households",
#                                                 file_description = "This file contains household data")
#
#
# test_vasyr <- haven::read_dta("UNHCR_LBN_2020_VASYR_data_household_v2.1.dta")
# test_vasyr_call <- mdl_create_vars_from_dataframe(survey_idno = test_survey_idno,
#                                                   data_frame = test_vasyr,
#                                                   file_id = "hh_vasyr",
#                                                   file_name = "hh vasyr",
#                                                   file_description = "This file contains household vasyr data")
#
#
# table(haven::as_factor( test_dta$var_factor))
#
#
# class(test_dta$var_character)[1]
# class(test_dta$var_factor)[1]
#
# c(as.vector(table(haven::as_factor(test_dta$var_factor))), 4)
#
#
#
# all(test_dta$var_integer == trunc(test_dta$var_integer))
# all(test_dta$var_numeric == trunc(test_dta$var_numeric))
# all(test_dta$var_logical == trunc(test_dta$var_logical))
#
#
# isTRUE(all.equal(test_dta$var_integer, trunc(test_dta$var_integer)))
# isTRUE(all.equal(test_dta$var_numeric, trunc(test_dta$var_numeric)))
# isTRUE(all.equal(test_dta$var_logical, trunc(test_dta$var_logical)))
#
# isTRUE(all.equal(a_var, trunc(a_var)))
#
# all(test_dta$var_numeric == trunc(test_dta$var_numeric))





#' Get a a variable
#'
#' Fetches the metadata for a variable
#'
#' @return API call response.
#'
#' @param survey_idno Survey unique identifier
#' @param variable_id Variable ID
#'
#' @export
mdl_survey_get_variable <- function(survey_idno, variable_id){

    url <- paste(mdl_api_get_url(), 'datasets', "variable", survey_idno, variable_id, sep = "/")

    httpResponse <- httr::GET(url,
                              httr::add_headers("X-API-KEY" = mdl_api_get_key())
    )

    response_content <- httr::content(httpResponse, "text")

    if(httpResponse$status_code!=200){
        warning(response_content)
    }

    output <- jsonlite::fromJSON(response_content)

    return (output)
}

