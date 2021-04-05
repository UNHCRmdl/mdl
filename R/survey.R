# Create metadata list to create a survey
#' @export
mdl_survey_generate_metadata_list <- function(
    survey_idno, # UNHCR_ETH_2020_SENS_V2.1
    title,
    country_ISO_alpha3_codes, # c()
    enum_survey_study_type = "",
    series_information = "",
    abstract,
    enum_survey_kind_of_data = "",
    unit_of_analysis,
    version_description, #
    version_date = "", # yyyy-mm-dd
    scope_description,
    enum_survey_topics,
    keywords = c(""),
    geographic_coverage = "",
    universe = "",
    primary_investigators, # c()
    other_producers = c(""),
    sampling_description,
    weighting_description,
    collection_date_start,
    collection_date_end,
    enum_survey_collection_mode,
    data_collection_notes = "",
    data_collectors,
    questionnaire_description,
    contacts_list = list(list(name = "Curation team", affiliation = "UNHCR", email = "microdata@unhcr.org"))
){

    # create countries
    country_ISO_alpha3_codes <- toupper(country_ISO_alpha3_codes)
    country_names <- sapply(country_ISO_alpha3_codes, function(x){return (mdl_enum_country[[x]])})


    # create citation
    # countries
    countries_string <- paste0(country_names[1], "")
    if(length(country_names) > 1){
        countries_string <- paste0(paste0(country_names[1:length(country_names)-1], ", "), country_names[length(country_names)], "")
    }
    # organizations
    investigators_string <- paste0(primary_investigators[1], "")
    if(length(primary_investigators) > 1){
        investigators_string <- paste0(paste0(primary_investigators[1:length(primary_investigators)-1], ", "), primary_investigators[length(primary_investigators)], "")
    }
    # citation string
    a_survey_citation <- paste0(investigators_string, ": ", countries_string, " - ", title, ". ", "UNHCR microdata library, https://microdata.unhcr.org")


    # create metadata list
    a_survey_metadata <- list(

        doc_desc=list(
            idno = survey_idno,
            title = gsub("[^A-Za-z0-9]", "_", title),
            prod_date = as.character(Sys.Date()),
            producers=list(
                list(
                    name = "UNHCR",
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
                    cit_req = a_survey_citation,
                    contact = contacts_list
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
        a_survey_metadata$study_desc$production_statement$producers <- NULL
    }

    # return
    return(a_survey_metadata)
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
# a_survey_metadata_create_response <- nadar::create("survey", "bbb", a_survey_metadata, overwrite = "yes", published = 0)
