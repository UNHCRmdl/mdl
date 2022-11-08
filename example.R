# set path to package
setwd("C:\\Users\\SANSON\\Documents\\GitHub\\mdl\\R")

# creates all files for the package to work
devtools::document()

# load the package like it was loaded with library()
devtools::load_all()

# specify api key
mdl_api_set_key(Sys.getenv("API_KEY_MDL"))
mdl_api_get_key()

# work on the UAT
mdl_api_set_url(mdl_enum_api_url$uat)
mdl_api_get_url()

# get collections to check if it works
collection_list <- mdl_collection_list()


# # to verify if it works with newer versions of NADA. Currently working with production but not with UAT
# mdl_survey_internal_id("UNHCR_AFG_2013_VOL_REP")
# mdl_survey_url("UNHCR_AFG_2013_VOL_REP")

# # copy a dataset from WBG
# harvest_response <- mdl_harvest_nada(
#     library_url = "https://microdata.worldbank.org",
#     survey_idno = "WBG_2010_MICS_v01_M",
#     base_path = "C:\\Users\\SANSON\\Downloads",
#     upload_files = TRUE,
#     idno_prefix = "WBG",
#     enum_collection = mdl_enum_collection$WorldBank,
#     overwrite = TRUE,
#     published = FALSE
# )


############
# GITHUB EXAMPLE
###########


# install the mdl package
#devtools::install_github("UNHCRmdl/mdl")

# create the metadata for a new survey
a_survey_metadata <-  mdl_survey_generate_metadata_list(
    survey_idno = "UNHCR_ETH_2021_TEST_v2.1",
    title = "A Test Survey, 2021",
    country_ISO_alpha3_codes = c("ETH"),
    enum_survey_study_type = mdl_enum_survey_study_type$`Sample Frame, Households [sf/hh]`,
    series_information = "A series test...",
    abstract = "This is a test survey create using APIs...",
    enum_survey_kind_of_data = mdl_enum_survey_kind_of_data$`Sample survey data [ssd]`,
    unit_of_analysis = "Household",
    version_description = "v2.1: Edited, cleaned and anonymised data.",
    version_date = "2021-07-04",
    scope_description = "The scope includes: \n- household characteristics \n- dwellings",
    enum_survey_topics = c(mdl_enum_survey_topics$Health, mdl_enum_survey_topics$Protection),
    keywords = c("Keyword 1", "Keyword 2", "Keyword 3"),
    geographic_coverage = "Whole country.",
    universe = "All people of concern.",
    primary_investigators = c("UNHCR"),
    other_producers = c("Another organisation"),
    sampling_description = "Simple sampling was applied...",
    weighting_description = "Weight was calculated in the following manner...",
    collection_date_start = "2020-12-12",
    collection_date_end = "2021-01-01",
    enum_survey_collection_mode = mdl_enum_survey_collection_mode$`Face-to-face [f2f]`,
    data_collection_notes = "Enumerators took a 3-day course...",
    data_collectors = c("Collector 1", "Collector2"),
    questionnaire_description = "Questionaire contained the following sections: ... ",
    contacts_list = list(list(name = "Curation team", affiliation = "UNHCR", email = "xxx@xxx.org"),
                         list(name = "Another team", affiliation = "UNHCR", email = "xxx2@xxx.org")),
    publication_year = 2021
)

# create the survey passing the metadata list
a_response <- mdl_survey_create(
    survey_metadata_list = a_survey_metadata,
    enum_collection = mdl_enum_collection$MENA,
    enum_survey_access_policy = mdl_enum_survey_access_policy$`Licensed use files`,
    published = FALSE,
    overwrite = TRUE
)

# get a test data frame and label it with package labelled
# alternatively, you can import a data frame from a Stata file using haven::read_dta
a_data_frame <- iris
labelled::var_label(a_data_frame) <- list(Sepal.Length =  "Length of sepal", Sepal.Width = "Width of sepal", Petal.Length = "Length of petal", Petal.Width = "Width of Petal", Species = "Species")

# creates the variables metadata for our survey starting from a data frame
# alternatively you can use mdl_vars_create_from_dta to use a Stata file instead
a_response <- mdl_vars_create_from_dataframe(
    survey_idno = "UNHCR_ETH_2021_TEST_v2.1",
    data_frame = a_data_frame,
    file_id = "hh",
    file_name = "Households",
    file_description = "This file contains the household data collected during the survey."
)

# create a test file
write.csv(iris, "test_file.csv")

# upload a file and create a resource linked to it
a_response <- mdl_resource_upload_file_and_link(
    survey_idno = "UNHCR_ETH_2021_TEST_v2.1",
    file_path = "test_file.csv",
    enum_resource_type = mdl_enum_resource_type$microdata,
    title = "Household data",
    description = "Contains the household data collected during the survey"
)



#### workaround to make the PDF generation work (but it does not includes the variables)

#test_id <- "UNHCR_ETH_2021_TEST_v2.1"

# # download the ddi file
# ddi_file <- mdl_survey_download_ddi(test_id)
#
# # upload the ddi
# a_response <- mdl_survey_import_ddi(
#     xml_file = ddi_file,
#     rdf_file = NULL,
#     enum_collection = NULL,
#     enum_survey_access_policy = NULL,
#     data_remote_url = NULL,
#     published = NULL,
#     overwrite = TRUE
# )
#
#
# # generate pdf
# a_response <- mdl_survey_generate_pdf(survey_idno = test_id)


