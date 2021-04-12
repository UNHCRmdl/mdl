# mdl
R client for UNHCR Microdata Library

## About
The UNHCR Microdata Library (MDL) is available at this link: https://microdata.unhcr.org/

The purpose of this package is provide an easy way to call MDL APIs. 

## Installation
Make sure you have installed the devtools package. You can than install the mdl package with this code:

``` r
# if not already, install devtools like this: 
# install.packages("devtools")

# install the mdl package
devtools::install_github("UNHCRmdl/mdl")
```


Once installed, you can load it at the beginning of each session like this:
```r
library(mdl)
```
You can then call directly the package's functions/objects. 
All functions/objects start with "mdl_" so that they are easy to find.


## Quick start tutorial

### Setup the API

Before starting to call any function, you will have to setup the API providing the key and the url.


The API key can can be found in your MDL profile (https://microdata.unhcr.org/index.php/auth/profile).


Apart from the production website (https://microdata.unhcr.org/), a user testing website is available for testing (https://microdata-uat.unhcr.org/). You can decide on which website to work providing the corresponding url.

```r
# setup API key
mdl_api_set_key(api_key = "XXXXXXXXXX")

# set the url to the user testing version of the MDL (https://microdata-uat.unhcr.org/)
mdl_api_set_url(enum_api_url = mdl_enum_api_url$uat)

#
# Do some testing here ....
#

# switch to the production platform (https://microdata.unhcr.org/)
mdl_api_set_url(enum_api_url = mdl_enum_api_url$production)
```

### Create a survey

To create a survey, first create a list with the metadata:

```r
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
                         list(name = "Another team", affiliation = "UNHCR", email = "xxx2@xxx.org"))
)
```


Secondly, pass the metadata list to the mdl_survey_create function to create a survey in the platform.
```r
# create the survey passing the metadata list
a_response <- mdl_survey_create(
    survey_metadata_list = a_survey_metadata,
    enum_collection = mdl_enum_collection$EastAfrica,
    enum_survey_access_policy = mdl_enum_survey_access_policy$`Licensed use files`,
    published = FALSE,
    overwrite = FALSE
)
```
Please note that this will only create the survey metadata. The variables metadata and the resources will be created in the following steps.


### Create the variables metadata

You can create the variables metadata with mdl_vars_create_from_dataframe starting from a data frame. The data frame variables should have labels set with labelled::var_label or the data frame should have been imported from a Stata file (.dta) using haven::read_dta.

Alternatively, you can read directly a Stata (.dta) file using mdl_vars_create_from_dta.

```r
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
```


### Upload files and create resources linked to them

You can upload files for a given survey specifying the kind of resource: microdata, report, questionnaire, etc. 

Please note that the resource files set as microdata are accessible accordingly to the survey access policy specified beforehand, at the moment of the survey creation.

```r
# create a test file
write.csv(iris, "test_file.csv")

# upload a file and create a resource linked to it
a_response <- mdl_resource_upload_file_and_link(
    survey_idno = "UNHCR_ETH_2021_TEST_v2.1",
    file_path = "test_file.csv",
    enum_resource_type = mdl_enum_resource_type$microdata,
    resource_title = "Household data",
    resource_description = "Contains the household data collected during the survey"
)

```



