#
# library(nadar)
# library(httr)
#
# # Enter folder where downloaded files will be saved, and image to be used as thumbnail
# base_path = "C:\\Users\\SANSON\\OneDrive - UNHCR\\Desktop\\nesstar"
#
# # -------------------------------------------------------------------------------------------------------------
# # Load the API credentials and catalog URL
# # -------------------------------------------------------------------------------------------------------------
# my_keys <- read.csv("C:/Users/SACKSFER/OneDrive - UNHCR/Documents/apikey.csv", header=F, stringsAsFactors=F)
# my_key1 = "201f836a23d3ba795d6c1a1542818ab1"
# set_api_key(my_key1)
# catalog_url <- "https://microdata.unhcr.org/index.php/api"
# set_api_url(catalog_url)
# set_api_verbose(FALSE)
# # -------------------------------------------------------------------------------------------------------------
#
# # List of surveys to be harvested
#
# list_idno = list(
#    # "ETH_2017_SPS_v01_M",
#    # "JOR_2015_SRHCS_v01_M",
#     #"JOR_2016_DR-BL_v01_M",
#     "BIH_2011_MICS-RS_v01_M"
# )
#
# # Select image to be used as thumbnail (use the same image for all; could be made specific to each survey)
# thumb_jpg = "C:\\Users\\SANSON\\OneDrive - UNHCR\\Desktop\\nesstar\\Capture.PNG"
#
# # Download and publish in UNHCR catalog
#
# for(idno in list_idno) {
#
#     print(paste("Processing", idno))
#
#     # Create the folders if they do not exist
#     svy_path <- paste0(base_path, idno)
#     doc_path <- paste0(svy_path, "/Doc")
#     if (file.exists(svy_path)){
#         setwd(svy_path)
#         if (!file.exists(doc_path)){
#             dir.create(doc_path)
#         }
#     } else {
#         dir.create(svy_path)
#         dir.create(doc_path)
#         setwd(svy_path)
#     }
#
#     # Download the metadata (ddi and rdf files + json version of rdf)
#     url1 <- paste0("https://microdata.worldbank.org/index.php/api/catalog/ddi/", idno)
#     url2 <- paste0("https://microdata.worldbank.org/index.php/api/catalog/rdf/", idno)
#     url3 <- paste0("https://microdata.worldbank.org/index.php/api/catalog/resources/", idno)
#     file1 <- paste0(idno, ".xml")
#     file2 <- paste0(idno, ".rdf")
#     file3 <- paste0(idno, ".json")
#     download.file(url = url1, destfile = file1, method = "curl")
#     download.file(url = url2, destfile = file2, method = "curl")
#     download.file(url = url3, destfile = file3, method = "curl")
#
#     # Download the external resources (save in /Doc folder)
#     res <- fromJSON(file3)
#     if(res$total > 0) {  # If there is at least one resource
#         for (i in 1:length(res$resources$url)) {
#             if(res$resources$dctype != "Microdata File [dat/micro]") {
#                 fn <- paste0("./Doc/", basename(res$resources$url[i]))
#                 if(!file.exists(fn)) {
#                     download.file(res$resources$url[i], fn, method = "curl")
#                 }
#             }
#         }
#     }
#
#     # Extract the dataset access policy
#     url <- paste0("https://microdata.worldbank.org/index.php/api/catalog/", idno, "?id_format=idno")
#     httpResponse <- GET(url)
#     output = fromJSON(content(httpResponse, "text"))
#
#     # If remote access in WB Microdata Library, we link to the repository of origin.
#     if(output$dataset$data_access_type == "remote") {
#         remote_url = output$dataset$remote_data_url
#     } else {
#         # Otherwise, we link to the WB Microdata Library survey page.
#         remote_url = paste0("https://microdata.worldbank.org/index.php/catalog/study/", idno)
#     }
#
#     # Publish the metadata and link
#     import_ddi(xml_file = file1,
#                rdf_file = file2,
#                repositoryid = "central",
#                overwrite = "yes",
#                access_policy = "remote",
#                data_remote_url = remote_url,
#                published = 1)
#
#     # Add a thumbnail
#     thumbnail_upload(idno = idno, thumbnail = thumb_jpg)
#
#     # Upload the resources files
#     if(res$total > 0) {  # If there is at least one external resource
#         for (i in 1:length(res$resources$url)) {
#             if(res$resources$dctype != "Microdata File [dat/micro]") {
#                 fn <- paste0("./Doc/", basename(res$resources$url[i]))
#                 external_resources_upload(idno, file=fn)
#             }
#         }
#     }
#
#     print(paste(idno, "added to UNHCR Microdata Library"))
#
# }
#
#
