library(httr)
library(readr)
library(jsonlite)

# Support function that check the apy.key and build the url
build.url <- function(endpt, query, api.key){
  if(api.key == "" || is.null(api.key)){
    stop("The TCIA Api Key is required!")
  }
  if(query != "")
    return(paste(endpt, "?", query, "&", "api_key=", api.key, sep=""))
  else
    return(paste(endpt, "?", "api_key=", api.key, sep=""))
}

#' Return the updated list of available body part names
#'
#' @title get.BodyPartValues
#'
#' @examples
#'
#' get.BodyPartValues(<TCIA Api Key>)
#'
#' @param apy.key the unique TCIA Api Key of the user
#' @return the updated list of body part names
#' @export get.BodyPartValues
#'
get.BodyPartValues <- function(api.key) {
  endpt = "https://services.cancerimagingarchive.net/services/v3/TCIA/query/getBodyPartValues"
  query = ""

  cat("Downloading the updated list of body part names...")
  url = build.url(endpt, query, api.key)
  data = fromJSON(url)
  return(data)
}


#' Return the updated list of available modality values (i.e. the types of images: CT, MR, ...)
#'
#' @title get.ModalityValues
#'
#' @examples
#'
#' get.ModalityValues(<TCIA Api Key>)
#'
#' @param apy.key the unique TCIA Api Key of the user
#' @return the updated list of modality values
#' @export get.ModalityValues
#'
get.ModalityValues <- function(api.key) {
  endpt = "https://services.cancerimagingarchive.net/services/v3/TCIA/query/getModalityValues"
  query = ""

  cat("Downloading the updated list of modality values...")
  url = build.url(endpt, query, api.key)
  data = fromJSON(url)
  return(data)
}


# Support function that create the correct path in
# which the files will be saved
create.paths <- function(dataset.path, dir_name){
  list_chars = strsplit(dataset.path,'')[[1]]
  last_char = list_chars[length(list_chars)]
  full_path = dataset.path
  if(nchar(dataset.path) > 0 &&
     last_char != "/" &&
     last_char != "\\"){
    full_path = paste(full_path, "/", sep="")
  }
  full_path = paste(full_path, dir_name, sep="")
  return(full_path)
}


# Support function that check the validiy of the groupby vector
check.groupgy <- function(groupby){
  attrs = c("SeriesInstanceUID",
            "StudyInstanceUID",
            "Modality",
            "ProtocolName",
            "SeriesDate",
            "SeriesDescription",
            "BodyPartExamined",
            "SeriesNumber",
            "Collection",
            "Visibility",
            "ImageCount",
            "AnnotationsFlag",
            "Manufacturer",
            "ManufacturerModelName",
            "SoftwareVersions")

  for(att in groupby){
    if(!is.element(att, attrs)){
      str_attr = paste(attrs, collapse = ", ")
      message = paste("The attribute \" ", att,
                      "\" is not correct, ",
                      "please select one of the ",
                      "following: ", str_attr, sep="")
      stop(message)
    }
  }
}

#' Return the list of the TCIA Series that match the required parameters
#'
#' @title tcia.get.Series
#'
#' @examples
#'
#' tcia.get.Series(<TCIA Api Key>, modality.values = "MR", body.parts = "HEAD")
#'
#' @param apy.key the unique TCIA Api Key of the user
#' @param body.parts a TCIA body part affected by the tumor (to get the available body part use: get.BodyPartValues)
#' @param modality.values a TCIA type of images (CT, MR, ...) that has to be downloaded (to get the update list use: get.ModalityValues)
#' @return a list of the TCIA Series
#' @export tcia.get.Series
#'
tcia.get.Series <- function(api.key, modality.values, body.parts){
  endpt = "https://services.cancerimagingarchive.net/services/v3/TCIA/query/getSeries"
  query = paste("Modality=", modality.values, sep="")

  cat("Downloading the series UIDs...", end="\n")
  url = build.url(endpt, query, api.key)
  data = fromJSON(url)
  if(body.parts != "" && length(data) > 0)
    data = data[data$BodyPartExamined %in% body.parts,]
  return(data)
}


# Support function that download the images
tcia.download.images <- function(api.key, row, dataset.path, groupby, file.name){
  endpt = "https://services.cancerimagingarchive.net/services/v3/TCIA/query/getImage"
  query = paste("SeriesInstanceUID=", row$SeriesInstanceUID, sep = "")
  url = build.url(endpt, query, api.key)

  path = create.paths(dataset.path, "")

  if(groupby != ""){
    for(j in groupby) {
      path = paste(path,
                   row[j], "/",
                   sep = "")
    }
  }

  if(!dir.exists(path)){
    dir.create(path)
  }

  path_name = paste(path, file.name, sep = "")
  cat(path_name, end="\n")

  response <- GET(url, write_disk(path_name, overwrite=TRUE))
  return(c(path, file.name))
}


#' Download the TCIA images related to a
#'
#' @title tcia.create.dataset
#'
#' @examples
#'
#' folder = c("Modality", "StudyInstanceUID")
#' tcia.create.dataset(<TCIA Api Key>, modality.values = "MR", body.parts = "HEAD", groupby=folder, unzip=FALSE)
#' # In this case all the magnetic resonance magnetic resonance of the heads, will be download but NOT unzipped.
#' # These zip files will be saved in the folder: <current directory>/MR/<StudyInstanceUID of the images>
#' #                                                                  ^^- Modality values of the images
#' #                                              ^^^^^^^^^^^^^^^^^^- given that dataset.path wasn't spacified
#'
#' @param apy.key the unique TCIA Api Key of the user
#' @param body.parts a TCIA body part affected by the tumor (to get the available body part use: get.BodyPartValues)
#' @param modality.values a TCIA type of images (CT, MR, ...) that has to be downloaded (to get the update list use: get.ModalityValues)
#' @param groupby A list of attribute that can be used to organize in images in nested folders (see example)
#' (Attribure that can be used: "SeriesInstanceUID","StudyInstanceUID", "Modality", "ProtocolName", "SeriesDate",
#' "SeriesDescription", "BodyPartExamined", "SeriesNumber", "Collection", "Visibility", "ImageCount", "AnnotationsFlag",
#' "Manufacturer", "ManufacturerModelName", "SoftwareVersions")
#' @export tcia.create.dataset
#'
tcia.create.dataset <- function(api.key, body.parts="", modality.values="", groupby="", dataset.path="", unzip=TRUE){
  if(groupby != "")
    check.groupgy(groupby)

  ## move to tcia.get.Series
  series = tcia.get.Series(api.key, modality.values, body.parts)

  if(nrow(series) > 0)
    for (i in 1:nrow(series)) {
      cat(
        "[",
        i,
        "/",
        nrow(series),
        "] Downloading the zip files: ",
        sep = "",
        end = ""
      )
      row = series[i, ]

      name = paste("images", i, ".zip", sep = "")
      path_name = tcia.download.images(api.key, row, dataset.path, groupby, name)
      path = path_name[1]
      path_name = paste(path_name[1], path_name[2], sep = "")
      if (unzip) {
        cat("\tUnzipping the file", end = "\n")
        unzip(path_name, exdir = path)
      }
    }else{
      cat("Unfortunately, there aren't images on TCIA that match your search criteria")
    }
}
