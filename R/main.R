library(httr)
library(readr)

#' Return the TCGA Gene Ensembl (ex. ENSG00000132694) give the TCGA Gene Symbol (ex. ARHGEF11)
#'
#' @title gene.symbol_to_ensembl
#'
#' @examples
#'
#' gene.symbol_to_ensembl("ARHGEF11")
#'
#' @param symbol the Gene Symbol of a TCGA Gene
#' @return the Gene Ensembl of the gene
#' @export gene.symbol_to_ensembl
#'
gene.symbol_to_ensembl <- function(symbol) {
  endpt = "https://portal.gdc.cancer.gov/auth/api/genes"
  fields = "gene_id"
  filters = paste('{"op":"and","content":',
                  '[{"op":"in","content":',
                  '{"field":"genes.symbol","value":["',
                  symbol,
                  '"]}}]}',
                  sep = "")

  tybody <- list(filters = filters, fields = fields, format = 'TSV')
  response <- GET(endpt, query = tybody)
  ensembl = content(response, type = "text/tab-separated-values", encoding = "UTF-8")$id
  return(ensembl)
}



#' Return the list of the TCGA case.ids which have the mutation of
#' TCGA Type <mutation_type> on the gene with Gene Ensembl equal
#' to <gene.ensembl> that couse the TCGA Consequence Type <mutation.cons_type>
#'
#' @title get.list_cases
#'
#' @examples
#' get.list_cases("ENSG00000152217", "Single base substitution", "missense_variant")
#'
#' @param gene.ensembl a Gene Symbol of a gene that occur in TCGA
#' @param mutation.type a TCGA mutation Type (related to the mutations)
#' @param mutation.cons_type a TCGA Consequence Type (related to the mutations)
#' @return the list of the case.ids which have the mutation of <mutation.type> on the gene <gene.ensembl>
#' @export get.list_cases
#'
get.list_cases<- function(gene.ensembl, mutation.type, mutation.cons_type="") {
  endpt = "https://portal.gdc.cancer.gov/auth/api/case_ssms"
  fields = "submitter_id,project.project_id,primary_site,demographic.gender,summary.file_count,summary.data_categories.data_category,summary.data_categories.file_count"
  filters = paste(
    '{"op":"and",',
    '"content":[',
    '{"op":"in",',
    '"content":{',
    '"field":"genes.gene_id","value":["', gene.ensembl,'"]}',
    '},',
    '{"op":"in",',
    '"content":{',
    '"field":"ssms.mutation_subtype","value":["', mutation.type,'"]}',
    '}', sep="")

  if(mutation.cons_type != "")
    filters = paste(filters, ',',
                    '{"op":"in",',
                    '"content":{',
                    '"field":"ssms.consequence.transcript.consequence_type","value":["', mutation.cons_type,'"]}',
                    '}', sep="")
  filters = paste(filters, ']', '}', sep="")

  cases = NULL
  tybody <- list(filters = filters, fields = fields, format = 'TSV', size="500")
  response <- GET(endpt, query = tybody)
  print(response)
  result = content(response, type = "text/tab-separated-values", encoding = "UTF-8")
  cases = data.frame(result)
  from_val = 500
  repeat{
    tybody <- list(filters = filters, fields = fields, format = 'TSV', size="500", from=as.character(from_val))
    response <- GET(endpt, query = tybody)
    result = content(response, type = "text/tab-separated-values", encoding = "UTF-8")
    if(is.null(result) || length(result) == 0){
      break
    }
    from_val = from_val + 500
    cases = rbind(cases, data.frame(result))
  }

  return(cases$id)
}



#' Return the list of the TCGA image ids (both of Diagnostic Slide and Tissue Slide)
#' related to a given case.id.
#'
#' @title get.case.images_ids
#'
#' @examples
#' # Where 942c0088-c9a0-428c-a879-e16f8c5bfdb8 is the case.id of
#' # the case with case.symbol TCGA-CJ-4642
#' get.case.images_ids("942c0088-c9a0-428c-a879-e16f8c5bfdb8")
#'
#' @param case.id an uuid of a TCGA case
#' @return the list of images that are related to the TCGA case specified
#' @export get.case.images_ids
#'
get.case.images_ids <- function(case.id) {
  endpt = "https://api.gdc.cancer.gov/files"
  filters = paste(
    '{"op":"and",',
    '"content":[',
    '{"op":"in",',
    '"content":{"field":"cases.case_id","value":["', case.id,'"]}},',
    '{"op":"in","content":{"field":"files.data_format","value":["SVS"]}}',
    ']',
    '}', sep = "")

  tybody <- list(filters = filters, format = 'TSV')
  response <- GET(endpt, query = tybody)
  result = content(response, type = "text/tab-separated-values", encoding = "UTF-8")$id
  return(result)
}


# Support function that generate the correct name
# of the image that has to be downloaded
get.image.name <- function(path, counter){
  path = unlist(strsplit(path, "[.]"))
  name = paste(path[1 : length(path)-1],
               collapse = "")
  ext = tail(path, 1)
  name = paste(name, "_", counter, ".", ext, sep = "")

  return(name)
}

# Support function that check if an Image (svs file)
# has already been download by seaching it in the specified
# path.
# @return the path of the file if exists, NULL otherwise
already.exists <- function(svsfile, path){
  if(grepl("/", path))
    dir = unlist(strsplit(path, "/"))
  path = paste(dir[1 : length(dir)-1], collapse = "/")
  if(grepl("\\\\", path))
    dir = unlist(strsplit(path, "\\"))
  path = paste(dir[1 : length(dir)-1], collapse = "/")
  if(path == ""){
    path = "."
  }
  files = list.files(path, pattern=".svs", recursive = TRUE)
  for(f in files){
    if(grepl(svsfile, f))
      return(f)
  }
  return(NULL)
}

#' Download and save the TCGA images specified using the UUID of the images.
#' Each image will be saved as <Image UUID>.svs
#'
#' @title download.images
#'
#' @examples
#' # Single image
#' # Given that 53507819-e29d-46fe-9fdc-fddc6d67a912 is the UUID of a TCGA image
#' download.images('53507819-e29d-46fe-9fdc-fddc6d67a912', '') # save the image in a folder in the workspace
#'
#' # List of images
#' images = c('e7b0b531-2fe3-4b31-bd79-dfeec3b316db', '959e61d6-f78e-4040-90a9-1cbd60f65db6')
#' download.images(images, '/path/to/a/folder/')
#'
#' @param images the list of TCGA UUID of the images that have to be downloaded
#' @param dataset.path the folder in which the images has to be saved (optional)
#' @export download.images
#'
download.images <- function(images, dataset.path="") {
  for(i in 1:length(images)){

    name = paste(dataset.path, images[i], ".svs", sep="")

    cat(paste("[", i, "/", length(images), "]  ",
              "Saving ", images[i], " as: ",
              name, sep=""), end="\n")

    check = already.exists(images[i], dataset.path)
    if(is.null(check)){
      url = paste("https://api.gdc.cancer.gov/data/",
                  images[i], sep="")
      response <- GET(url, write_disk(name, overwrite=TRUE))
    }else{
      write(paste("\t The image", images[i],
                  "has been already download, therefore it will be copied from: ",
                  check, "\n"),
            stderr()
      )
      file.copy(check, name)
    }
  }
}

# Support function that create the correct path to folder
# where the images (related to a specific mutation) will be saved
create.dataset_path <- function(dataset.path, dir_name){
  list_chars = strsplit(dataset.path,'')[[1]]
  last_char = list_chars[length(list_chars)]
  full_path = dataset.path
  if(nchar(dataset.path) > 0 &&
     last_char != "/" &&
     last_char != "\\"){
    full_path = paste(full_path, "/", sep="")
  }
  full_path = paste(full_path, dir_name, "/", sep="")
  return(full_path)
}

#' Download and save the TCGA images related to a specified mutation type and
#' mutation consequence type occured on a gene. These image will be saved in a folder
#' named "<gene_name>_<mutation type>_<mutation consequence type>, which will
#' be placed in <dataset.path>, if it is specified, or in the current
#' directory otherwise.
#'
#' @title download.mutation.images
#'
#' @examples
#' download.mutation.images("AKAP2", "Single base substitution", "intron_variant")
#'
#' @param gene.symbol the Gene Symbol of a TCGA Gene
#' @param mutation.type a TCGA mutation Type
#' @param mutation.cons_type a TCGA mutation Consequence Type (optional)
#' @param dataset.path the root folder of the dataset (optional)
#' @export download.mutation.images
#'
download.mutation.images <- function(gene.symbol, mutation.type, mutation.cons_type="", dataset.path=""){
  cat(paste("Retrieving the ID of the gene symbol: ", gene.symbol), end="\n")
  gene.ensembl = gene.symbol_to_ensembl(gene.symbol)

  cat("Downloading the cases in which the mutation: ", gene.symbol, mutation.type, mutation.cons_type, "had occurred", end="\n")
  cases = get.list_cases(gene.ensembl, mutation.type, mutation.cons_type)

  cat("Retrieving the images IDs for each case...", end="\n")
  all_images = c()
  for(i in 1:length(cases)){
    case_images = get.case.images_ids(cases[i])
    all_images = c(all_images, case_images)
  }

  cat("Creating the folder for the mutation: ", gene.symbol, mutation.type, mutation.cons_type, end="\n")
  dir_name = paste(gene.symbol,
                   gsub(",", "_", gsub(" ", "", mutation.type)),
                   gsub(",", "_", gsub(" ", "", mutation.cons_type)),
                   sep = "-")
  full_path = create.dataset_path(dataset.path, dir_name)
  if(!dir.exists(full_path)){
    dir.create(full_path)
  }

  cat("Downloading the images related to the mutation:", gene.symbol, mutation.type, mutation.cons_type, end="\n")
  download.images(all_images, full_path)
}

#' It create the dataset containing all the images available in TCGA related
#' to the specified mutations. The images will be organized in folders based
#' on the mutation type, the mutation consequence type and the gene on which
#' the  mutation had occured. These folders will be placed in the <dataset.path>,
#' if it specified, or in the current directory otherwise.
#'
#' @title create.mutation.dataset
#'
#' @examples
#' genes =      c("MUC16",            "PIK3CA")
#' types =      c("Small deletion",   "Small insertion")
#' cons_types = c("inframe_deletion", "inframe_insertion")
#' #              |________________|  |_________________|
#' #                       |
#' #                       |-> Gene + Type + Conseguence Type = First Mutation
#' # The three vectors (or any iterable data structure) have
#' # to be created in such a way that:
#' #   - the first mutation to be considered is: "Small deletion on MUC16 that cause an inframe_deletion"
#' #   - the second mutation to be cosidered is: "Small insertion in PIK3Ca that cause an inframe_insertion"
#' #   - ...
#' create.mutation.dataset(genes, types, cons_types)
#'
#' @param gene.list a list of Gene Symbols of TCGA Genes
#' @param mutation.types a list of TCGA mutation Types
#' @param mutation.cons_types a list of TCGA mutation Consequence Type (optional)
#' @param dataset.path the root folder of the dataset (optional)
#' @export create.mutation.dataset
#'
create.mutation.dataset <- function(gene.list, mutation.types, mutation.cons_types="", dataset.path=""){
  for(i in 1:length(gene.list)){
    gene.symbol = gene.list[i]
    mutation.type = mutation.types[i]
    mutation.cons_type = mutation.cons_types[i]
    download.mutation.images(gene.symbol, mutation.type, mutation.cons_type, dataset.path)
  }
}



