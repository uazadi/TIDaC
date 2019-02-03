
# Support function that implemtent a
split.path <- function(path){
  path.splitted = ""
  if(grepl("/", path))
    path.splitted = unlist(strsplit(path, "/"))
  else
    path.splitted = unlist(strsplit(path, "\\"))
  return(path.splitted)
}

# Support function that create a dictionary, where the key
# is the file name and the value is the label (or class)
# of the file, i.e. the mutation occured.
# If a file is associate with more then one mutations
# the value will have the following structure:
# <mutation 1>-<mutation 2>-...-<mutation N>
create.dict_file_labels <- function(path, dict.class_mut){
  files = list.files(path, recursive=TRUE, full.names=TRUE)
  images = vector(mode="list")
  for (path in files) {
    path.splitted = split.path(path)
    name = path.splitted[length(path.splitted)]
    label = dict.class_mut[path.splitted[length(path.splitted) -1]]
    if(is.null(images[[name]]))
      images[name] = label
    else
      images[name] = paste(images[name],
                           "-", label, sep="")
  }
  return(images)
}

# Support function that create a dictionary, where the key
# is the Mutation(Gene-Type-Consequence) and the value is
# a short label, i.e. mut([0-9]+).
# This function also save a CSV file where this disctionary
# is printed.
create.dict_short_label <- function(path, path_for_dict){
  dict.class_mut = vector(mode="list")
  dirs = list.dirs(path)
  count = 1
  csv = "Mutation(Gene-Type-Consequence);Mutation_name\n"
  for(dir in dirs){
    if(dir != path){
      path.splitted = split.path(dir)
      mutation = path.splitted[length(path.splitted)]
      mutation_name = paste("mut", count, sep = "")
      dict.class_mut[mutation] = mutation_name
      csv = paste(csv, mutation, ";", mutation_name, end="\n")
      count = count + 1
    }
  }

  write_file(csv, paste(path_for_dict, "/dictionary.csv", sep = ""))
  return(dict.class_mut)
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


#' This dataset require in input the path (<path_from>) of a directory
#' where each sub-directory is named after mutation and contain the images
#' of the cases affected by that mutation. This function will copy
#' the images into a new folder (<path_to>), specifically into the sub-folders
#' that represent the label (class) of the images.
#'
#' The labels will have the following structure: mut([0-9]+).
#' A file called dictionary.csv will be saved in <path_to> and it will
#' contains all the pairs <mutation, label>
#'
#' Furthermore, if an image in labelled with several different mutations,
#' the name of the directory in which it will be saved will have the
#' following structure: mutX-nutY-...-mutZ (where X,Y,Z are numbers).
#'
#'
#'
#'
#' @title create.multilabeled.dataset
#'
#' @examples
#' create.multilabeled.dataset(path_from, path_to)
#'
#' @param dataset.path the path of the dataset form which te image has to be copied
#' @param new.dataset.path the path of the folder in which the multilabeled dataset will be saved
#' @export create.multilabeled.dataset
#'
create.multilabeled.dataset <- function(dataset.path, new.dataset.path){
  dict.class_mut = create.dict_short_label(dataset.path, new.dataset.path)
  images = create.dict_file_labels(dataset.path, dict.class_mut)
  for(i in 1:length(names(images))){

    class_name = images[names(images)[i]]
    full_path = create.paths(new.dataset.path, class_name)
    if(!dir.exists(full_path))
      dir.create(full_path)

    class_name = as.character(class_name)
    if(grepl("-", class_name)){
      class_name = unlist(strsplit(class_name, "-"))[1]
    }
    mutation = names(dict.class_mut)[which(class_name == dict.class_mut)]

    from_path = create.paths(dataset.path, paste(mutation, "/", names(images)[i], sep=""))
    to_path = create.paths(full_path, names(images)[i])
    file.copy(from_path, to_path)
  }
}

