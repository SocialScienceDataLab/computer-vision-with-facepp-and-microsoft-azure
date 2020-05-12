## Author: Theresa Kuentzler, theresa.kuentzler@uni-konstanz.de
## Date: 12.05.2020
## Load the function azure and fill in the information below (lines 111 to 128)

## Load Packages
library(data.table)
library(jsonlite)
library(httr)
library(dplyr)

## Function gathers recognition estimates from Azure
## input: 
## 1. data: vector of fullpaths to images
## 2. your personal key
## output:
## 1. dataframe with variables, listed in faces <- data.table(....);
azure <- function(fullpath, key) {
  ## Initilize Object to store API output for single image
  face <- NULL
  
  ## create data.table with spots for all information
  faces <- data.table(faceid = as.character(NA),
                      
                      face_rectangle_top = as.numeric(NA), face_rectangle_left = as.numeric(NA),
                      face_rectangel_width = as.numeric(NA), face_rectangle_height = as.numeric(NA),
                      
                      emo_anger = as.numeric(NA), emo_contempt = as.numeric(NA), 
                      emo_disgust = as.numeric(NA), emo_fear = as.numeric(NA),
                      emo_hapiness = as.numeric(NA), emo_neutral = as.numeric(NA),
                      emo_sadness = as.numeric(NA), emo_surprise = as.numeric(NA),
                      
                      gender = as.character(NA),
                      
                      fullpath = fullpath,
                      
                      error_code = as.character(NA), error_message = as.character(NA))
  
  ## run counts the number of image at testing
  ## go over each fullpath and send to API
  ## write API-output in face
  run <- 0
  for (i in 1:length(fullpath)) {
    run <- run + 1
    cat(run, "\n")
    while(is.null(face)) {
      try(
        face <- as.character(httr::POST(url = "https://westeurope.api.cognitive.microsoft.com/face/v1.0/detect", # note: url changes depending on your location in the world
                                         config = add_headers(.headers = c("Ocp-Apim-Subscription-Key" = key)),
                                         query = list(returnFaceAttributes = "emotion,gender"),
                                         accept_json(),
                                         body = upload_file(fullpath[i], "application/octet-stream"),
                                         encode = "multipart")),
        silent = FALSE
      )
    }
    
    ## if error
    if (is.null(fromJSON(face)$error)) {
    ## if face is found, extract information and write into data.table
    facecount <- length(fromJSON(face)$faceId)
    
      if (facecount != 0) {
        faceid <- fromJSON(face)$faceId
        face_rectangle <- fromJSON(face)$faceRectangle
        
        emotion <- fromJSON(face)$faceAttributes$emotion
        gender <- fromJSON(face)$faceAttributes$gender
        
        ## write info to data.table
        faces[faces$fullpath == fullpath[i],][,1:14] <- c(faceid[1], face_rectangle[1,], emotion[1,], gender[1])
        
        if (facecount > 1) {
          faces <- union(x = faces, 
                         y = data.table(faceid = faceid, 
                                        face_rectangle_top = face_rectangle[,1], 
                                        face_rectangle_left = face_rectangle[,2],
                                        face_rectangel_width = face_rectangle[,3], 
                                        face_rectangle_height = face_rectangle[,4],
                                        
                                        emo_anger = emotion[,1], 
                                        emo_contempt = emotion[,2], 
                                        emo_disgust = emotion[,3], 
                                        emo_fear = emotion[,4],
                                        emo_hapiness = emotion[,5], 
                                        emo_neutral = emotion[,6],
                                        emo_sadness = emotion[,7], 
                                        emo_surprise = emotion[,8],
                                        
                                        gender = gender,
                                        
                                        fullpath = fullpath[i])) 
        }
        
        face <- NULL
        Sys.sleep(3)
      } else {
        face <- NULL
        Sys.sleep(3)
      }
    } #close if(error)
    else{
      faces[faces$fullpath == fullpath[i]][,16] <- fromJSON(face)$error$code 
      faces[faces$fullpath == fullpath[i]][,17] <- fromJSON(face)$error$message
      face <- NULL
      Sys.sleep(3)
    }
  }
  return(faces)
}

## Add your Azure key
mykey = "[your key]"

## Create your vector with filepaths
mypaths <- "[your vector with filepaths to images]"

## call the api
faces <- azure(fullpath = mypaths, key = mykey)
