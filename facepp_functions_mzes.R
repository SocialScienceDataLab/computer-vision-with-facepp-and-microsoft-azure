## Author: Theresa Kuentzler, theresa.kuentzler@uni-konstanz.de
## Date: 12.05.2020
## Load the functions authFacepp andfacepp and fill in the information below (lines 107 to 114)

## Load Packages
library(data.table)
library(jsonlite)
library(httr)
library(dplyr)

## Function creates object with Face++ key and secret, pass object to faceEst function
## Input:
## 1. api_key: character, given from faceplusplus account
## 2. api_secret: character, given from faceplusplus account
## Output:
## 1. authentifiaction object to be used in faceEST function
## Note: Function written by Sascha Goebel
authFacepp <- function(api_key, api_secret){
  auth <- structure(list(api_key = api_key, api_secret = api_secret), class="FaceppProxy")
}

## Function gathers recognition estimates from Face++
## input: 
## 1. data: vector of fullpaths to images
## 2. auth with api info (from function authFacepp)
## output:
## 1. dataframe with variables, listed in faces <- data.table(....);
## Note: Original function written by Sascha Goebel; adjsted by Theresa Kuentzler to:
## a) upload images from local machine instead of via link
## b) save multiple faces per image
## c) Adjustment to some changes in the API
facepp <- function(fullpath, auth) {
  ## Initilize Object to store API output for single image
  face <- NULL
  
  ## create empty table to fill with API output
  faces <- data.table(emo_anger = as.numeric(NA), emo_disgust = as.numeric(NA),
                      emo_fear = as.numeric(NA), emo_happiness = as.numeric(NA),
                      emo_neutral = as.numeric(NA), emo_sadness = as.numeric(NA), 
                      emo_surprise = as.numeric(NA),
                      
                      gender = as.character(NA),
        
                      facecount = as.numeric(NA), 
                      
                      fullpath = fullpath)
  
  ## run counts the number of image at testing
  ## go over each fullpath and send to API
  ## write API-output in face
  run <- 0
  for (i in 1:length(fullpath)) {
    run <- run + 1
    cat(run, "\n")
    while(is.null(face)) {
      try(
        face <- as.character(httr::RETRY("POST", "https://api-us.faceplusplus.com/facepp/v3/detect",
                                         body = list(api_key  = auth$api_key,
                                                     api_secret = auth$api_secret,
                                                     image_file = upload_file(fullpath[i]),
                                                     return_landmark = 0,
                                                     return_attributes = "emotion,gender"),
                                         times = 2, 
                                         encode = "multipart")),
        silent = FALSE
      )
    }
    
    ## if face is found, extract information and write into data.table
    facecount <- length(fromJSON(face)$faces$face_token)
    
    if (facecount != 0) {
      emotion <- fromJSON(face)$faces$attributes$emotion
      gender <- fromJSON(face)$faces$attributes$gender
      
      ## write info to data.table
      faces[faces$fullpath == fullpath[i],][,1:9] <- c(emotion[1,], gender[1,], facecount)

      ## if more than one face found in image, make df with all info and merge
      if(facecount > 1) {
        faces <- dplyr::union(x = faces, 
                       y = data.table(emo_anger = emotion[,1], 
                                      emo_disgust = emotion[,2],
                                      emo_fear = emotion[,3], 
                                      emo_happiness = emotion[,4],
                                      emo_neutral = emotion[,5], 
                                      emo_sadness = emotion[,6], 
                                      emo_surprise = emotion[,7],
                                      
                                      gender = gender[,1],
                                      
                                      facecount = facecount,
                                      
                                      fullpath = fullpath[i])) 
      }
      
      face <- NULL
      Sys.sleep(2)
    } else {
      face <- NULL
      Sys.sleep(2)
    }
  }
  return(faces)
}

## Fill in your details
myauth <- authFacepp(api_key = "[your key]", api_secret = "[your secret]")

## Create your vector with filepaths
mypaths <- "[your vector with filepaths to images]"

## Call the function
faces <- facepp(fullpath = mypaths, auth = myauth)
