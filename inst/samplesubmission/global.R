#
# shiny App to order a band identification
#
# developer: Timur Horn
# version: 0.0.3
# date: 2018.12.05
#
#

library(shiny)
#library(shinyhelper)
library(shinyBS)
library(shinyjs)
# package "V8" needed
library(yaml)
library(seqinr)
#library(cfpscripts)
library(rmarkdown)
library(pdftools)
library(tiff)
library(jpeg)
library(mailR)

library(samplesubmission)

config_file <- "config.yaml"

if(!file.exists(config_file)) file.copy("config_template.yaml", config_file)
config <- yaml::read_yaml(config_file)

#Output directory for all orders
orders_directory <- config$orders_directory

maximum_upload_megabytes <- config$maximum_upload_megabytes
options(shiny.maxRequestSize=maximum_upload_megabytes * 1024^2)

user_inputs_file_name <- config$user_inputs_file_name

user_info_fields <- list(surname="Surname",
                         firstname="Firstname",
                         phone="Phone",
                         billing="Billing",
                         email="Email")

available_groups <- config$available_groups

#fallback
species_dbs <-
  c(h.sapiens="H.sapiens",
    m.musclus="M.musclus",
    x.laevis="X.laevis",
    c.elegans="C.elegans")

if(file.exists(config$db_summary_file)){
  available_dbs <- read.csv2(config$db_summary_file, stringsAsFactors=FALSE)
  species_dbs <- c("--none--", available_dbs$species_names)
}else{
  createSummaryFileOfFastaFiles()
  available_dbs <- read.csv2(config$db_summary_file, stringsAsFactors=FALSE)
  species_dbs <- c("--none--", available_dbs$species_names)
}

names(species_dbs) <- c("--none--", paste0(available_dbs$species_names,
                             " (", available_dbs$prot_count,")"))

background_dbs <- species_dbs

available_proteases <- config$available_proteases

extend_js_code <- 'shinyjs.winopenAndPrint=function(url){
myWindow=window.open(url, "_blank");
myWindow.onload=function(){this.print()}
}
'
