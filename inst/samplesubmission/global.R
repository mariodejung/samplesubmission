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
library(cfpscripts)
library(rmarkdown)
library(pdftools)

library(samplesubmission)

##Mario General ToDo
##Mario * we should create an extra package for the GUI, fancy names could be fouRS for Shiny Sample Submission Sheet :) but it reminds me on the iPhone 4S joke saying iPhone for Ass :-)
##Mario * Swithcing to package, we split code into ui.R, server.R and global.R files. All functions go in individual files into R folder
##Mario * add documentation to functions, shouldn't be much but roughly telling what they do and what they return
##Mario * recommended but not mendatory, add the package to the related function, e.g. shinyjs::useShinyjs()

#Output directory for all orders
orders_directory <- file.path(".", "outputs")

maximum_upload_megabytes <- 40
options(shiny.maxRequestSize=maximum_upload_megabytes * 1024^2)

user_inputs_file_name <- "user_inputs.yaml"

user_info_fields <- list(surname="Surname",
                         firstname="Firstname",
                         #group="Group",
                         phone="Phone",
                         billing="Billing",
                         email="Email")

#fallback df
available_groups <- data.frame(V1=c("IMB_EW", "IMB_BU", "EXT_CF"),
                               V2=c("Eva Wolf", "Falk Butter", "External"))
if(file.exists("groups.txt")){
  available_groups <- read.delim("groups.txt", header=FALSE)
}

#fallback
species_dbs <-
  c(h.sapiens="H.sapiens",
    m.musclus="M.musclus",
    x.laevis="X.laevis",
    c.elegans="C.elegans")

if(file.exists("Available_species_DBs.csv")){
  available_dbs <- read.csv2("Available_species_DBs.csv", stringsAsFactors=FALSE)
  species_dbs <- available_dbs$species_names
  print(species_dbs)
}

available_proteases <- c("undefined","AspN", "GluC", "LysC", "Chymotrypsin")

extend_js_code <- 'shinyjs.winopenAndPrint=function(url){
myWindow=window.open(url, "_blank");
myWindow.print()
}'

background_dbs <- list(E.coli="E.coli", S.cerevisiae="S.cerevisiae") ##Mario background might be the same as species_dbs in the end
background_dbs <- species_dbs

#define UI

# Define server logic


# Run the application
#shinyApp(ui=ui, server=server)


