

ui <- fluidPage(theme="style.css",

  tags$script(src = "myscript.js"),
  
  shinyjs::useShinyjs(), ##Mario adding package:: makes everything more robust but a lot of work!
  shinyjs::extendShinyjs(text=extend_js_code),

  # Header ------------------------------------------------------------------

  shiny::fluidRow(

    column(3,
           tags$img(src="imb_logo.png",
                    align="left",
                    width="250")
    ),
    column(9,
           shiny::h1("IMB Proteomics"),
           shiny::h2("Sample Submission Form: Band Identification")
    )
  ),

  # User Informations -------------------------------------------------------

  fluidRow(
    ##TODO coockies as service
    ##TODO credentials
    #shiny::actionButton(inputId="testMark", label="TEST MARK ELEMENTS"),
    column(12,
     h3("Contact Information"),
     tags$div(class="userinfos",
              # HTML(paste0(sapply(names(user_info_fields), function(field){
              #         sprintf("%s",shiny::textInput(field, user_info_fields[[field]]))
              #       }),collapse="\n")),
              tagList(lapply(names(user_info_fields), function(field){
                shiny::textInput(field, user_info_fields[[field]])
              })),

              shiny::selectInput("group", "Group", selectize=FALSE,
                                 choices=c("", names(available_groups))),
              shiny::textAreaInput("affiliation", "Billing address (if external)", cols=3)

              ##Mario also commented below, these fields should be created with a list since you will use ALL these names again for validation.
              ##Mario maybe you can also add the validation to the list as well as width
              ##Mario might be a bit complicated if we now also add dropdown for group info but should be feasable like
              ##Timur keep it simple
              # list(
              #   list(type='textInput', variable='firstname', name='First name', validation='\\w'),
              #   list(type='dropdown', variable='group', name='Group',
              #        values=c(IMB_EW='Eva Wolf',
              #                 IMB_BU='Falk Butter',
              #                 EXT_CF='External'))
              # )
              ##Mario and we could loop over this list
     )
    )
  ),

  # Barcode section ---------------------------------------------------------

  h3("Digestion"),
  shiny::tags$p(paste0("Each sample will be treated with one or more proteases listed below. ",
                       "The standard treatment will be proceeded with trypsin. ",
                       "Therefore provide a barcode for each protease(or acid)")),
  fluidRow(
    column(4,
           shiny::textInput(inputId="barcode_acid", label="Acid Hydrolysis",
                            placeholder="barcode format IMB_XX_9999"),
           tags$p(style="color:red", ##Mario this text needs to be adapted since we need up to 3 times the material for all proteases one amount
                  "Please provide enough material (one band per sample/barcode)")
    ),
    column(4,
           shiny::textInput(inputId="barcode_trypsin", label="Trypsin",
                            placeholder="barcode format IMB_XX_9999")),
    column(4,
           shiny::selectizeInput(inputId="name_protease", label="Additional protease",
                                 choices=available_proteases),
           shiny::textInput(inputId="barcode_protease", label="Barcode",
                            placeholder="barcode format IMB_XX_9999"))
           ),


  # Databases section -------------------------------------------------------

  fluidRow(
    column(4,
           h3("Species"),

           tipify(shiny::selectInput(inputId="species_dbs", label="Databases (multiple)",
                              selectize=F, width="100%",
                              choices=species_dbs,
                              multiple=T, size=10), "Multiple choises are possible",
                  placement="right"),

           shiny::textInput(inputId="custom_species_db", label="Other species", width="5cm"),
           hidden( ##Mario does hidden make sense here? I would comment out the rows or does this has an advantage
             ##Timur by commenting out the code must be edited on other locations
             shiny::fileInput(inputId="custom_species_db_file",
                              label="or custom DB file",
                              buttonLabel="Upload File",
                              multiple=TRUE)
           )
    ),
    column(4,
           h3("Expression background"),
           shiny::selectInput(inputId="background_dbs",label="Database",
                              selectize=F, width="100%",
                              choices=background_dbs,
                              multiple=F, size=10),
           shiny::textInput(inputId="custom_background_db", label="Other species", width="5cm"),
           hidden(
             shiny::fileInput(inputId="custom_background_db_file",
                              label="or custom DB file",
                              buttonLabel="Upload File"))

    ),
    column(3,
           h3("Comments"),
           shiny::textAreaInput(inputId="dbs_comments", label="Additional comments",
                                height="200")
    )),


  # Specific infos for Band Identification ----------------------------------


  h3("Band Identification"),
  fluidRow(

    column(4,
           shiny::numericInput(inputId="band_weight_kda",
                               label="Moleculare weight(kDa) of band",
                               min=10, max=1000, value=0),
           shiny::selectizeInput(inputId="target_protein",
                                 label="Choose protein accesion number if known",
                                 choices=NULL, options=list(create=TRUE, maxItems=4),
                                 multiple=TRUE),
           shinyBS::popify(
             shiny::textAreaInput(inputId="fasta_sequence",
                                  label="Or/and protein sequence(s)",
                                  placeholder="fasta sequence with headers",
                                  width="100%", rows=6),
             title="<b>Usage:</b>", content=paste("Use the standard fasta format",
                                                  "<b>No whitespaces in sequence and we recommend no whitespace in names.</b>",
                                                  "Example:",
                                                  ">ProteinXY_SpeciesZ_FunctionW",
                                                  "QSGAGNNWAKGHYTEGAELVDQVLDV...",
                                                  ">ProteinABC_SP....",
                                                  "WAKGHYTEGAEL..."),
             placement="right", trigger="hover")
    ),
    column(4, ##Mario so many!!!! spaces... :-(
           shinyBS::tipify(shiny::textInput(inputId="protease", label="Additional protease",
                            placeholder="used in case of partial digestion"),
                           "Was the sample treated with any proteases?"),
           shiny::textInput(inputId="modification", label="Modification",
                            placeholder="name"),
           shiny::fileInput(inputId="gel_picture", label="Gel picture",
                            accept=c("image/jpeg","image/png", "application/pdf", "image/tiff"),
                            buttonLabel="Gel picture", multiple=F)
    ),
    column(3,
           h3("Comments"),
           shiny::textAreaInput(inputId="bandIdent_comments", label="Additional comments",
                                height="200")
    )
  ),

  h3("Staining"),
  shiny::radioButtons(
    inputId="staining",
    label="Staining",
    choices=c("Coomassie",
              "Silver stain (for optimal results we recommend Life Technology LC6070)")),


  hr(),

  verbatimTextOutput("message"),
  shiny::tags$div(id="buttons",
    shiny::actionButton(inputId="submit", label="Print", class="button"),
    shiny::uiOutput("link_placeholder", inline=TRUE),
    shinyjs::hidden(
      shiny::checkboxInput("changed_check", "Changed" , FALSE)
    )
    #shiny::tags$a(href="TEST.html", "Click here!"),
  ),

  hr()

)
