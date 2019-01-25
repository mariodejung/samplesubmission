###############################################################################
#
# Script to do the band identification.
# 
# This script has to be executed within the folder with all the raw files. 
# This version creates reportings for all Proteins with 'Most peptides',
# 'Highest intensity' and Proteins of Interest from user_inputs.yaml
#
# developer: Mario Dejung <m.dejung@imb.de>
# version: 0.0.25
# date: 2018.10.24
#
# package version: 0.4.10
# package date: 2018-05-25
#
###############################################################################



library(ggplot2)
library(seqinr)
library(stringr)
# library(muscle)
library(Peptides)
library(gridExtra)
library(grid)
options(java.parameters = "-Xmx4096m")
library(xlsx)

library(rMQanalysis)

cat(sprintf('Working directory is set to: %s\n', getwd()))
copySourcedScript()


#user provided infos from web interface
user_inputs_file_name = "user_inputs.yaml"
if(file.exists(user_inputs_file_name)){
  user_inputs <- yaml::read_yaml(user_inputs_file_name)
}else{
  mylog(paste0('No configuration input file found: ', user_inputs_file_name, "WARN"))
}

data_directory <- file.path('combined','txt') # where are the txt files located
data_directory <- file.path('example_data','EW_0210') # where are the txt files located
out_directory <- file.path('.')
if(exists("user_inputs")){
  FASTA_DBs <- c(user_inputs$fasta_sequence$path)
}else{
  FASTA_DBs <- user_inputs$fasta_sequence$path
}
# provide full path to fasta file in case of problems like:
# c('C:\path\to\db.fasta','E:\some\other\path\otherdb.fasta')
background_system <- '' # we search this term in the Fasta.headers and 
# remove these entries. Leave empty '' to skip this step.

##!! should be interaktiv??? 
protein_number <- 1 # uses the first protein with highest peptide count, change
# to 2 for second 3 for third and so on
sort_by_peptide_count <- TRUE

first_of_group <- TRUE # Set to FALSE to get all sequences of the proteinGroup
specific_proteins <- c(ifelse(exists("user_inputs"), user_inputs$target_protein, NULL))
# search for specific protein ids and the related
# peptides. Set to empty vector 'c()' to switch it off, set to any protein ID
# like 'c("D3YTG2", "P15924")'
filter_contaminants <- TRUE # set to FALSE only if you look for bands marked as
# contaminants.s
my_peptide_length <- c() # fill in a start and end position to calculate weight.

seq_per_page <- 30 # leave it like this, might be a bit buggy...

##!! ?? Argumente ?? automatisches testen, nicht wirklich relevant
if(file.exists('default.R')) {source('default.R')}
if(!dir.exists(out_directory)) dir.create(out_directory)


###############################################################################
# reading the fasta db from the hard drive. Depending on the computer you use, 
# we might have to update this path.
path_list <- list(
  file.path('.'),
  file.path('E:','Fasta_UNIPROT'),
  file.path('','Volumes','Proteomics-Data','Fasta_UNIPROT'))

###############################################################################
# read in the files
proteinGroups <- read.delim(file.path(data_directory,'proteinGroups.txt'))
peptides <- read.delim(file.path(data_directory,'peptides.txt'))
summary <- read.delim(file.path(data_directory,'summary.txt'))
parameters <- read.delim(file.path(data_directory, 'parameters.txt'), stringsAsFactors=FALSE)

fasta_files <- unlist(strsplit(as.character(
  subset(parameters, Parameter == 'Fasta file', Value)), ';'))
##!! von MQ benutzte FASTA-Files-Pfade ausgelesen (DBs) 

##!! Falls kein zugriff auf eines(!) oder mehr Files 
##!! Eventuell eine func schreiben
if(!all(file.exists(fasta_files))){
  message('We could not use the fasta file names from the parameters file.')
  fasta_files_alternative_paths <- 
    c(FASTA_DBs, fasta_files, 
      file.path(path_list, 
                ##!! Warum werden die namen repliziert ?? kreuzprodukt
                rep(basename(gsub('\\\\','/',fasta_files)), each=length(path_list))))
  existing_fasta_files <- 
    fasta_files_alternative_paths[file.exists(fasta_files_alternative_paths)]
} else {
  existing_fasta_files <- fasta_files 
}

if(length(existing_fasta_files) == 0) {
  stop(sprintf('We could not find any of the following fasta files:\n%s\nMake sure to mount all network drives.',
               paste(fasta_files, collapse=', ')),
       call.=FALSE)
}

if(length(existing_fasta_files) < length(fasta_files)) {
  message('We could not find all fasta files:\nThe script might fail if the top protein could not be found.')
}

##!! Alle gefundenen DB-Files in eine DB-Struktur 
##!! kann man nicht mehr unterscheiden welche DB benutzt wurde -> liste bilden
fasta_db <- c()
for(fasta_file in existing_fasta_files) {
  fasta_db <- c(fasta_db, seqinr::read.fasta(fasta_file, seqtype='AA', as.string=T))
}

###############################################################################
# analysing the missed cleavage
enzyme <- as.character(summary$Enzyme[1])

##!! Warum die Unterscheidung ??
##!! Verbesserungspotential
if("Unspecific" %in% summary$Enzyme.mode) {
  acid_row <- grep("Unspecific", summary$Enzyme.mode)
  identifier <- sub('\\d{8}_[^_]+_[^_]+_([^_]+_[^_]{4})_.*',
                    '\\1',
                    summary$Raw.file[acid_row])
  title <- as.character(summary$Raw.file[[acid_row]])
} else {
  identifier <- sub('\\d{8}_[^_]+_[^_]+_([^_]+_[^_]{4})_.*',
                    '\\1',
                    summary$Raw.file[1])
  title <- as.character(summary$Raw.file[[1]])
}

##!! Kein Enzyme - keine missed Cleavega, da unspezifisch 
if(!is.na(enzyme)) {
  missed_cleavage <- getMissedCleavageDF(peptides)
  missed_cleavage_plot <- plotMissedCleavages(missed_cleavage, 
                                              list(enzymes=c(enzyme)),
                                              title=title)
  print(missed_cleavage_plot)
  ggsave(file.path(out_directory, 'missed_cleavage.pdf'),
         missed_cleavage_plot, width=8, height=8)
} else {
  warning('There was no enzyme detected, so NO MISSED CLEAVAGE plot!', 
          call.=FALSE)
}

##!! Warum missed Cleavage nicht im user_output-dir
##!! quality control (intern)
user_output <- sprintf('%s_band_identification', identifier)
if(!dir.exists(user_output)) dir.create(user_output)

###############################################################################
# filter all modifications
rMQanalysis::filterModifications(identifier, data_directory)
##!! Was genau macht die Funktion ?? In meinem Beispiel gibts keine *Sites.txt, also weniger wichtig

###############################################################################
# filter the data set

##!! Ungefähr das selbe wie in der Arbeitsprobe ??
##!! Die Funktion in getFilteredRowsFromDataset (in filterWhole.....) scheint nicht zu existieren
##!! nicht mit @export gekennzeichnet
pg_flt <- filterWholeDataset(proteinGroups, 
                             by_contaminant=filter_contaminants)
pg_ident <- filterIdentifiedProteins(pg_flt)
if(nrow(pg_ident) == 0) {
  stop(sprintf('There are no protein groups left after filtering.'), call.=FALSE)
}


detected_protein_ids <- unlist(str_split(pg_ident$Protein.IDs,';'))
if(length(detected_protein_ids) < 500) {
  detected_protein_ids_regex <- 
    ##!! gsub zum maskieren von | ?? teil des pr-namens
    paste(gsub('\\|', '\\\\|', detected_protein_ids), collapse='|')
  detected_fastas <-
    fasta_db[grep(detected_protein_ids_regex, names(fasta_db))]
  seqinr::write.fasta(detected_fastas, 
                      names(detected_fastas),
                      file.out=paste0(identifier,'_detected.fasta'))
  ##!! Besser eine var als ordnernamen. betriebssystem abhängig
  if(dir.exists('../Acid_Digestion')) {
    seqinr::write.fasta(detected_fastas, 
                        names(detected_fastas),
                        file.out=paste0('../Acid_Digestion/',identifier,'_detected.fasta'))
  }
} else {
  mylog('there are to many protein IDs, so we do not create the detected.fasta file.',
        dbg_level=0)
}

identified_headers <- 
  c('Protein.IDs','Fasta.headers',
    if('Gene.names' %in% names(pg_ident)) {c('Gene.names')},
    'Razor...unique.peptides',
    'Unique.peptides','Sequence.coverage....','Mol..weight..kDa.',
    'Sequence.length','Intensity','Peptide.counts..razor.unique.',
    'Peptide.counts..unique.')
##!! die erste Zahl nehmen ?? von was ??
pep_counts_all <- gsub('([^;]+).*',
                       '\\1',
                       as.character(pg_ident$Peptide.counts..all.))

##!! - für decreasing ?? überflüssige zeile ??
print_order_all <- order(-as.numeric(pep_counts_all))
if(sort_by_peptide_count) {
  print_order_all <- order(-as.numeric(pep_counts_all))
} else {
  print_order_all <- order(-pg_ident$Intensity)
}
write.table_imb(pg_ident[print_order_all,identified_headers], 
                file.path(out_directory, paste0(identifier,'_proteins_identified.txt')))

excel_wb <- xlsx::createWorkbook()
filtered_proteins_sheet <- 
  createSheetOfDF(excel_wb, 
                  pg_ident[print_order_all,identified_headers], 
                  'identified proteins')
xlsx::saveWorkbook(excel_wb, 
                   file.path(user_output, paste0(identifier,'_proteins_identified.xlsx')))

data <- pg_ident

##!! was genau ist das? hintergrundsignal? proteine, die vom wirtsorganismus stammen ?
if(background_system != '') {
  background_pG <- grep(background_system, pg_ident$Fasta.headers)
  if(length(background_pG) >= 1) {
    data <- pg_ident[-background_pG,]
  } 
} 

print_headers <- c('Protein.IDs','Peptide.counts..all.',
                   if('Gene.names' %in% names(data)) {c('Gene.names')},
                   'Mol..weight..kDa.', 'Intensity')
##!! ist nur das erste protein interessant
pep_counts <- gsub('([^;]+).*','\\1',as.character(data$Peptide.counts..all.))
##!! Proteine mit den meisten Peptiden. warum?? 

sorted_by_peptide_order <- order(-as.numeric(pep_counts))
sorted_by_intensity_order <- order(-data$Intensity)

protein_ids <- list()
protein_ids$`Most peptides` <-  unlist(strsplit(as.character(data$Protein.IDs[sorted_by_peptide_order[1]]), ';'))
protein_ids$`Highest intensity` <- unlist(strsplit(as.character(data$Protein.IDs[sorted_by_intensity_order[1]]), ';'))

if(sort_by_peptide_count) {
  print_order <- sorted_by_peptide_order
} else {
  print_order <- sorted_by_intensity_order
}



data <- data[print_order, ]
ifelse(nrow(data) <= 10, max_pG <- nrow(data), max_pG <- 10)

reduced_df <- data[1:max_pG, print_headers]
reduced_df$Intensity <- formatC(reduced_df$Intensity)
reduced_df$Protein.IDs <- sapply(reduced_df$Protein.IDs, function(x) {
  ifelse(nchar(as.character(x)) > 50, paste(substr(as.character(x),1,46), '...'), as.character(x))
})
reduced_df$Peptide.counts..all. <- sapply(reduced_df$Peptide.counts..all., function(x) {
  ifelse(nchar(as.character(x)) > 16, paste(substr(as.character(x),1,16), '...'), as.character(x))
})
if('Gene.names' %in% names(reduced_df)) {
  reduced_df$Gene.names <- sapply(reduced_df$Gene.names, function(x) {
    ifelse(nchar(as.character(x)) > 16, paste(substr(as.character(x),1,16), '...'), as.character(x))
  })
}

cat('\n\n\nOrdered proteinGroups:\n')
print(reduced_df)
cat('\n\n\n')


if(length(specific_proteins) >= 1) {
  protein_ids$`Protein(s) of Interest` <- specific_proteins 
} 
#else {
#   protein_ids <- strsplit(as.character(data$Protein.IDs[protein_number]), ';')[[1]]
#   if(first_of_group) {
#     protein_ids <- protein_ids[1]
#   }
# }


peptides_flt <- filterWholeDataset(peptides, 
                                   by_bysite=F, 
                                   by_contaminant=filter_contaminants)


for(prot_category in names(protein_ids)){
for(protein_id in protein_ids[[prot_category]]) {
  filesafe_protein_id <- gsub('\\|', '-', protein_id)
  protein_id_regex <- gsub('\\|', '\\\\|', protein_id)
  matching_pepdata_index <- 
    grep(paste0(c('^',';'), 
                protein_id_regex, 
                rep(c('$',';'), each=2), 
                collapse='|'), 
         peptides_flt$Proteins)
  
  if(length(matching_pepdata_index) > 0) {
    alignment_filename <- sprintf("%s_%s_alignment.txt", identifier, filesafe_protein_id)
    pepdata <- peptides_flt[matching_pepdata_index,]
    
    annotation <- getAnnot(fasta_db)
    matching_sequence_index <- grep(protein_id_regex, annotation, ignore.case=T)
    fadata <- fasta_db[matching_sequence_index]
    if(!length(fadata) >= 1) {
      stop(sprintf('Could not find %s in %s.\nPlease check the fasta database.',
                   protein_id,
                   paste(basename(existing_fasta_files), collapse=', ')))
    }
    sequence_index <- 1
    if(length(fadata) > 1) {
      print(unlist(annotation[matching_sequence_index]))
      warning(sprintf('There are several proteins matching "%s" in the database.',
                      protein_id), call.=FALSE, immediate.=TRUE)
      sequence_index <- as.integer(readline(prompt="Enter the sequence index number: "))
    }
    
    sink(file = file.path(user_output, alignment_filename), type = "output", split=TRUE)
    df <- data.frame(sequence=getSequence(fadata)[[sequence_index]])
    df$identified <- '-'
    #   str_list <- str_locate_all(c2s(getSequence(fadata)[[1]]), as.character(pepdata$Sequence))
    str_list <- as.data.frame(
      str_locate(c2s(getSequence(fadata)[[sequence_index]]), as.character(pepdata$Sequence)))
    for(rownr in seq(1:nrow(str_list))) {
      df[seq(str_list[rownr,1], str_list[rownr,2]),2] <- 
        as.character(df[seq(str_list[rownr,1], str_list[rownr,2]),1])
    }
    peptides_df <- str_list
    peptides_df$sequence <- as.character(pepdata$Sequence)
    peptides_df <- peptides_df[order(peptides_df$start),]
    
    names(df) <- c(protein_id, 'identified')
    
    seq_coverage <- data$Sequence.coverage....[grep(protein_id_regex, data$Protein.IDs)]
    
    
    cat(sprintf('%s with %d amino acids and %d aligned peptides and %.1f%% sequence coverage:\n\n',
                protein_id,
                nrow(df),
                nrow(peptides_df),
                seq_coverage))
    printAlignment(df, width=79)
    
    cat('List of peptides:\n')
    cat(sprintf('%6s%6s %s\n',
                'start',
                'end',
                'sequence'))
    for(i in seq(1:nrow(peptides_df))) {
      cat(sprintf('%6d%6d %s\n',
                  peptides_df[i,'start'],
                  peptides_df[i,'end'],
                  peptides_df[i,'sequence']))
    }
    
    biggest_peptides <- substr(c2s(getSequence(fadata)[[1]]),
                               min(peptides_df$start),
                               max(peptides_df$end))
    mass_biggest_peptide <- mw(biggest_peptides)
    mass_protein <- mw(c2s(getSequence(fadata)[[1]]))
    cat(sprintf('\nMolecular mass from position %d to %d is %5.2f kDa.\n\n',
                min(peptides_df$start),
                max(peptides_df$end),
                mass_biggest_peptide / 1000))
    cat('\n\n\n\n\n')
    sink()
    
    # Creating graph ----------------------------------------------------------
    
    # We merge our start and end to sequence since they match 100%!
    names(peptides_df) <- c('Start.position','End.position','Sequence')
    start_end_col <- grep('Start.position|End.position', names(pepdata))
    if(length(start_end_col) == 2) { 
      pepdata <- merge(pepdata[-grep('Start.position|End.position', names(pepdata))], 
                       peptides_df, 
                       by='Sequence')
    } else {
      pepdata <- merge(pepdata, peptides_df)
    }
    
    ordered_pepdata <- pepdata[rev(order(pepdata$Start.position)),]
    ordered_pepdata$labels <- sprintf('%s (%4d:%4d)', 
                                      ordered_pepdata$Sequence, 
                                      ordered_pepdata$Start.position, 
                                      ordered_pepdata$End.position)
    ordered_pepdata$labels <- factor(ordered_pepdata$labels, 
                                     levels=ordered_pepdata$labels)
    
    sequence_intensity_graph <- ggplot(ordered_pepdata, aes(labels, Intensity)) + 
      geom_bar(stat='identity') + coord_flip() + theme_imb() +
      ggtitle('Peptide intensity') + theme(axis.title.y=element_blank())
    
    df2 <- df
    df2$x <- seq(nrow(df2))
    df2$kDa <- sapply(df2$x, function(x) mw(substr(c2s(df2[[1]]),1,x)))
    df2$intensity <- 0
    df2$matches <- 0
    for(peptide in seq(nrow(ordered_pepdata))) {
      s_pos <- ordered_pepdata$Start.position[peptide]
      e_pos <- ordered_pepdata$End.position[peptide]
      df2[s_pos:e_pos,'intensity'] <- df2[s_pos:e_pos,'intensity'] + ordered_pepdata$Intensity[peptide]
      df2[s_pos:e_pos,'matches'] <- df2[s_pos:e_pos,'matches'] + 1
    }
    mw_intensity_graph <- ggplot(df2, aes(kDa/1000, intensity)) + 
      geom_bar(stat='identity', width=1, alpha=1) +
      theme_imb() + scale_x_continuous(
        breaks=seq(0,10000,ifelse(max(df2$kDa) > 200000, 20, 10)), 
        minor_breaks=seq(0,10000,ifelse(max(df2$kDa) > 200000, 5, 2))) +
      ylab('summarised intensity') + xlab('molecular weight (kDa)') +
      ggtitle('Intensity over molecular weight')
    
    unique_peptides <- length(unique(ordered_pepdata$labels))
    layout_height <- ceiling(unique_peptides / 30) + 1
    peptide_graphs <- arrangeGrob(mw_intensity_graph,
                                  sequence_intensity_graph, 
                                  # layout_matrix=rbind(c(1),c(2)), 
                                  heights=c(1 / layout_height, (layout_height - 1) / layout_height),
                                  top=paste(identifier, 'Protein ID:', protein_id))
    # ggsave(file.path(out_directory,
    #                  sprintf('%s_%s_intensity_graph2.pdf', identifier, filesafe_protein_id)),
    #        peptide_graphs,
    #        width=8.27,height=11.69)
    grid.newpage()
    grid.draw(peptide_graphs)
    pdf(file.path(user_output, sprintf('%s_%s_intensity_graph.pdf', identifier, filesafe_protein_id)),
        width=8.27, height=5.845)
    print(mw_intensity_graph)
    for(start in seq(nrow(ordered_pepdata), 1, -seq_per_page)) {
      my_diff <- start-seq_per_page-1
      end <- ifelse(my_diff <= 1, 1, my_diff)
      g <- 
        ggplot(ordered_pepdata[start:end,], aes(labels, Intensity)) + 
        geom_bar(stat='identity') + ylim(0,max(ordered_pepdata$Intensity)) + coord_flip() + theme_imb() +
        ggtitle('Peptide intensity') + theme(axis.title.y=element_blank())
      if(my_diff < 1) {
        space <- (seq_per_page-start)*12
        print(g + theme(plot.margin = unit(c(5.5,5.5,space,5.5), "pt")))
      } else {
        print(g)
      }
    }
    
    dev.off()
    
  } else {
    alignment_filename <- sprintf("%s_%s_NOT_FOUND_alignment.txt", identifier, filesafe_protein_id)
    sink(file = file.path(user_output, alignment_filename), type = "output", split=TRUE)
    cat(sprintf('We could not find %s within our protein groups file.\n', protein_id))
    sink()
  }
}
}

print(reduced_df)


if(length(my_peptide_length) == 2) {
  my_peptide <- substr(c2s(getSequence(fadata)[[sequence_index]]),
                       min(my_peptide_length),
                       max(my_peptide_length))
  mass_my_peptide <- mw(my_peptide)
  cat(sprintf('\nThe peptide you asked for (AA %d-%d) has a molecular weight of %5.2f kDa.\n',
              min(my_peptide_length), max(my_peptide_length), 
              mass_my_peptide / 1000))
}


species_check <- table(gsub('[^;]+ OS=([^GN=]+).*', '\\1', pg_ident$Fasta.headers))
if(length(species_check) > 5) {
  cat(paste(fasta_files, collapse='\n'), '\n\n')
  cat('\n\nEither this is an acid digestion or maybe something with the regex is not working.\nPlease check background manually!\n\n')
} else {
  cat(paste(fasta_files, collapse='\n'))
  cat('\nCheck for background organism:\n')
  print(species_check)
}

files2zip <- dir(user_output, full.names = TRUE)
zip(zipfile = paste0(user_output,'.zip'), files = user_output)


# jetzt mach ich irgendwas ------------------------------------------------





