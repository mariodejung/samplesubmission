

# parameterGroups <- list(
#   list(mode=0, enzyme=c('LysC','GluC')),
#   list(mode=0, enzyme=c('LysC','Trypsin')),
#   list(mode=4, enzyme=NULL)
# )

parameterGroups <- list(
  list(mode=0, enzyme=c('LysC','GluC'))
)


user_inputs_yaml_path <- "user_inputs.yaml"
mqpar_template_path <- "mqpar_template.xml"

user_inputs <- yaml::read_yaml(user_inputs_yaml_path)



enzymes <- user_inputs$enzymes

fasta_paths <- unique(c(user_inputs$species_dbs$paths,
                 user_inputs$background_dbs$paths,
                 user_inputs$custom_background_db_file,
                 user_inputs$custom_species_db_file,
                 user_inputs$fasta_sequence$path))

filled_template <- fillMqparTemplate(parameterGroups, fasta_paths)

write_file(filled_template,"Test_mqpar.xml")

#' fills a template mqpar string with the given infos 
#' @param paramterGroups list of parameterGroups settings (mode, enzymes)
#' @param fastaFiles vector of fasta files paths
#' @param rowFiles optionaly vector of row files paths
#' @param templateFile template File path
#' @return filled mqpar string
#' @example 
#' fillMqparTemplate( list(list(mode=0, enzyme=c('LysC','GluC'))), c("human.fasta", "yeast.fasta"))
#' 
fillMqparTemplate <- function (parameterGroups,
                               fastaFiles,
                               rowFiles=NULL,
                               templateFile="mqpar_template.xml"){
  template <- read_file(templateFile)
  parameterGroup_string <- 
    "<parameterGroup>
  <msInstrument>0</msInstrument>
  <maxCharge>7</maxCharge>
  <minPeakLen>2</minPeakLen>
  <useMs1Centroids>False</useMs1Centroids>
  <useMs2Centroids>False</useMs2Centroids>
  <cutPeaks>True</cutPeaks>
  <gapScans>1</gapScans>
  <minTime>NaN</minTime>
  <maxTime>NaN</maxTime>
  <matchType>MatchFromAndTo</matchType>
  <intensityDetermination>0</intensityDetermination>
  <centroidMatchTol>8</centroidMatchTol>
  <centroidMatchTolInPpm>True</centroidMatchTolInPpm>
  <centroidHalfWidth>35</centroidHalfWidth>
  <centroidHalfWidthInPpm>True</centroidHalfWidthInPpm>
  <valleyFactor>1.4</valleyFactor>
  <isotopeValleyFactor>1.2</isotopeValleyFactor>
  <advancedPeakSplitting>False</advancedPeakSplitting>
  <intensityThreshold>0</intensityThreshold>
  <labelMods>
  <string></string>
  </labelMods>
  <lcmsRunType>Standard</lcmsRunType>
  <reQuantify>False</reQuantify>
  <lfqMode>0</lfqMode>
  <lfqSkipNorm>False</lfqSkipNorm>
  <lfqMinEdgesPerNode>3</lfqMinEdgesPerNode>
  <lfqAvEdgesPerNode>6</lfqAvEdgesPerNode>
  <lfqMaxFeatures>100000</lfqMaxFeatures>
  <neucodeMaxPpm>0</neucodeMaxPpm>
  <neucodeResolution>0</neucodeResolution>
  <neucodeResolutionInMda>False</neucodeResolutionInMda>
  <neucodeInSilicoLowRes>False</neucodeInSilicoLowRes>
  <fastLfq>True</fastLfq>
  <lfqRestrictFeatures>False</lfqRestrictFeatures>
  <lfqMinRatioCount>2</lfqMinRatioCount>
  <maxLabeledAa>0</maxLabeledAa>
  <maxNmods>5</maxNmods>
  <maxMissedCleavages>0</maxMissedCleavages>
  <multiplicity>1</multiplicity>
  <enzymeMode>%s</enzymeMode>
  <complementaryReporterType>0</complementaryReporterType>
  <reporterNormalization>0</reporterNormalization>
  <neucodeIntensityMode>0</neucodeIntensityMode>
  <fixedModifications>
  <string>Carbamidomethyl (C)</string>
  </fixedModifications>
  <enzymes>%s
  </enzymes>
  <enzymesFirstSearch>
  </enzymesFirstSearch>
  <enzymeModeFirstSearch>0</enzymeModeFirstSearch>
  <useEnzymeFirstSearch>False</useEnzymeFirstSearch>
  <useVariableModificationsFirstSearch>False</useVariableModificationsFirstSearch>
  <variableModifications>
  <string>Oxidation (M)</string>
  <string>Acetyl (Protein N-term)</string>
  </variableModifications>
  <useMultiModification>False</useMultiModification>
  <multiModifications>
  </multiModifications>
  <isobaricLabels>
  </isobaricLabels>
  <neucodeLabels>
  </neucodeLabels>
  <variableModificationsFirstSearch>
  </variableModificationsFirstSearch>
  <hasAdditionalVariableModifications>False</hasAdditionalVariableModifications>
  <additionalVariableModifications>
  </additionalVariableModifications>
  <additionalVariableModificationProteins>
  </additionalVariableModificationProteins>
  <doMassFiltering>True</doMassFiltering>
  <firstSearchTol>20</firstSearchTol>
  <mainSearchTol>4.5</mainSearchTol>
  <searchTolInPpm>True</searchTolInPpm>
  <isotopeMatchTol>2</isotopeMatchTol>
  <isotopeMatchTolInPpm>True</isotopeMatchTolInPpm>
  <isotopeTimeCorrelation>0.6</isotopeTimeCorrelation>
  <theorIsotopeCorrelation>0.6</theorIsotopeCorrelation>
  <checkMassDeficit>True</checkMassDeficit>
  <recalibrationInPpm>True</recalibrationInPpm>
  <intensityDependentCalibration>False</intensityDependentCalibration>
  <minScoreForCalibration>70</minScoreForCalibration>
  <matchLibraryFile>False</matchLibraryFile>
  <libraryFile></libraryFile>
  <matchLibraryMassTolPpm>0</matchLibraryMassTolPpm>
  <matchLibraryTimeTolMin>0</matchLibraryTimeTolMin>
  <matchLabelTimeTolMin>0</matchLabelTimeTolMin>
  <reporterMassTolerance>NaN</reporterMassTolerance>
  <reporterPif>NaN</reporterPif>
  <filterPif>False</filterPif>
  <reporterFraction>NaN</reporterFraction>
  <reporterBasePeakRatio>NaN</reporterBasePeakRatio>
  <timsHalfWidth>0</timsHalfWidth>
  <timsStep>0</timsStep>
  <timsResolution>0</timsResolution>
  <timsMinMsmsIntensity>0</timsMinMsmsIntensity>
  <timsRemovePrecursor>True</timsRemovePrecursor>
  <timsIsobaricLabels>False</timsIsobaricLabels>
  <timsCollapseMsms>True</timsCollapseMsms>
  <crosslinkSearch>False</crosslinkSearch>
  <crossLinker></crossLinker>
  <minMatchXl>0</minMatchXl>
  <minPairedPepLenXl>6</minPairedPepLenXl>
  <crosslinkOnlyIntraProtein>False</crosslinkOnlyIntraProtein>
  <crosslinkMaxMonoUnsaturated>0</crosslinkMaxMonoUnsaturated>
  <crosslinkMaxMonoSaturated>0</crosslinkMaxMonoSaturated>
  <crosslinkMaxDiUnsaturated>0</crosslinkMaxDiUnsaturated>
  <crosslinkMaxDiSaturated>0</crosslinkMaxDiSaturated>
  <crosslinkUseSeparateFasta>False</crosslinkUseSeparateFasta>
  <crosslinkCleaveModifications>
  </crosslinkCleaveModifications>
  <crosslinkFastaFiles>
  </crosslinkFastaFiles>
  <crosslinkMode>PeptidesWithCleavedLinker</crosslinkMode>
  <peakRefinement>False</peakRefinement>
  <isobaricSumOverWindow>True</isobaricSumOverWindow>
  </parameterGroup>"
  
  
  par_groups <- paste(sapply(parameterGroups, function(settings) {
    sprintf(parameterGroup_string,
            settings['mode'],
            paste(sprintf('\n<string>%s</string>\n', 
                          switch(is.na(settings['enzyme']) + 1, 
                                 unlist(settings['enzyme']), 
                                 NULL)), 
                  collapse=''))
  }), sep='\n\n\n', collapse='\n\n')
  
  
  group_indices <- paste0("<int>", 0:(length(parameterGroups)-1), "</int>", collapse="\n")
  fasta_paths <- paste0("<string>", fastaFiles, "</string>", collapse="\n")
  
  template <- sub("##__Parameter_Groups__##", par_groups, template)
  template <- sub("##__Group_Indices__##", group_indices, template)
  #TODO \\ disappears
  fasta_paths <- gsub("\\\\", "\\\\\\\\", fasta_paths)# seems to work
  template <- sub("##__fastaFiles_##", fasta_paths, template)
  print(fasta_paths)
  
  if(!is.null(rowFiles)){
    row_paths <- fasta_paths <- paste0("<string>", rowFiles, "</string>", collapse="\n")
    fasta_paths <- gsub("\\\\", "\\\\\\\\", fasta_paths)
    template <- sub("##__rowFiles__##", row_paths, template)  
  }
  

  return(template)
}


