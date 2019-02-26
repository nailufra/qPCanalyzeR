############ qPCanalyzeR #####################
###### V. 0.0
###### https://github.com/nailufra/qPCanalyzeR
###### Only for non-commercial use

######################################################### BEGIN: USER INPUT AREA #################################################################

rm(list=ls())

# 0) SETTING PARAMETERS

# 0.1) MANDATORY PARAMETERS
# 0.1.1) name of your experiment (required for output directory) [string]
experimentName <- "my_test"
# 0.1.2) specify your working directory (directory where results are exported to) (e.g. "/home/user/.../") [string]
setwd("/home/julian/")
# 0.1.3) number of 384-well plates used in this experiment (required for plate input) [integer]
totalPlateCount <- 1
# 0.1.4) does your experiment has different timepoints (e.g. harvest1, harvest2 etc.)? -> then set TRUE, else FALSE [boolean]
isTimePointExperiment <- TRUE
# 0.1.5) does your experiment has several biological (NOT technical!) replicates? -> then set TRUE, else FALSE [boolean]
hasBiologicalReplicates <- FALSE
# 0.1.6) specify the gene treated as housekeeping gene, i.e. expression level of this gene is set to 100%.
# Expression of other genes is set in relation to it. 
# Make sure it is written exactly as in your GenesPlateView file (e.g. "ACTIN") [string]
housekeepingGene <- "ACTIN"   
# 0.1.7) specifiy samples that should be ignored in the gene expression analysis, e.g. your negative control.
# You can specify multiple samples by using c("Sample1","Sample2",...)
# Make sure it is written exactly as in your SamplesPlateView file (e.g. "H2O") [string]

#ignoreSamples <- c("H2O")

ignoreSamples <- c("H2O")


# 0.1.8) Comparator for ddCt
comparator <- "Col-0"

# 0.1.9) first timePoint for ddCt
firstTimePoint <- "E2"

# 0.2) OPTIONAL PARAMETERS
# 0.2.1) Name samples that should be ignored in analysis
# You can specify multiple samples by using c("Gene1","Gene2",...)
# Make sure it is written exactly as in your GenesPlateView file (e.g. "PP2A") [string]
ignoreGenes <- c()
# 0.2.2) Set plotting order of samples (default: alphabetical order)
plottingOrder <- c("35S:miR165",
                   "35S:ZPR3",
                   "Col-0",
                   "max2-1",
                   "max2-1/35S:miR165",
                   "max2-1/35S:ZPR3",
                   "max2-1/rev10d",
                   "max2-1/rev5",
                   "rev10d",
                   "rev5")

# 0.2.3) size of plots
pointSize <- 2.5

######################################################### END: USER INPUT AREA #################################################################


####################################################################
### PRESS STRG + A, then STRG + ENTER and check output directory ###
####################################################################


# 1) LOADING REQUIRED PACKAGES
#################################
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(plotrix)
#################################

# 2) PREPARING OUTPUT
#############################################################
# 2.1) creates directories where output will be exported to.
dir.create(file.path(paste(getwd(),"/",experimentName,"_","Results",sep="")))

#############################################################

# 3) LOADING DATA
# You will be asked for every 384-well plate
#############################################################
createInput <- function(nameOut,sepa){
  x <- as.list(totalPlateCount)
  for (i in 1:totalPlateCount){
    x[[i]] <- assign(paste(nameOut,i,sep=""),read.delim(file.choose(), header = FALSE, sep = sepa))
  }
  assign(as.list(match.call())[[2]],x,envir = .GlobalEnv)
}
# 3.1) provide BioRad output files (Plate View Results.csv)
createInput("plateInput",";")
# 3.2) provide your pipetting scheme: (c)DNA (Plate View)
createInput("sampleInput",",")
# 3.3) provide your pipetting scheme: GOIs (PlateView)
createInput("geneInput",",")

#############################################################


# 4) MODIFY PLATE INPUT
######################################
modifyPlateInput <- function(){
  for (z in 1:totalPlateCount){
    x <- as.data.frame(plateInput[[z]])
    # extracts every 4th line from input (consider specific output!)
    x <- x[seq(4, nrow(x), 4),]
    # removes unneccessary column
    x[,2] <- NULL
    # NAs instead of "n. def."
    x[x=="n. def."] <- NA
    # rename colnames and rownames
    x <- data.frame(x, row.names = x[,1])
    x[] <- lapply(x, gsub, pattern = ",", replacement= ".")
    #remove first column
    x[,1] <- NULL
    #alter colnames
    colnames(x) <- seq(1,24)
    plateInput[[z]] <<- x
  }
}
modifyPlateInput()
######################################


# 5) CREATING DATAFRAME 
######################################
createDataFrame <- function(){
plateInformation <<- data.frame(SampleID = seq(1,384*totalPlateCount),
                               FieldID = rep(NA,384*totalPlateCount),
                               Row = rep(NA,384*totalPlateCount),
                               Column = rep(NA,384*totalPlateCount),
                               Gene = rep(NA,384*totalPlateCount),
                               InputName = rep(NA,384*totalPlateCount),
                               Samples = rep(NA,384*totalPlateCount),
                               TimePoint = rep(0,384*totalPlateCount),
                               BiolRep = rep(1,384*totalPlateCount),
                               Plate = rep(0,384*totalPlateCount),
                               Ct = rep(NA,384*totalPlateCount),
                               usedInAnalysis = rep(FALSE,384*totalPlateCount))
}
createDataFrame()
######################################


# 6) FILL DATAFRAME
######################################
# 6.1) FieldID,Row,Column,Plate
modifyDataFrame <- function(){
  counter = 1
  for (z in 1:totalPlateCount){
    for (i in 1:nrow(plateInput[[z]])){
      for (j in 1:length(plateInput[[z]])){
        plateInformation$FieldID[counter] <<- paste(rownames(plateInput[[z]])[i],colnames(plateInput[[z]])[j],sep="")
        plateInformation$Row[counter] <<- paste0(rownames(plateInput[[z]])[i],sep="")
        plateInformation$Column[counter] <<- paste0(colnames(plateInput[[z]])[j],sep="")
        plateInformation$Plate[counter] <<- z
        counter <- counter + 1
      }
    }
  }
}
modifyDataFrame()

# 6.2) Fill DataFrame with values 
fillDataFrame <- function(YourCol,YourInput){
  for (z in 1:totalPlateCount){
    counter = 1
    for (i in 1:nrow(YourInput[[z]])){
      for (j in 1:length(YourInput[[z]])){
          plateInformation[counter, YourCol] <<- as.character(YourInput[[z]][i,j])
          counter <- counter + 1
        }
    }
  }
  plateInformation$Ct <- as.numeric(plateInformation$Ct)
  plateInformation <<- as.data.frame(plateInformation)
}

# 6.2.1) Ct Values
fillDataFrame("Ct",plateInput)
# 6.2.2) Samples
fillDataFrame("InputName",sampleInput)
# 6.2.3) Genes
fillDataFrame("Gene",geneInput)
######################################

###Export information about Ct values using geom_text (geom_label)

plateInformation$Column <- factor(plateInformation$Column, levels = c(seq(1,24)))

###with sample name###

ggplot(plateInformation, aes(x = max(as.integer(Column)/2), y = 20, colour = Gene)) + 
       geom_text(label = paste0(plateInformation$InputName,"\n",round(plateInformation$Ct,2)), size = 2.5, angle = 45) +
       facet_grid(Row~Column, switch = "y") +
       ylab("") +
       scale_colour_brewer(palette = "Dark2") +
       ggtitle(paste(experimentName,"\n","Plate Overview: Raw Ct values - Plate",plateInformation$Plate)) +
       
       theme(axis.text = element_text(size=7, color="black"),
             plot.title = element_text(hjust = 0.5, size= 15),
             axis.title.x = element_blank(),
             axis.text.x = element_blank(),
             axis.ticks.x = element_blank(),
             axis.ticks.y = element_blank(),
             axis.text.y = element_blank(),
             legend.position = "bottom",
             legend.title = element_text(size = 8),
             panel.border = element_rect(color = "black", fill = NA, size = 1))

ggsave(paste(experimentName,"_Results/",experimentName,"_CtPlateView.pdf",sep = ""), last_plot(), width = 297, height = 210, units = "mm")
ggsave(paste(experimentName,"_Results/",experimentName,"_CtPlateView.svg",sep = ""), last_plot(), width = 297, height = 210, units = "mm")
####


###without sample name###

ggplot(plateInformation, aes(x = max(as.integer(Column)/2), y = 20, colour = Gene)) + 
  geom_text(label = round(plateInformation$Ct,2), size = 3.75) +
  facet_wrap_paginate(~cut:Plate) + 
  facet_grid(Row~Column, switch = "y") +
  ylab("") +
  scale_colour_brewer(palette = "Dark2") +
  ggtitle(paste(experimentName,"\n","Plate Overview: Raw Ct values - Plate",plateInformation$Plate)) +
  
  theme(axis.text = element_text(size=7, color="black"),
        plot.title = element_text(hjust = 0.5, size= 15),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 8),
        panel.border = element_rect(color = "black", fill = NA, size = 1))

ggsave(paste(experimentName,"_Results/",experimentName,"_CtPlateViewNoSampleNames.pdf",sep = ""), last_plot(), width = 297, height = 210, units = "mm")
ggsave(paste(experimentName,"_Results/",experimentName,"_CtPlateViewNoSampleNames.svg",sep = ""), last_plot(), width = 297, height = 210, units = "mm")
### ###


#all fields excluded by user (ignoreSamples, ignoreGenes) are excluded
plateInformation <- plateInformation %>%
  mutate(usedInAnalysis = replace(usedInAnalysis, (!Gene %in% ignoreGenes) & (!InputName %in% ignoreSamples) & !is.na(Gene), TRUE)) %>%
  as.data.frame() #plateInformation
###


ggplot(plateInformation, aes(x = max(as.integer(Column)/2), y = 20, color = usedInAnalysis)) + 
  geom_point(size = 25) +
  geom_text(label = plateInformation$InputName, size = 1, angle = 45, color = "black") +
  facet_wrap_paginate(~cut:Plate) + 
  facet_grid(Row~Column, switch = "y") +
  ylab("") +
  scale_colour_brewer(palette = "Dark2") +
  ggtitle(paste(experimentName,"\n","Samples used for analysis - Plate:",plateInformation$Plate)) +
  
  theme(axis.text = element_text(size=7, color="black"),
        plot.title = element_text(hjust = 0.5, size= 15),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 8),
        panel.border = element_rect(color = "black", fill = NA, size = 1))

ggsave(paste(experimentName,"_Results/",experimentName,"_SamplesUsedForAnalysis.pdf",sep = ""), last_plot(), width = 297, height = 210, units = "mm")
ggsave(paste(experimentName,"_Results/",experimentName,"_SamplesUsedForAnalysis.svg",sep = ""), last_plot(), width = 297, height = 210, units = "mm")



# 7) CHECK FOR BIOLOGICAL REPLICATES AND DIFFERNET TIMEPOINTS
#############################################################
checkInput <- function(YourFrame,row){
  if(is.na(YourFrame$InputName[row]) | YourFrame$InputName[row] == ignoreSamples){
    YourFrame$TimePoint[i] = 0
  }
  
  else if(!grepl("@",YourFrame$InputName[row])){
    YourFrame$Samples[i] = YourFrame$InputName[i]
    YourFrame$TimePoint[i] = 1
  }
  
  else if(str_count(YourFrame$InputName[row],"@") > 1){
    YourFrame$Samples[row] <- strsplit(YourFrame$InputName[row],"@")[[1]][[1]]
    YourFrame$TimePoint[row] <- strsplit(YourFrame$InputName[row],"@")[[1]][[2]]
    YourFrame$BiolRep[row] <- substr(strsplit(YourFrame$InputName[row],"@")[[1]][[3]],4,10)
  }
  
  else if(str_count(YourFrame$InputName[row],"@") == 1 & grepl("@Rep",YourFrame$InputName[row])){
    YourFrame$Samples[row] <- strsplit(YourFrame$InputName[row],"@")[[1]][[1]]
    YourFrame$BiolRep[row] <- substr(strsplit(YourFrame$InputName[row],"@")[[1]][[2]],4,10)
  }
  
  else if(str_count(YourFrame$InputName[row],"@") == 1 & !grepl("@Rep",YourFrame$InputName[row])){
    YourFrame$Samples[row] <- strsplit(YourFrame$InputName[row],"@")[[1]][[1]]
    YourFrame$Samples[row] <- strsplit(YourFrame$InputName[row],"@")[[1]][[1]]
    YourFrame$TimePoint[row] <- strsplit(YourFrame$InputName[row],"@")[[1]][[2]]
  }
  plateInformation <<- YourFrame
}       

for (i in 1:nrow(plateInformation)){             
  checkInput(plateInformation,i)                 
}                                                                                      
#############################################################


###removes all rows labelled with NA in "Gene"
plateInformation <- plateInformation[!is.na(plateInformation$Gene),]


##TechRep
plateInformation <- plateInformation %>%
  group_by(Gene,Samples,TimePoint, BiolRep, Plate) %>%
  mutate(TechRep = row_number()) %>%
  as.data.frame()


  

#plateInformation <- rbind(plateInformation, allGenes)
# plateInformation <- plateInformation %>%
#   group_by(Gene) %>%
#   arrange(Samples,TimePoint) %>%
#   as.data.frame()


# 9) STATISTICS
#############################################################
calculateStatistics <- function(){
  
  ###calculate statistics
  plateInformation <<- plateInformation %>%
    group_by(Gene,Samples,TimePoint,BiolRep) %>%
    mutate(CtMean = mean(Ct, na.rm = TRUE), 
           CtStdDev = sd(Ct, na.rm = TRUE),
           CtStdErr = std.error(Ct, na.rm = TRUE)) %>%
    as.data.frame()
  
  #relExpression & dCT
  plateInformation <<- plateInformation %>%
    arrange(Gene) %>%
    group_by(Gene) %>%
    filter(usedInAnalysis == TRUE) %>%
    mutate(relativeExpression = (2^(filter(., Gene == housekeepingGene)$Ct) / 2^Ct) * 100,
           relativeExpressionMean = (2^(filter(., Gene == housekeepingGene)$CtMean) / 2^CtMean) * 100,
           dCt = Ct - filter(., Gene == housekeepingGene)$Ct,
           dCtMean = CtMean - filter(., Gene == housekeepingGene)$CtMean) %>%
    as.data.frame()
  
    ##2^-dCT
    plateInformation$log2dCt <<- 2^-(plateInformation$dCt)


    ##2^-ddCT

    if(isTimePointExperiment){
      plateInformation <<- plateInformation %>%
        arrange(Gene) %>%
        group_by(Gene,BiolRep,Samples) %>%
        mutate(ddCtByTimePoint = dCtMean - dCtMean[TimePoint == firstTimePoint]) %>%
        as.data.frame()
    }


    if(!isTimePointExperiment){
      plateInformation <<- plateInformation %>%
        group_by(Gene,BiolRep,Samples) %>%
        mutate(ddCtByTimePoint = dCtMean - dCtMean[Samples == comparator]) %>%
        as.data.frame()
    }

    ### ddCtMean
    plateInformation$log2ddCtByTimePoint <<- 2^-(plateInformation$ddCtByTimePoint)

}
calculateStatistics()


###PLOTS

#arrange
plottingFrame <- plateInformation %>%
  filter(Gene != housekeepingGene) %>%
  as.data.frame()

plottingFrame$sampleTimePoint <- paste(plottingFrame$Samples,plottingFrame$TimePoint,sep="\n")


##relative expression

ggplot(plottingFrame, aes(x = TimePoint, y = relativeExpressionMean, fill = Samples, color = Samples, group = Samples)) + 
  geom_line(size= 1) + 
  geom_point(size = 3) +
  facet_grid(Gene~., scales = "free") +
  ylab(paste("Relative Expression - (",housekeepingGene," 100%)",sep="")) +
  ggtitle(paste(experimentName)) + 
  
  theme(axis.text = element_text(size=7, color="black"),
        plot.title = element_text(hjust = 0.5, size= 15),
        legend.position = "right",
        legend.title = element_text(size = 8),
        panel.border = element_rect(color = "black", fill = NA, size = 1))

ggsave(paste(experimentName,"_Results/",experimentName,"_relativeExpression.pdf",sep = ""), last_plot(), width = 297, height = 210, units = "mm")
ggsave(paste(experimentName,"_Results/",experimentName,"_relativeExpression.svg",sep = ""), last_plot(), width = 297, height = 210, units = "mm")


##relative Expression - by Sample
ggplot(plottingFrame, aes(x = TimePoint, y = relativeExpressionMean , fill = Samples, color = Samples, group = Samples)) + 
  geom_line(size= 1) + 
  geom_point(size = 3) +
  facet_grid(Gene~Samples, scales = "free") +
  ylab(paste("Relative Expression - (",housekeepingGene," 100%)",sep="")) +
  ggtitle(paste(experimentName)) +
  
  theme(axis.text = element_text(size=7, color="black"),
        plot.title = element_text(hjust = 0.5, size= 15),
        legend.position = "right",
        legend.title = element_text(size = 8),
        panel.border = element_rect(color = "black", fill = NA, size = 1))

ggsave(paste(experimentName,"_Results/",experimentName,"_relativeExpression_bySample.pdf",sep = ""), last_plot(), width = 297, height = 210, units = "mm")
ggsave(paste(experimentName,"_Results/",experimentName,"_relativeExpression_bySample.svg",sep = ""), last_plot(), width = 297, height = 210, units = "mm")


###2^-dCt


ggplot(plottingFrame, aes(x = sampleTimePoint, y = log2dCt, fill = Samples)) + 
  geom_point(size = 5,colour="black",pch=21, size=5) +
  facet_grid(Gene~Samples, scales = "free") +
  ylab(bquote('2'^-ΔCt)) +
  ggtitle(paste(experimentName)) +
  
  theme(axis.text = element_text(size=7, color="black"),
        plot.title = element_text(hjust = 0.5, size= 15),
        legend.position = "none",
        legend.title = element_text(size = 8),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

ggsave(paste(experimentName,"_Results/",experimentName,"_dCt_TimePoints.pdf",sep = ""), last_plot(), width = 297, height = 210, units = "mm")
ggsave(paste(experimentName,"_Results/",experimentName,"_dCt_TimePoints.svg",sep = ""), last_plot(), width = 297, height = 210, units = "mm")
  

###2^-ddCT


ggplot(plottingFrame, aes(x = TimePoint, y = log2ddCtByTimePoint, fill = Samples, color = Samples, group = Samples)) + 
  geom_line(size= 1) + 
  geom_point(size = 3) +
  facet_grid(Gene~., scales = "free") +
  ylab(bquote('2'^-ΔΔCt)) +
  ggtitle(paste(experimentName)) +
  
  theme(axis.text = element_text(size=7, color="black"),
        plot.title = element_text(hjust = 0.5, size= 15),
        legend.position = "right",
        legend.title = element_text(size = 8),
        panel.border = element_rect(color = "black", fill = NA, size = 1))

ggsave(paste(experimentName,"_Results/",experimentName,"_ddCt_TimePoints.pdf",sep = ""), last_plot(), width = 297, height = 210, units = "mm")
ggsave(paste(experimentName,"_Results/",experimentName,"_ddCt_TimePoints.svg",sep = ""), last_plot(), width = 297, height = 210, units = "mm")



##ddCt - by Sample

ggplot(plottingFrame, aes(x = TimePoint, y = log2ddCtByTimePoint, fill = Samples, color = Samples, group = Samples)) + 
  geom_line(size= 1) + 
  geom_point(size = 3) +
  facet_grid(Gene~Samples, scales = "free") +
  ylab(bquote('2'^-ΔΔCt)) +
  ggtitle(paste(experimentName)) +
  
  theme(axis.text = element_text(size=7, color="black"),
        plot.title = element_text(hjust = 0.5, size= 15),
        legend.position = "right",
        legend.title = element_text(size = 8),
        panel.border = element_rect(color = "black", fill = NA, size = 1))

ggsave(paste(experimentName,"_Results/",experimentName,"_ddCt_TimePoints_bySample.pdf",sep = ""), last_plot(), width = 297, height = 210, units = "mm")
ggsave(paste(experimentName,"_Results/",experimentName,"_ddCt_TimePoints_bySample.svg",sep = ""), last_plot(), width = 297, height = 210, units = "mm")



