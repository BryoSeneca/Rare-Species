
                  # Rare species classification
         # Balsam fir-white birch (BFWB) bioclimatic domain  

    # Rare species classification based on the 7 forms of rarity 
    # of Rabinowitz (1984) and performed by the Rare7 R package.

# Author:  Enrique Hernandez Rodriguez
# Date:    2024/11/19 (Script cleaned this date)
# Project: Revealing rarity patterns of miniature plants 
#          and the processes driving them across contrasting landscapes

# 1.- Activating R packages  ----------------------------------------------
# Libraries to use:
#remotes::install_github("evertonmaciel/Rare7")# for Rare7 install
library(Rare7)
library(ggplot2)

# 2.- Setting directory ----------------------------------------------------
setwd("C:/Users/enriq/OneDrive - UQAT/Second Chapter document/Scritps/II._ Rarity analysis")
list.files()

# 3.- Load database -------------------------------------------------------
MixBryoRare4 <- read.csv("BFWB_BryoAbundance.csv")
str(MixBryoRare4)

# Delete first column ($ X) created when the data is loaded.
MixBryoRare4 <- MixBryoRare4[,-1] 

# 4.- Assessing species rarity with Rare 7 (Maciel and Arle, 2020) --------
# The next functions were provides by Everton Maciel (author of the package)
# because the package provided in R-cran did not work (this in summer 2021).

## 4.1.- Running and performing the rariData function -----------------------------------------
rariData <- function(table){
  `%ni%` <- Negate(`%in%`) #create an operator for "not in"
  Species <- as.character(sort(unique(table$specie))) # gets the list of species in the table
  Sample_area <- round(abs(diff(range(table$lat)))) #gets the range of the study in degrees
  Detection_area <- numeric()
  for(i in 1:length(Species))
  {
    Detection_area[i] <- length(unique(round(table$lat[which(table$specie==Species[i])]))) #gets the number of rounded lats where each species was collected, should we round them? 
  }
  Abundance <- numeric()
  for(i in 1:length(Species))
  {
    a <- table$NumIndiv[which(table$specie==Species[i])] # gets the abundance info in all sites where the species was observed
    b <- max(a)  
    if(b!=0){   # if there are sites with abundance > 0 
      Abundance[i] <- b
    }else{             # if not, NA   
      Abundance[i] <- NA
    }
  }
  Habitats <- numeric()
  for(i in 1:length(Species))
  {
    a <- unique(table$habitat[which(table$specie==Species[i])])  # gets all types of habitat
    b <- which(a==0)  # checks if any is 0
    if(length(b)==0){
      Habitats[i] <- length(a)   # problems with names not standardised (capital X not capital etc), we can try to create an automated way of fixing it (probably best to have it as an option)
    }else{
      Habitats[i] <- length(a[-b])
    }
  }
  Habitats[which(Habitats==0)] <- NA  # includes NA
  table2 <- data.frame(Species,Sample_area,Detection_area,Abundance,Habitats)
  return(table2)
} 

# Apply rariData function
# From Maciel and Arle (2020, https://doi.org/10.1016/j.ecolind.2020.106419): 

# The sort of data required by ‘rariData’ is a matrix data frame with numeric, 
# integer and factor vectors (BorBryoRare4). 
# Each entry (row) of the data frame represents necessary parameters for 
# the ‘rareData’ function to provide an output, and matrix data to 
# use in the second function. Missing data for abundance or habitats 
# should be indicated by 0 in the input data. 

tab <- rariData(MixBryoRare4)
View(tab)

# View the values in the tab data frame
unique(tab$Species)              # The list of species,
unique(tab$Sample_area)          # Total geographic area of a data set
                                 #        base on latitudinal degrees.
unique(tab$Detection_area)       # Number of latitudinal belts in which 
                                 #        each species are present.
                                 # Maximum number of individuals of each
                                 #        species listed at a site.
unique(tab$Habitats)             # Total number of habitats that a species 
                                 #        inhabits in the dataset.

## 4.2.- Performing the rareForms function -----------------------------------------
# Now tab can be used as an input for the ‘rarityForms’ calculation. 
# The "rareForms" function combines the values of the three parameters to 
# classify the seven forms of rarity. 
# The function returns a list of species and the rarity forms
RaForms <- rareForms(tab, percentage = 0.1, min_abund = 2, habitats = 1)

####          Create a graph with the rarity forms      ####
ResultsRare7 <- as.data.frame(RaForms)
View(ResultsRare7)
dim(ResultsRare7)

# Create a column with the bryophyte lineage
# m = mosses
# h = hepatiques
# s = sphagnums
BGuild <- substr(ResultsRare7$Species, 1, 1)   # Extract first letter 
                                               # from first column 

# Create a column with value of 1 to do a specie by rarity form sum later.
Value <- rep(1, times = 162) # times as many rows you have

# Add de columns
BryRarForms <- data.frame(ResultsRare7, BGuild, Value)
attach(BryRarForms)
unique(BryRarForms$Form)

# Modifying the acronyms to identify easily the bryophyte lineages
BryRarForms$BGuild[BryRarForms$BGuild == "h"] <- "Liverworts"
BryRarForms$BGuild[BryRarForms$BGuild == "m"] <- "Mosses"
BryRarForms$BGuild[BryRarForms$BGuild == "s"] <- "Sphagna"

# Modifying the acronyms of rarity form to better aesthetic in the bar plot
BryRarForms$Form[BryRarForms$Form == "common"] <- "Common"
BryRarForms$Form[BryRarForms$Form == "form1"]  <- "Form 1"
BryRarForms$Form[BryRarForms$Form == "form2"]  <- "Form 2"
BryRarForms$Form[BryRarForms$Form == "form3"]  <- "Form 3"

# Remove the row with information about the specie with no habitat information
# * This specie is fictitious and created because the analysis required it.
BryRarForms1 <- BryRarForms[!BryRarForms$Form == "No habitats information",]
unique(BryRarForms1$BGuild)

# Create groups by bryophyte lineage and rarity form 
BryRarForms2 <- aggregate(Value~Form+BGuild, BryRarForms1, sum) 
View(BryRarForms2)
sum(BryRarForms1$Value)

## 4.3.- Graph of rarity forms by bryophyte lineage ---------------------------

#### Number of species by rarity form and bryophyte lineage####

#png(filename = "3.-RarityForms_BFWB_4_forest.png", 
#    width = 2700, height = 2100,  res= 350)
pdf(file = "Fig.2b_BFWB b) BFWB bioclimatic domain.pdf",   # The directory you want to save the file in
    width = 7, # The width of the plot in inches
    height = 6) # The height of the plot in inches

    ggplot(data=BryRarForms2, 
       aes(x=Form, y=Value, fill=BGuild)) +
       geom_bar(stat="identity", position=position_dodge()) +
       theme_classic() + 
       theme(legend.position = "bottom") + 
       scale_colour_manual(values=c("olivedrab3", "chartreuse4", "mediumaquamarine")) +
       scale_fill_manual(values=c("olivedrab3", "chartreuse4", "mediumaquamarine")) +
       scale_y_continuous(limits = c(0,80), breaks=seq(0, 80, by = 20)) + 
       geom_text(aes(label=Value), size= 5, 
                 position=position_dodge(width=0.9), vjust=-0.25) + 
       labs(title="b) BFWB bioclimatic domain (161 species)",
            x = "Rarity form", y = "Number of species", fill = "Bryophyte guild")+ 
      theme(axis.text = element_text(size = 18),
            axis.title = element_text(size = 20),
            plot.title = element_text(size = 22),
            legend.text = element_text(size = 18),
            legend.title = element_text(size = 18))     
        
dev.off()

