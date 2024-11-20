
                   # Sample coverage and species diversity

          # Hills numbers using occurrences data of boreal bryophytes

# Author:  Enrique Hernandez Rodriguez
# Date:    2024/11/19 (Script cleaned this date)
# Project: Revealing rarity patterns of miniature plants 
#          and the processes driving them across contrasting landscapes

# 1.- Activating R packages  ----------------------------------------------
# iNEXT calculates the Hills numbers representing the 
# Richness (q0), Frequent (q1) and Dominant species (q2) of a assemblage.    
library(iNEXT)
citation("iNEXT")

# 2- Setting directory ----------------------------------------------------
setwd("Insert your directory addres here")

# 3.- Load databases -----------------------------------------------------
# Databases containing the number of occurrences of bryophyte species 
# in four habitat types. There is one database for each bryophyte lineage (3) 
# and for each bioclimatic domain (2).

# Spruce moss = SPMO bioclimatic domain
BorBryLiv2 <- read.csv("SPMOBryLiv.csv", row.names=1)
BorBryMos2 <- read.csv("SPMOBryMos.csv", row.names=1) 
BorBrySph2 <- read.csv("SPMOBrySph.csv", row.names=1)

# Balsam fir-white birch = BFWB bioclimatic domain
MixBryLiv2 <- read.csv("BFWBBryLiv.csv", row.names=1) 
MixBryMos2 <- read.csv("BFWBBryMos.csv", row.names=1)
MixBrySph2 <- read.csv("BFWBBrySph.csv", row.names=1)


# 4.- Analyzing species diversity by bioclimatic domain -------------------
# We use the function iNEXT to calculate the 
# Richness (q0), Frequent species (q1) and Dominant species (q2)
# as well as the Sample coverage (SC) by bryophyte lineage 

# Spruce moss = SPMO bioclimatic domain
nHillBorBryLiv <-iNEXT(BorBryLiv2, q = c(0,1,2), datatype = "abundance", 
                       size = NULL, se = TRUE, conf = 0.95, nboot = 50)

nHillBorBryMos <-iNEXT(BorBryMos2, q = c(0,1,2), datatype = "abundance", 
                       size = NULL, se = TRUE, conf = 0.95, nboot = 50)

nHillBorBrySph <-iNEXT(BorBrySph2, q = c(0,1,2), datatype = "abundance", 
                       size = NULL, se = TRUE, conf = 0.95, nboot = 50)

# Balsam fir-white birch = BFWB bioclimatic domain
nHillMixBryLiv <-iNEXT(MixBryLiv2, q = c(0,1,2), datatype = "abundance", 
                       size = NULL, se = TRUE, conf = 0.95, nboot = 50)

nHillMixBryMos <-iNEXT(MixBryMos2, q = c(0,1,2), datatype = "abundance", 
                       size = NULL, se = TRUE, conf = 0.95, nboot = 50)

nHillMixBrySph <-iNEXT(MixBrySph2, q = c(0,1,2), datatype = "abundance", 
                       size = NULL, se = TRUE, conf = 0.95, nboot = 50)


# 5.- Reading results ------------------------------------------------------
# Table S2 is based on the following results.
# Regarding the columns in the Table S2:

## Richness (q0), Frequent species (q1) and Dominant species (q2) ---------
# In table nHillBorBryLiv$AsyEst:
# Assemblage = habitat
# Diversity = 
#            Species richness = q0
#            Shannon diversity = q1
#            Simpson diversity = q2
# Observed = Species number observed and rounded in Table S2

# Spruce moss = SPMO bioclimatic domain
nHillBorBryLiv$AsyEst 
nHillBorBryMos$AsyEst
nHillBorBrySph$AsyEst

# Balsam fir-white birch = BFWB bioclimatic domain
nHillMixBryLiv$AsyEst 
nHillMixBryMos$AsyEst
nHillMixBrySph$AsyEst

## Sample coverage --------------------------------------------------------
# SC = Sample coverage rounded in Table S2.

# Spruce moss = SPMO bioclimatic domain
nHillBorBryLiv$DataInfo 
nHillBorBryMos$DataInfo
nHillBorBrySph$DataInfo

# Balsam fir-white birch = BFWB bioclimatic domain
nHillMixBryLiv$DataInfo 
nHillMixBryMos$DataInfo
nHillMixBrySph$DataInfo
