
               # Rare species community analysis

# This script use the
# of Rabinowitz (1984) and performed by the Rare7 R package.

# Authosr:  Carlos Cerrejon Lozano and Enrique Hernandez Rodriguez
# Date:     2024/11/19 (Script cleaned this date)
# Project:  Revealing rarity patterns of miniature plants 
#           and the processes driving them across contrasting landscapes

# 1.- Activating R packages  ----------------------------------------------
library(FD)       # For CWM
library(vegan)
library(dplyr)
library(reshape2)
library(FD)
library(ggplot2)
library(RColorBrewer)
library(analogue)
library(ade4)
library(factoextra)
library(ggrepel)
library(gridExtra)
library(ggpubr)

# 2.- Setting directory ----------------------------------------------------
setwd("C:/Users/enriq/OneDrive - UQAT/Second Chapter document/Scritps/III._Community_Anlysis_NMDS")
list.files()

# 3.- Load databases -------------------------------------------------------
# Data base 1: Species abundance (at site scale) in the SPMO and BFWB bioclimatic domains
L<- read.csv("AllSpeciesInTwoBioDomains.csv", row.names=1, header = T) #row.names=1, header = T
View(L)

# Date base 2: sites x number of rarity form 
# This database contains the number of species within a rarity form
# at site scale by bryophyte lineages 
# The data was prepared using the results from the rarity analysis 
# using the package Rare 7.
rare<- read.csv("SppRarityForm_SiteScale_TwoBioDomains.csv",  row.names=1, header = T)
View(rare)

# 4.- Creating the data bases for the community analysis -------------------
## 4.1.- Habitat matrix  = sites x VegZone ---------------------------------
names(L)
# Variables:
#          L = Data base with P/A of all the species in both bioclimatic domains
#          VegZone = Column indicating the bioclimatic domain
#                    Boreal forest = SPMO
#                    Mixed forest = BFWB

R <-dplyr::select(L,VegZone)
R$VegZone<-as.factor(R$VegZone)

## 4.2.- Community matrix = sites x species ---------------------------------
# We need only the species columns
L<-dplyr::select(L,- VegZone)

# Outliers!
# "id_final" = 931 is an outlier (0 species found!)
# "id_final" = 932 is an outlier (1 species found!)

## Remove outlier from both databases
L <- L %>% slice(-which(rownames(.) == c("931", "932")))
R <- R %>% slice(-which(rownames(.) == c("931", "932")))

# 5.- NMDS ----------------------------------------------------------------
# 5.1.- Dissimilarity metric ---------------------------------------------
# Method = 5 = Sorensen (1948): s5 = 2a / (2a + b + c)
L_soren <- dist.binary(L, method = 5)

# 5.2.- Running NMDS ------------------------------------------------------------
# Random Number Generation
set.seed(12) 

# trymax = number of iterations
# k = number of dimensions
nmds_distance <-metaMDS(L_soren,k=2,trymax=999) # Visually, same result as with 999

# Saving the R file to avoid repeat the last long process
save(nmds_distance, file = "nmds_distance.RData") 

# With this function you can load the last object
load("nmds_distance.RData")                 

nmds_distance         # Stress:     0.210497 
nmds_distance$points  # Axes values
scores(nmds_distance) # Position of all the point in the two axes
plot(nmds_distance)
stressplot(nmds_distance)

# 6.- Visualizing NMDS --------------------------------------------------------
# NMDS plot with ordiplot() from vegan (with sorensen distance)
# It is NMDS even if pcoa is indicated in the name # type = text/points
ordiplot_pcoa <- ordiplot(nmds_distance, display="sites", type="points", cex = 0.2,
                          cex.axis = 1.4, cex.lab = 1.4)  # Adjust the font size as needed#Incluir esto no cambia el resultado; el plot se ajusta solo a el rango de las coordenadas: xlim = range(x), ylim = range(y)

# Visualizing bioclimatic domains
# Boreal forest is SPMO
# Mixed forest is BFWB
points(ordiplot_pcoa$sites[R$VegZone == 'Boreal forest',], pch=21, col="darkgreen", bg="darkgreen", cex = 1)
points(ordiplot_pcoa$sites[R$VegZone == 'Mixed forest',], pch=21, col="orange", bg="orange", cex = 1)

# 7.- Include number of rarity form species in NMDS -------------------------------

# We will use the second data base with the Rare community matrix.
# This means sites x number of rarity form by bryophyte lineage.

# Reminding the outliers deletions, do not delete row 931 in this data base 
# because  it does not appear already because there is not species
rare <- rare %>% slice(-which(rownames(.) == "932"))

rare <-dplyr::select(rare, LiverCommo,	LiverForm1,	LiverForm2,	LiverForm3,
                    MosseCommo,	MosseForm1,	MosseForm2,	MosseForm3,
                    SphagCommo,	SphagForm1,	SphagForm2,	SphagForm3,
                    TotCommoSpp,	TotForm1Spp,	TotForm2Spp,	TotForm3Spp)

# View variables, R2 and their significance (Table S4)
ord_envfit<-vegan::envfit(ordiplot_pcoa, rare, permutations = 999)#999 permutatios by default
ord_envfit   
r2_1 <- as.data.frame(ord_envfit$vectors$r)
r2_2 <- as.data.frame(ord_envfit$vectors$pvals)
r2_bind <- cbind(r2_1, r2_2)

write.csv(r2_bind, "Table S4.csv")

## 7.1.- Plotting significant rarity forms  -------------------------------
# We use p.max argument to select significant rarity forms
plot(ord_envfit, col="black", font=2, cex = 0.8, p.max =0.05)

## 7.2.- Plotting NMDS with ggplot2         -------------------------------
data.scores = as.data.frame(scores(nmds_distance))
data.scores$VegZone = R$VegZone

# Save species intrinsic values into dataframe
en_coord_cont2 <- as.data.frame(scores(ord_envfit, display = "vectors")) 

# Add pvalues to dataframe so you can select species which are significant
en_coord_cont2 <- cbind(en_coord_cont2, pval = ord_envfit$vectors$pvals)

# Subset data to show species significant at 0.05
en_coord_cont2 <- subset(en_coord_cont2, pval<=0.05) 

# See the vectors
head(en_coord_cont2)

NMDS1 = ggplot(data = data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(data = data.scores, aes(colour = VegZone), size = 2, alpha = 1) + 
  scale_colour_manual(values = c("darkgreen", "orange"))  + 
  coord_fixed()+
  theme_classic()+ 
  geom_segment(aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), 
               data = en_coord_cont2, size =1, alpha = 1, colour = "black") +
  geom_text(data = en_coord_cont2, aes(x = NMDS1, y = NMDS2), colour = "black", 
            fontface = "bold", label = row.names(en_coord_cont2), size = 7) + 
  theme(axis.title = element_text(size = 17, face = "bold", colour = "black"), 
        panel.background = element_blank(), panel.border = element_rect(fill = NA, colour = "black"), 
        axis.text = element_text(size = 15), legend.key = element_blank(), 
        legend.title = element_text(size = 15, face = "bold", colour = "black"), 
        legend.text = element_text(size = 15, colour = "black")) + 
  labs(colour = "Bioclimatic domains")+
  stat_ellipse(aes(color = VegZone), size = 1, alpha = 1) +
  scale_x_continuous(breaks=seq(-0.8,0.8,0.2),limits = c(-0.85,0.85))+
  scale_y_continuous(breaks=seq(-.50,.50,0.25),limits = c(-.60,.445))

# Remind!
# Boreal forest is the SPMO bioclimatic domain 
# Mixed forest is the BFWB bioclimatic domain 

# 8.- Community weighted mean (CWM)  --------------------------------------
## 8.1.- Species and functional traits matrix ----------------------------
Q <- read.csv("Species_FunctionalTraits.csv",  row.names=1, header = T)
colnames(Q)

    # Binary traits for the analysis need to be in numeric format, 
    # so integer is also ok

# Columns and values in the data base:
# [1] Species            = Species acronym
# [1] "Shared"           = 1 if the species is shared bewteen bioclimatic domains
# [1] "Different_rarity" = 1 if the species shared change its rarity form between them
# [1] "Dioicous"         = 1 if the species is dioicous
# [1] "Monoicous"        = 1 if the species is monoicous
# [1] "Both"             = 1 if the species presents both systems
# [6] "PresentAsex"      = 1 if the specie present asexual reproduction
# [1] "LargeSpore"       = 1 if the spore is large (>25 um)

## 8.2.- Preparing data bases to match them ------------------------------------
# Order species in the same order across the two matrices
L <- L[,order(colnames(L))] # Aplicar la función order y guargar un nuevo objeto                     

# Change in info in the Q data base 
rownames(Q) <- Q[,1]
Q <- Q[,-1]

## 8.3.- Apply dbFD function --------------------------------------------------
CWM.vegetation<-dbFD(Q, L, w = c(rep(1,1),  # No shared species 
                                 rep(1/1),  # Change in rarity form 
                                 rep(1/3,3),# Three sexual conditions
                                 rep(1/1),  # Presence of asexual reproduction 
                                 rep(1/1)), # Large spores 
                     w.abun = TRUE, stand.x = FALSE, calc.FRic = FALSE, 
                     calc.CWM = TRUE, CWM.type = c("all"),#or "dom" # if CWM.type = "all" also classes 0 of each binary vector are alre kept
                     messages = TRUE)

# Extract values from CWM
CWM.vegetation_values <- CWM.vegetation$CWM

# Select just positive binary traits (1) because the absence of each trait (0)
# is otherwise also represented in the plot
colnames(CWM.vegetation_values)
CWM.vegetation_values<-dplyr::select(CWM.vegetation_values, No_Shared_1,
                                                            Different_rarity_1, 
                                                            Dioicous_1,
                                                            Monoicous_1, 
                                                            Both_1, 
                                                            PresentAsex_1, 
                                                            LargeSpore_1)

set.seed(5)
ord_envfit_CWB <-vegan::envfit(ordiplot_pcoa, CWM.vegetation_values, permutations = 999)#999 permutatios by default

# See variables, R2 and its significance (Table S2)
ord_envfit_CWB 

# ***VECTORS
#                      NMDS1    NMDS2   r2      Pr(>r)     In Table S4
#  No_Shared_1        -0.66145 -0.74999 0.3484  0.001 *** Exclusive species
#  Different_rarity_1 -0.97657 -0.21521 0.4345  0.001 *** Species with rarity pattern change
#  Dioicous_1         -0.99410  0.10849 0.5661  0.001 *** Dioicous species
#  Monoicous_1         0.99591 -0.09040 0.6669  0.001 *** Monoicous species
#  Both_1             -0.99769 -0.06799 0.0649  0.001 *** Both sexual conditions
#  PresentAsex_1      -0.23871 -0.97109 0.3213  0.001 *** Asexual reproduction
#  LargeSpore_1       -0.98894  0.14831 0.2154  0.001 *** Large spores
#   ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Permutation: free
# Number of permutations: 999

## 8.4.- Plotting functional traits in NMDS  -----------------------------------
data.scores = as.data.frame(scores(nmds_distance))
data.scores$VegZone = R$VegZone

en_coord_cont3 <- as.data.frame(scores(ord_envfit_CWB, display = "vectors")) #save species intrinsic values into dataframe
en_coord_cont3 <- cbind(en_coord_cont3, pval = ord_envfit_CWB$vectors$pvals) #add pvalues to dataframe so you can select species which are significant
en_coord_cont3 <- subset(en_coord_cont3, pval<=0.05) #subset data to show species significant at 0.05
en_coord_cont3
str(en_coord_cont3)

# NMDS 2 with ggplot2

NMDS2 = ggplot(data = data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(data = data.scores, aes(colour = VegZone), size = 2, alpha = 1) + 
  scale_colour_manual(values = c("darkgreen", "orange"))  + 
  coord_fixed()+
  theme_classic()+ 
  geom_segment(aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), 
               data = en_coord_cont3, size =.7, alpha = 1, colour = "black") +
  geom_text(data = en_coord_cont3, aes(x = NMDS1, y = NMDS2), colour = "black", 
            fontface = "bold", label = row.names(en_coord_cont3), size = 7) + 
  theme(axis.title = element_text(size = 17, face = "bold", colour = "black"), 
        panel.background = element_blank(), panel.border = element_rect(fill = NA, colour = "black"), 
        axis.text = element_text(size = 15), legend.key = element_blank(), 
        legend.title = element_text(size = 15, face = "bold", colour = "black"), 
        legend.text = element_text(size = 15, colour = "black")) + 
  labs(colour = "Bioclimatic domains")+
  stat_ellipse(aes(color = VegZone), size = 1, alpha = 1) +
  scale_x_continuous(breaks=seq(-0.8,0.8,0.2),limits = c(-0.85,0.85))+
  scale_y_continuous(breaks=seq(-.50,.50,0.25),limits = c(-.60,.445))

# 9.- Combining both NMDs - Figure 1 --------------------------------------
NMDS1  # NMDS and rarity forms all bryophytes
NMDS2  # NMDS and functional traits

NMDSs_FinalPlot <- ggarrange(NMDS1, 
                             NMDS2, 
                            common.legend = TRUE, legend = "bottom",
                            ncol = 1, nrow = 2)

# Save the figure to edit it on edition program
# In our case we used Adobe Illustrator to improve the 
# visualization of the figure

pdf(file = "Figure 1 .pdf",   
    width = 16, # The width of the plot in inches
    height = 22) # The height of the plot in inches

NMDSs_FinalPlot

dev.off()   

# 10.- PERMANOVA ----------------------------------------------------------
## Analysis with adonis2
perm.L_soren <- adonis2(L_soren ~ R$VegZone, permutations = 999)

# See variables, R2 and their significance (Table S5)
perm.L_soren 

