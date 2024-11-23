Revealing rarity patterns of miniature plants and the processes driving them across contrasting landscapes

Enrique Hernández-Rodríguez, Carlos Cerrejón, Xiangbo Yin, Marion Noualhaguet, Marc-Frédéric Indorf, Marion Barbé, Varina E. Crisfield, Juan C. Villarreal A., and Nicole J. Fenton 

The files used in this study are grouped into the following folders, organized according to the methods described in the manuscript.

I._ Sample coverage and species diversity:
This folder contains the databases with the number of occurrences for each bryophyte species across the four habitat types studied. There are six databases, one for each bryophyte lineage (3) for each bioclimatic domain (2).

BFWBBryLiv - Liverworts in Balsam fir-white birch = BFWB
BFWBBryMos - Mosses in Balsam fir-white birch = BFWB
BFWBBrySph - Sphagna in Balsam fir-white birch = BFWB

SPMOBryLiv - Liverworts in Spruce moss = SPMO  
SPMOBryMos - Mosses in Spruce moss = SPMO  
SPMOBrySph - Sphagna in Spruce moss = SPMO  

These databases are required for the species diversity analysis (q0, q1, q2) and to assess sampling completeness through sample coverage (SC) using Hill numbers.
The script I._ Sample coverage and species diversity contains the detailed code to perform the analyses with Hill numbers.

II._ Rarity analysis:
This folder contains two databases (BFWB_BryoAbundance and SPMO_BryoAbundance) which include information on:

Codsite: Sampling site number
Lat: Latitude
Long: Longitude
specie: Species acronym (full names are listed in Table S3 of the supplementary material)
NumIndiv: Number of occurrences of the species at the site indicated by the Codsite column
Habitat: Habitat type where the species was collected

for each bioclimatic domain.

The databases are structured so they can be directly used for the rarity analysis detailed in the scripts a_RarityAnalysis_Bryophyte_SPMO and b_RarityAnalysis_Bryophyte_BFWB.

III._ Community_Anlysis_NMDS

This folder contains the following databases:

AllSpeciesInTwoBioDomains: Contains raw abundance data for all species in the study (282 species) at the sampling site scale, including both bioclimatic domains.

SppRarityForm_SiteScale_TwoBioDomains: Contains the number of species, considering bryophyte lineages and their rarity patterns, as well as the total number of species per rarity pattern at the site scale.

Species_FunctionalTraits: A presence-absence matrix of functional traits related to life history (sexual condition and asexual reproduction) and the dispersal ability of each species (spore size). Additionally, it includes information on species presence in one or both bioclimatic domains (shared species) and, if the species is shared, changes or persistence in its rarity pattern.

These databases are necessary for NMDS and PERMANOVA analyses, which are detailed in the script NMD_RareSpecies&FunctionalTraits&PERMANOVA.

IV.- Figures 2 and 3 were created using Table S3 and the tutorial for alluvial plots available at https://cran.r-project.org/web/packages/ggalluvial/vignettes/ggalluvial.html. Editing details were made with Adobe Illustrator.
