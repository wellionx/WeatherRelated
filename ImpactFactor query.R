#ImpactFactor check ####
library(scholar)
library(dplyr)
jn = c("BMC Plant Biology", "Frontiers in Plant Science","Euphytica",
       "Molecular Plant", "Crop Science","Theoretical and Applied Genetics",
       "Molecular Breeding","Nature Genetics","New Phytologist",
       "Journal of Integrative Plant Biology","Genome Research",
       "The Plant Journal","Journal of Genetics and Genomics","Plant physiology",
       "PNAS","Genetics",
       "Journal of Experimental Botany","Planta","Plant Cell Reports",
      "BMC Genomics" )
IF_df <- get_impactfactor(jn)
#order by IF
arrange(IF_df, desc(ImpactFactor))

get_impactfactor("Proceedings of the National Academy of Sciences of the United States of America")

get_impactfactor("Field crop research")

get_impactfactor("Agricultural Water Management")

get_impactfactor("Journal of Natural Resources")

get_impactfactor("Global Change Biology")

get_impactfactor("Irrigation Science")

get_impactfactor("Journal of Agricultural Science")

