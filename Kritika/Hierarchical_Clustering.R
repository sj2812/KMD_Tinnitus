library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization
library(dendextend) # for comparing two dendrograms

#Data preprocessing
df <- read_rds("190426_charite_tinnitus.rds") %>%
  arrange(.testdatum) %>%
  group_by(.jour_nr) %>%
  slice(1) %>%
  ungroup() %>%
  filter(.phase == "A") %>%
  mutate(phqk_paniksyndrom = if_else(phqk_phqk_2a +
                                       phqk_phqk_2b +
                                       phqk_phqk_2c +
                                       phqk_phqk_2d +
                                       phqk_phqk_2e == 5, 1, 0)) %>%
  select(.jour_nr,
         .age,
         acsa_acsa,
         adsl_adsl_sum,
         bi_erschoepfung, bi_magen, bi_glieder, bi_herz, bi_beschwerden,
         bsf_geh, bsf_eng, bsf_aerg, bsf_an_de, bsf_mued, bsf_tnl,
         isr_deprsyn, isr_angstsyn, isr_zwasyn, isr_somasyn, isr_essstsyn,
         isr_zusatz, isr_isr_ges,
         phqk_depressivitaet, phqk_paniksyndrom,
         psq_anford, psq_anspan, psq_freude, psq_sorgen, psq_psq_sum,
         schmerzskal_beein10, schmerzskal_haeuf10, schmerzskal_staerke10,
         ses_ses_affektiv, ses_ses_sensorisch,
         sf8_bp_sf36ks, sf8_gh_sf36ag, sf8_mcs8, sf8_mh_sf36pw, sf8_pcs8,
         sf8_pf_sf36kf, sf8_re_sf36er, sf8_rp_sf36kr, sf8_sf_sf36sf, sf8_vt_sf36vit,
         sozk_soz01_male, sozk_soz02_german, sozk_soz05_partner, sozk_soz06_married,
         sozk_soz09_abitur, sozk_soz10_keinAbschl, sozk_soz11_job, sozk_soz18_selbstst, 
         sozk_soz1920_krank, sozk_soz21_tindauer, sozk_soz2224_psycho, sozk_soz25_numdoc,
         swop_sw, swop_opt, swop_pes,
         tq_aku, tq_co, tq_em, tq_inti, tq_pb, tq_sl, tq_som, tq_tf,
         tinskal_beein10, tinskal_haeuf10, tinskal_laut10,
         starts_with("tlq"), -tlq_timestamp
  ) %>%
  drop_na() %>%
  scale()  #for scaling/normalising the data

#Hierarchical Clustering

# Dissimilarity matrix
d <- dist(df, method = "euclidean")
# Hierarchical clustering using Complete Linkage
hc2 <- agnes(d, method = "complete")
pltree(hc2, cex = 0.6, hang = -1)
hc2$ac
# methods to assess
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")
m
dist_methods <- c("euclidean","manhattan","maximum", "canberra","binary","minkowski")

# function to compute coefficient
for(dist_m in dist_methods)
{
  dist_matrix<-dist(df, method = dist_m)
  for(x in m)
  {
    hc <-agnes(dist_matrix, method = x)
    pltree(hc, cex = 0.6, hang = -1)
    ac<-hc$ac
  }
}


hc3 <- agnes(df, method = "ward")
pltree(hc3, cex = 0.6, hang = -1, main = "Dendrogram of agnes")
