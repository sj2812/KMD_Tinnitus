##original data
dataset<-readRDS("C:\\Users\\shivani Jadhav\\Desktop\\DKE\\DKE SECOND SEM\\KMD Project\\190426_charite_tinnitus\\190426_charite_tinnitus.rds")


##Cleaned data
library("tidyverse")

df <- read_rds("C:\\Users\\shivani Jadhav\\Desktop\\DKE\\DKE SECOND SEM\\KMD Project\\190426_charite_tinnitus\\190426_charite_tinnitus.rds") %>%
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
  drop_na()
View(df)

library(factoextra)
library(fpc)
library(RcmdrMisc)

#correlation between all dimensions
df.cor<-cor(df_scaled)
View(df.cor)


set.seed(123)
# Elbow method to get best k from k = 2 to k = 12.
k.max <- 12
df_scaled<-scale(df)

wss <- sapply(1:k.max, 
              function(k){kmeans(df_scaled, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

#HKmeans implementation
res.hk <-hkmeans(df_scaled, 4)
names(res.hk)

#result summary
res.hk

#visualisation
hkmeans_tree(res.hk, cex = 0.2)
fviz_cluster(res.hk, frame.type = "norm", frame.level = 0.68)

#appening the created clusterlabels to data fo further work
df$cluster<-res.hk$cluster
View(df)

