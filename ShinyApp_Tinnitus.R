library(dplyr)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization
library(dendextend) # for comparing two dendrograms
library(r2d3)
library(sqldf)
library(caret)
library(rpart.plot)
library(e1071)
library(randomForest)
library(mlbench)
library(rJava)
library(subspace)
library(orclus)
library(diceR)
library(clValid)

## Preprocessing of Dataset

df <- read_rds("190426_charite_tinnitus.rds") %>%
  arrange(.testdatum) %>%
  group_by(.jour_nr) %>%
  slice(1) %>%
  ungroup() %>%
  filter(.phase == "A") %>%
  mutate(
    phqk_paniksyndrom = if_else(
      phqk_phqk_2a +
        phqk_phqk_2b +
        phqk_phqk_2c +
        phqk_phqk_2d +
        phqk_phqk_2e == 5,
      1,
      0
    )
  ) %>%
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
originaldf <- read_rds("190426_charite_tinnitus.rds")
df_allF <- select(df, -c(.jour_nr))
#Data frame with all features "Scaled" except journ no
df_allF_scaled <- scale(df_allF) %>% data.frame()


correlations <- function(cor_threshold) {
  
  correlated_coloumns <- data.frame(F1 = character(),F2 = character())
  
  #cat("\ncorrelation with 90%:\n")
  matriz_cor <- cor(df_allF,method = "spearman")
  
  
  for (i in 1:nrow(matriz_cor)){
    correlations <-  which((abs(matriz_cor[i,]) > cor_threshold) & (matriz_cor[i,] != 1))
    matriz_cor[correlations,i] <- NA
    
    if(length(correlations)> 0){
      #lapply(correlations,FUN =  function(x) (cat("\t",paste(colnames(test)[i], "with",colnames(test)[x]), "\n")))
      correlated_coloumns <-  rbind(correlated_coloumns,data.frame(F1=colnames(df_allF)[i],F2=colnames(df_allF)[correlations]))
      rownames(correlated_coloumns) <- NULL
    }
  }
  
  
  x <- as.list(sqldf("SELECT distinct(F1) as feat FROM correlated_coloumns UNION SELECT distinct(F2) FROM correlated_coloumns") )
  count <- data.frame(matrix(ncol = length(x$feat), nrow = 0))
  count[1,] <- 0
  colnames(count) <- x$feat
  
  for (i in correlated_coloumns$F1) {
    count[1,which(colnames(count) == i)] <- count[1,which(colnames(count) == i)] + 1
  }
  for (i in correlated_coloumns$F2) {
    count[1,which(colnames(count) == i)] <- count[1,which(colnames(count) == i)] + 1
  }
  count <- as.data.frame(t(apply(count, 1, FUN=function(x) sort(x, decreasing=TRUE))))
  
  feature_remove <- c()
  k <- 1
  
  for ( i in 1:length(colnames(count))) {
    if(i < length(colnames(count))) {
      for (j in 1:length(correlated_coloumns$F1)) {
        if(colnames(count[i]) == correlated_coloumns$F1[j])
        {
          num <- which(colnames(count) == correlated_coloumns$F2[j])
          if((correlated_coloumns$F2[j] %in% x) == FALSE) {
            feature_remove[k] <- colnames(count[num])
            count <- select(count,-num)
            k <- k + 1 
          }
        }
      }
      for (j in 1:length(correlated_coloumns$F2)) {
        if(colnames(count[i]) == correlated_coloumns$F2[j])
        {
          num <- which(colnames(count) == correlated_coloumns$F1[j])
          if((correlated_coloumns$F1[j] %in% x) == FALSE) {
            feature_remove[k] <- colnames(count[num])
            count <- select(count,-num)
            k <- k + 1 
          }
        }
      }
    }
    else 
      break()
  }
  return(feature_remove)
}

feature_remove <- correlations(0.9)
#dropping the columns
df_noCorr <- select(df_allF,-feature_remove)
#Data frame with reduced features "Scaled"
df_noCorr_scaled <- scale(df_noCorr) %>% data.frame()
label <- c()

## Create DT

createDT <- function(final_labels, remove_na)
{
  set.seed(123)
  
  # labelling the records
  df_allF_labeled <- df_allF %>%
    mutate(label = final_labels)
  df_string_labeled <- df_allF_labeled
  
  #Adding "Type" to each cluster number to make it non numeric
  df_string_labeled$label <- sub("^", "Type ", df_allF_labeled$label)
  if (remove_na)
  {
    df_string_labeled <-
      df_string_labeled[-c(which(is.na(df_string_labeled$label))), ]
  }
  
  trctrl <- trainControl(method = "boot", number = 10)
  dtree_fit <-
    train(
      label ~ .,
      data = df_string_labeled,
      method = "rpart",
      parms = list(split = "information"),
      trControl = trctrl,
      tuneLength = 10
    )
  
  prp(
    dtree_fit$finalModel,
    extra = 2,
    box.palette = "Reds",
    tweak = 1.5,
    varlen
    = -10,
    branch.type = 5
  )
  #rpart.plot::rpart.plot(dtree_fit$finalModel, branch.type = 5, digits = 5, tweak = 1.9, fallen.leaves = FALSE)
}

## Visualization

feature_summary_scaled <- data.frame(df_allF_scaled %>%
                                       summarise_each(mean))
feature_summary <- data.frame(df_allF %>%
                                summarise_each(mean))

#Finally creating a data frame which can be used for the visualization.
options(scipen = 999)
createPlotData <<-
  function(algo_name,
           clus_name,
           cluster_scaled,
           cluster)
  {
    cluster_plot_data <-
      rbind(colnames(cluster[-1]),
            cluster_scaled[-1],
            feature_summary,
            cluster[-1])
    rownames(cluster_plot_data) <-
      c(
        "feature",
        "scaled_cluster_feature_value",
        "population_mean",
        "cluster_feature_value"
      )
    cluster_plot_data <-
      t(cluster_plot_data) %>% data.frame() %>% mutate(algorithm = algo_name, cluster_name =
                                                         clus_name)
    return(cluster_plot_data)
  }


getPlotData <- function(algo_name, final_labels, no_of_clusters)
{
  set.seed(123)
  # labelling the records
  df_allF_labeled <- df_allF %>%
    mutate(label = final_labels)
  df_allF_scaled_labeled <- df_allF_scaled %>%
    mutate(label = final_labels)
  
  # creating cluster feature vectors from obtained labels
  
  cluster_features_scaled <- data.frame(df_allF_scaled_labeled %>%
                                          group_by(label) %>%
                                          summarise_each(mean))
  cluster_features <- data.frame(df_allF_labeled %>%
                                   group_by(label) %>%
                                   summarise_each(mean))
  
  cluster_features <- cluster_features[1:no_of_clusters, ]
  
  plotData <- list()
  for (i in seq(1:no_of_clusters))
  {
    clus_name <- paste("Type ", i)
    plotData[[i]] <-
      createPlotData(algo_name,
                     clus_name,
                     cluster_features_scaled[i, ],
                     cluster_features[i, ])
  }
  return(plotData)
}
getProclusPlotData <- function(algo_name, proclus_cluster, cluster_num)
{
  # labelling the records
  df_noCorr_labeled <-
    df_noCorr[proclus_cluster[[cluster_num]]$objects, which(proclus_cluster[[cluster_num]][["subspace"]] == TRUE)] %>%
    mutate(label = cluster_num)
  df_noCorr_scaled_labeled <-
    df_noCorr_scaled[proclus_cluster[[cluster_num]]$objects, which(proclus_cluster[[cluster_num]][["subspace"]] == TRUE)] %>%
    mutate(label = cluster_num)
  
  # creating cluster feature vectors from obtained labels
  
  cluster_feature_scaled <- data.frame(df_noCorr_scaled_labeled %>%
                                         group_by(label) %>%
                                         summarise_each(mean))
  cluster_feature <- data.frame(df_noCorr_labeled %>%
                                  group_by(label) %>%
                                  summarise_each(mean))
  p_feature_summary <- data.frame(df_noCorr %>%
                                    summarise_each(mean))
  p_feature_summary <-
    p_feature_summary[, which(proclus_cluster[[cluster_num]][["subspace"]] == TRUE)]
  plotData <- list()
  clus_name <- paste("Type ", cluster_num)
  cluster_plot_data <-
    rbind(
      colnames(cluster_feature[-1]),
      cluster_feature_scaled[-1],
      p_feature_summary,
      cluster_feature[-1]
    )
  rownames(cluster_plot_data) <-
    c(
      "feature",
      "scaled_cluster_feature_value",
      "population_mean",
      "cluster_feature_value"
    )
  cluster_plot_data <-
    t(cluster_plot_data) %>% data.frame() %>% mutate(algorithm = algo_name, cluster_name =
                                                       clus_name)
  return(cluster_plot_data)
}

## Call from shiny
dt <- function(approach, labels) {
  if (approach == "Proclus") {
    createDT(labels, 1)
  }
  else {
    createDT(labels, 0)
  }
}

getnumberPcs <- function(variance, km.pc) {
  PCvar <- as.data.frame(t(summary(km.pc)$importance[3, ]))
  j <- 0
  for (i in seq(1:ncol(PCvar))) {
    if (PCvar[1, i] <= variance)
    {
      j <- j + 1
    }
  }
  return(j)
}

#getting labels of proclus
getLabelproc <- function(n, avgdim) {
  set.seed(123)
  ProClus.clusters.k <- ProClus(df_noCorr_scaled, k = n, d = avgdim)
  labels <- c()
  
  for (i in 1:n) {
    for (j in ProClus.clusters.k[[i]]$objects) {
      labels[j] <- i
    }
  }
  return(list("label" = labels, "objradial" = ProClus.clusters.k))
}
#getting labels of orclus
getLabelorc <- function(n, findim, initclust) {
  set.seed(123)
  library(orclus)
  
  orclus_res_k <-
    orclus(df_noCorr_scaled,
           k = n,
           l = findim,
           k0 = initclust)
  
  ok_labels <- orclus_res_k$cluster
  return(list("label" = ok_labels))
}

getLabelPCA <- function(n, var) {
  set.seed(123)
  km.pc <- prcomp(df_noCorr_scaled, center = TRUE)
  PC_num <- getnumberPcs(var, km.pc)
  df_PC <- km.pc$x[, 1:PC_num]
  pckmclust <- kmeans(df_PC, n)
  pckm_labels <- pckmclust$cluster
  return(list("label" = pckm_labels))
}

#getting labels of selected approach other than proclus and orclus
getLabel <- function(approach, n) {
  if (approach == "k-means") {
    set.seed(123)
    kmclust <- kmeans(df_noCorr_scaled, n)
    label <- kmclust$cluster
    
  }
  else if (approach == "Hk-means") {
    set.seed(123)
    hkmclust <- hkmeans(df_noCorr_scaled, n)
    label <- hkmclust$cluster
  }
  else if (approach == "Hierarchical") {
    set.seed(123)
    dist_matrix <- dist(df_noCorr_scaled, method = "euclidean")
    hc <- agnes(dist_matrix, method = "ward")
    label <- cutree(hc, k = n)
    
  }
  return(list("label" = label))
}

## SHINY APP

library(shiny)



ui <- fluidPage(pageWithSidebar(
  headerPanel('Tinnitus Analysis'),
  sidebarPanel(
    selectInput(
      "Algorithm",
      "Choose a clustering approach:",
      c(
        "none",
        "k-means",
        "Hk-means",
        "Hierarchical",
        "Orclus",
        "Proclus",
        "PCA k-means"
      )
    ),
    
    numericInput(
      "numClust",
      "Select number of clusters",
      min = 2,
      max = 10,
      value = 2
    ),
    conditionalPanel(
      condition = "input.Algorithm == 'Orclus'",
      numericInput("findim", "Final subspace dimensionality", 25, 2, 72),
      numericInput(
        "initclust",
        "Initial no. of clusters",
        20,
        max =  100
      )
    ),
    conditionalPanel(
      condition = "input.Algorithm == 'Proclus'",
      numericInput("avgdim", "Average dimensionality",value = 30, 3, 3, 72)
    ),
    conditionalPanel(
      condition = "input.Algorithm == 'PCA k-means'",
      numericInput(
        "var",
        "Variance",
        value = 0.4,
        min = 0.1,
        max = 1.0,
        step = 0.05
      )
    ),
    fluidRow(column(2, verbatimTextOutput("value"))),width = 3
  ),
  mainPanel(
    navbarPage(
      title = 'Visualization Options',
      tabPanel("Decision Tree", textOutput("out"), plotOutput('Dt')) ,
      tabPanel(
        "Radial Chart",
        textOutput("selected"),
        d3Output('Rc1', height = "500px"),
        conditionalPanel(condition = "input.numClust >= 2",
                         hr(), d3Output("Rc2", height = "500px")),
        conditionalPanel(condition = "input.numClust >= 3",
                         hr(), d3Output("Rc3", height = "500px")),
        conditionalPanel(condition = "input.numClust >= 4",
                         hr(), d3Output("Rc4", height = "500px")),
        conditionalPanel(condition = "input.numClust >= 5",
                         hr(), d3Output("Rc5", height = "500px")),
        conditionalPanel(condition = "input.numClust >= 6",
                         hr(), d3Output("Rc6", height = "500px")),
        conditionalPanel(condition = "input.numClust >= 7",
                         hr(), d3Output("Rc7", height = "500px")),
        conditionalPanel(condition = "input.numClust >= 8",
                         hr(), d3Output("Rc8", height = "500px")),
        conditionalPanel(condition = "input.numClust >= 9",
                         hr(), d3Output("Rc9", height = "500px")),
        conditionalPanel(condition = "input.numClust >= 10",
                         hr(), d3Output("Rc10", height = "500px"))
      )
    )
  )
))
server <- function(input, output) {
  condition <- reactive(input$Algorithm)
  viewsel <-
    reactive(
      if (condition() == "none")
        "You cannot see the results because you haven't selected any approach"
      else
        paste(
          "The decision tree for",
          input$Algorithm,
          "with number of cluster",
          input$numClust
        )
    )
  
  sel <-
    reactive(
      if (condition() == "none")
        "You cannot see the results because you haven't selected any approach"
      else
        paste(
          "The radial chart for",
          input$Algorithm,
          "with number of clusters",
          input$numClust
        )
    )
  
  label <-
    reactive(if (condition() != "none") {
      switch(
        input$Algorithm,
        "Proclus" = getLabelproc(input$numClust, input$avgdim),
        "Orclus" = getLabelorc(input$numClust, input$findim, input$initclust),
        "PCA k-means" = getLabelPCA(input$numClust, input$var),
        getLabel(input$Algorithm, input$numClust)
      )
    })
  DT <-
    reactive(if (condition() != "none")
      dt(input$Algorithm, label()$label))
  
  cluster_plot_data <-
    reactive(if (condition() != "none" &&
                 condition() != "Proclus")
      getPlotData(input$Algorithm, label()$label, input$numClust))
  
  output$Dt <- renderPlot({
    DT()
  })
  
  output$Rc1 <- renderD3({
    if (condition() == "Proclus")
      r2d3(
        data = getProclusPlotData("Proclus", label()$objradial, 1),
        script = "cluster_chart.js",
        viewer = "internal"
      )
    else if (condition() != "none") {
      r2d3(data = cluster_plot_data()[[1]],
           script = "cluster_chart.js",
           viewer = "internal")
    }
  })
  
  output$Rc2 <- renderD3({
    if (condition() == "Proclus")
      r2d3(
        data = getProclusPlotData("Proclus", label()$objradial, 2),
        script = "cluster_chart.js",
        viewer = "internal"
      )
    else if (condition() != "none") {
      r2d3(data = cluster_plot_data()[[2]],
           script = "cluster_chart.js",
           viewer = "internal")
    }
  })
  
  output$Rc3 <- renderD3({
    if (condition() == "Proclus")
      r2d3(
        data = getProclusPlotData("Proclus", label()$objradial, 3),
        script = "cluster_chart.js",
        viewer = "internal"
      )
    else if (condition() != "none") {
      r2d3(data = cluster_plot_data()[[3]],
           script = "cluster_chart.js",
           viewer = "internal")
    }
  })
  
  output$Rc4 <- renderD3({
    if (condition() == "Proclus")
      r2d3(
        data = getProclusPlotData("Proclus", label()$objradial, 4),
        script = "cluster_chart.js",
        viewer = "internal"
      )
    else if (condition() != "none") {
      r2d3(data = cluster_plot_data()[[4]],
           script = "cluster_chart.js",
           viewer = "internal")
    }
  })
  
  output$Rc5 <- renderD3({
    if (condition() == "Proclus")
      r2d3(
        data = getProclusPlotData("Proclus", label()$objradial, 5),
        script = "cluster_chart.js",
        viewer = "internal"
      )
    else if (condition() != "none") {
      r2d3(data = cluster_plot_data()[[5]],
           script = "cluster_chart.js",
           viewer = "internal")
    }
  })
  
  output$Rc6 <- renderD3({
    if (condition() == "Proclus")
      r2d3(
        data = getProclusPlotData("Proclus", label()$objradial, 6),
        script = "cluster_chart.js",
        viewer = "internal"
      )
    else if (condition() != "none") {
      r2d3(data = cluster_plot_data()[[6]],
           script = "cluster_chart.js",
           viewer = "internal")
    }
  })
  
  output$Rc7 <- renderD3({
    if (condition() == "Proclus")
      r2d3(
        data = getProclusPlotData("Proclus", label()$objradial, 7),
        script = "cluster_chart.js",
        viewer = "internal"
      )
    else if (condition() != "none") {
      r2d3(data = cluster_plot_data()[[7]],
           script = "cluster_chart.js",
           viewer = "internal")
    }
  })
  
  output$Rc8 <- renderD3({
    if (condition() == "Proclus")
      r2d3(
        data = getProclusPlotData("Proclus", label()$objradial, 8),
        script = "cluster_chart.js",
        viewer = "internal"
      )
    else if (condition() != "none") {
      r2d3(data = cluster_plot_data()[[8]],
           script = "cluster_chart.js",
           viewer = "internal")
    }
  })
  
  output$Rc9 <- renderD3({
    if (condition() == "Proclus")
      r2d3(
        data = getProclusPlotData("Proclus", label()$objradial, 9),
        script = "cluster_chart.js",
        viewer = "internal"
      )
    else if (condition() != "none") {
      r2d3(data = cluster_plot_data()[[9]],
           script = "cluster_chart.js",
           viewer = "internal")
    }
  })
  
  output$Rc10 <- renderD3({
    if (condition() == "Proclus")
      r2d3(
        data = getProclusPlotData("Proclus", label()$objradial, 10),
        script = "cluster_chart.js",
        viewer = "internal"
      )
    else if (condition() != "none") {
      r2d3(data = cluster_plot_data()[[10]],
           script = "cluster_chart.js",
           viewer = "internal")
    }
  })
  output$selected <- renderText({
    sel()
  })
  output$out <- renderText({
    viewsel()
  })
}
shinyApp(ui = ui, server = server)
