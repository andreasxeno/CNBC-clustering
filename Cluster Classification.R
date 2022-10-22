
# References: 
#http://uc-r.github.io/kmeans_clustering
#https://uc-r.github.io/hc_clustering

#====================================================================#
#Rev: January 2021

############################################################################# #
#Preliminaries =================  
remove(list = ls())
setwd("~/OneDrive - University of New Haven/Spring 2022/BANL 6420-Unsupervised Machine Learning/Week 2 1.31.2022")
options(digits = 3, scipen = 9999)

  library(pacman)
  p_load(tidyverse, #requires you install pacman ahead of this command
                   cluster, 
                   factoextra, 
                   NbClust, 
                   caret, 
                   randomForest,
                  cowplot,
                   useful,
                   ggfortify,
                 flexclust)
  
  library(FeatureImpCluster)
    
############################################################################## #
############################# Use the MTCARS data set ######################## #
# Data ================
data(mtcars)

  
        head(mtcars, 3); tail(mtcars, 3)
          str(mtcars)
        
        #Quick Checks
        table(is.na(mtcars))
        colSums(is.na(mtcars))  #equivalent
          #The point here is to appraise the extent of missing values to either
          #remove them or impute them.
    
        ############################# k-means Clustering###############
        ##
        ##
        # The Task
        # Can you match individual cars to their cyl designation via a cluster analysis?
        # How accurate is the match?
        table(mtcars$cyl)
        mtcars_df = mtcars %>% dplyr::select(-cyl) #take them arbitrarily
        head(mtcars_df)
        str(mtcars_df)
        
        # ================================= #
        # Assesing Clustering Tendence =========
        res <- get_clust_tendency(mtcars_df, n = nrow(mtcars_df)-1,
                                  graph = TRUE)
        names(res)
        res$hopkins_stat
        res$plot
        
        #0.805 indicates that there are a lot of clusters. closer to 1, the better. 0.5 not good
        
        #are there are non-random statistics within the data? then do the statistic below
        
        
              # Compute Hopkins statistic for a random dataset =====****important concept. Randomized any 
              #dataset, very important. 
              # First: generate a random data set from an existing data set
        
              random_df = apply(mtcars_df, 2,
                                
                                function (x){
                runif(length(x), min(x),max(x))
                
              })  
        
                get_clust_tendency(random_df,  n = nrow(random_df)-1,
                           graph = TRUE)
              
                #not clusterable cause the shopkins stat is 0.453. bad number. They will give you three clusters but
                #there are not meaningful.
                
              ################################################### #  
        
        
        #Determine how many clusters =============
        #Use NbClust
        NbClust(mtcars_df, method = "complete", index = "dindex") #maybe between 3-4 or 8-9, cause the slope gets smaller
            
        nb = NbClust(mtcars_df, method = "complete", index = 'hartigan')
          names(nb)
        
        NbClust(mtcars_df, method = "complete", index = 'hartigan')$Best.nc
        NbClust(mtcars_df, method = "complete", distance = "minkowski", index = 'hartigan')$Best.nc
        
        NbClust(mtcars, method = "ward.D", index = "hartigan")$Best.nc
        NbClust(mtcars_df, method = "centroid", index = "hartigan")$Best.nc
        
        NbClust(mtcars_df, method = "complete", index = "hartigan")$Best.partition
        
              ############################################### #
              #How many clusters to seek - via Scree Plot ==== (save this, important, you need this when you have factors
              
              x = mtcars_df
              
              Es <- numeric(10)
              
              for(i in 1:10){
                kpres <- kmeans(x, i)
                Es[i] <- kpres$tot.withinss
              }
              
              plot(1:10, Es, type = "b", ylab = "Objective Function", xlab = "# Clusters",main = "Scree Plot")
              plot(1:9, diff(Es,1), type = "b", ylab = "Objective Function", xlab = "# Clusters",main = "Scree Plot")
              
              ############################################# #
        
        #kmeans algorithm to identify the clusters ========
        k = stats::kmeans(mtcars_df, 3) #number 3 identifies the number of clusters
        
          names(k)
        mtcars_df$cluster = k$cluster
        head(mtcars_df)
        
        k$centers #use $ to find the clusters

                #============================================== #
                #Visualize ============ #
                #from the package cluster
                clusplot(mtcars_df, mtcars_df$cluster, color = TRUE, shade = TRUE, labels = 2, cex = .75)
                #from the package factoextra; a ggplot extension #excellent package"useful", shows you the clusters clearly
                fviz_cluster(list(data = mtcars_df[,1:10], 
                                  cluster = mtcars_df$cluster), 
                             repel = TRUE, ggtheme = theme_minimal()) +
                  theme(legend.position = "none")
          
                
                autoplot(k, data = mtcars_df, label = TRUE)
                
                autoplot(k, data = mtcars_df, label = TRUE, 
                         label.size = 5, frame = T, size = 3)
                
                #from the package "useful"
                plot.kmeans(k,data = mtcars_df, size = 5)
                plot.kmeans(k, data = mtcars_df, class = "cluster", size = 5) #looking for a correlation between color and shape
                #==============================================  #
              
     
      #Performance =========          
      # Appraise classification performance by comparing the clusters generated by the algorithm 
      # with the original classification
      tab_kc = table(mtcars_df$cluster, mtcars$cyl)
        tab_kc #sort of a confusion matrix, 
        
        tab_kc = tab_kc[c(2,1,3),] #dimensions 3x3 (row,column) #try again below 
        tab_kc = tab_kc[c(3,2,1),c(1,3,2)] #(row3,2,1, column 1,3,2 in this order) #7 six-cylinder cars. the labels 
        
      accuracy=  sum(diag(tab_kc))/nrow(mtcars)  #it looks funky, out of line #this is the formula for accuracy
      paste("accuracy =", accuracy, "percent")
      
      
                #Plot the results; recall that you can only map 2-dimensions
                ggplot(mtcars_df, aes(x =wt, y = mpg, col = as.factor(cluster)))+
                  geom_point(pch = 19, cex = 2, size = 9 )+
                  stat_ellipse() +
                  theme(legend.position = "none")

                
          # ======================================================= #
          # Tools ==============                
          #the function eclust from the package factoextra combines 
          #the 2 steps we outlined above.
          #it finds the clusters and graphs it simultaneously.
          #cool package eclust
                
          g1 = eclust(mtcars_df, "kmeans", k  = 3)
            names(g1)
          g2 = eclust(mtcars_df, "kmeans", k  = 4)

          #and you can use the package cowplot to visualize several
          #ggplots at the same time.  
          
          # here we run the algorithm with 3 and 4 clusters, graph them with 
          # the objective of deciding visually
          #cowplot helps you to see both at the same time and decide which one you want 
          cowplot::plot_grid(g1$clust_plot + theme(legend.position = "none"), 
                   g2$clust_plot + theme(legend.position = "none"), 
                   nrow = 2)

                    #======================================================= #
                    # Last calculate summary statistics across clusters
                    
                          mtcars_df$cyl = mtcars$cyl
                          mtcars_df$cluster = k$cluster
                    mtcars_df %>% group_by(cluster, cyl) %>% summarize(Avg_mpg = mean(mpg),
                                                                      Avg_wt = mean(wt))
                    
                    
                      b1 = ggplot(mtcars_df, aes(x = cluster, y = mpg, fill = as.factor(cluster))) + 
                      geom_boxplot()
                      b2 = ggplot(mtcars_df, aes(x = cluster, y = wt, fill = as.factor(cluster))) + 
                        geom_boxplot()
                    
                    cowplot::plot_grid(b1, b2, nrow = 2)
                    
      
      #Variable importance ######################################## #very important, whats driving this? 
      # maybe some data are reduntant, high correlation,
      # Which variables drive the cluster assignment?
      
      library(FeatureImpCluster)
      library(flexclust)
      
    
      set.seed(12345)
      result = kcca(mtcars_df[1:10],k=3)
            barplot(result)
            
            image(result)
            points(mtcars_df[1:10])
            
            
      FeatureImp_res <- FeatureImpCluster(result,as.data.table(mtcars_df[1:10]))
      plot(FeatureImp_res)
        names(FeatureImp_res)
      
      barplot(FeatureImp_res$featureImp)
    
      
      mtcars_df[1:10]$cluster = result@cluster
      
      mtcars_df
        
      #you find the three clusters and then regression for each one separately 
      
      # ========================================================+  
      
      # PENDING      
      # Three issues will remain - pending and for subsequent consideration
      #we did the raw data without standardizing the data.
      # (i) whether and how to "normalize" or scale" the data?
      # (ii) what to do when the data contains factor variables?
      #cylinder is a factor but we run it as a numeric data
    
     
      # Use the CNBC data on Top States for Business in 2021
      # Source: https://www.cnbc.com/2021/07/13/americas-top-states-for-business.html
      
      
        # Initial treatment: Remove the Overall ranking from the cluster analysis
        # "Move" the variable state to rownames using tibble::column_to_rownames()
      
      #Scrape the data off web site
      library(xml2)
      library(rvest)
      webpage_c = xml2::read_html("https://www.cnbc.com/2021/07/13/americas-top-states-for-business.html")
      
      cnbc_data = webpage_c %>%
        html_node("table") %>%
        html_table()
      
      names(cnbc_data) = c("OVERALL", "State", "Cost_of_Doing_Business",  
                           "Infrastructure", "Life_Health_Inclusion", "Workforce",
                           "Economy",  "Business_Friendliness", "Access_to_Capital",       
                           "Technology_Innovation",  "Education", "Cost_of_Living")
      
      write.csv(cnbc_data, "cnbc_data_2021.csv", row.names = FALSE )
      
      cnbc = read.csv("cnbc_data_2021.csv", header = T)
      head(cnbc)
      
      cnbc = cnbc %>% column_to_rownames("State")
      Overall = cnbc$OVERALL
      
      cnbc$OVERALL = NULL
      #  1. Use the Hopkins Stat(to see if it is "clusterable")
      
      NbClust(cnbc, method = "complete", index = 'hartigan')$Best.nc
      
      #We do have 4 clusters and it shows us that we hae a value index of 5.83
      
      # 1. find optimal number of clusters
           #Use a scree plot; use NBcluster()
      
      x = cnbc
      
      Esclust <- numeric(10)
      
      for(i in 1:10){
        kpres <- kmeans(x, i)
        Esclust[i] <- kpres$tot.withinss
      }
      
      plot(1:10, Esclust, type = "b", ylab = "Objective Function", xlab = "# Clusters",main = "Scree Plot")
      plot(1:9, diff(Esclust,1), type = "b", ylab = "Objective Function", xlab = "# Clusters",main = "Scree Plot")
      
      #The optimal number of clusters is 4. 
      
      # 2. classify the data into clusters
      result = kcca(cnbc,k=4)
      
      # 5. Show a plot of the clusters 
      
      barplot(result)
      image(result)
      points(cnbc)
      str(result) 
      
      # 6. Determine feature importance
      
      FeatureImp_res <- FeatureImpCluster(result,as.data.table(cnbc))
      plot(FeatureImp_res)
      names(FeatureImp_res)
      
      # 7. Upload the graph of feature importance for top states for Business 2021
      barplot(FeatureImp_res$featureImp, horiz = FALSE, las = 2)
      
      FI = as.data.frame(FeatureImp_res$featureImp)
      names(FI) = "FIMP"
      
      FI = FI %>% rownames_to_column("Feature")
      
      ggplot(FI, aes(x = reorder(Feature, desc(FIMP)), y = FIMP, 
                     fill = as.factor(FIMP))) + 
        geom_col() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
        theme(legend.position = "none") +
        coord_flip()
      
      
      # 8. Make a mental note: in which cluster is Connecticut? #What states are
      
      cnbc$cluster = result@cluster
      cnbc$Overall = Overall
      
      cnbc["Connecticut",]
      
      cnbc2 = cnbc %>% filter(cluster==2) 
      
      # 9. are associated with CT.  what are the definining attributes.
      
      dat_m = cnbc %>% rownames_to_column("State") 
      head(dat_m)
      
      ggplot(data = dat_m, aes(x = cluster, y = reorder(Overall, desc(Overall)), 
                               label = State, col = as.factor(cluster))) +        
        geom_text(vjust = 1) +
        geom_point(aes(size = cluster), alpha = 0.2) +
        geom_point(aes(size = Overall), alpha = 0.2) +
        geom_jitter() +
        labs(y = "Overall Ranking on CNBC Top States for Doing Business",
             x = "Groupings",
             title = "Top State for Doing Business, by cluster groups",
             caption = "AXeno") +
        theme(legend.position = "null")
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      #Scrape the data off web site
      #library(xml2)
      #library(rvest)
      #webpage_c = xml2::read_html("https://www.cnbc.com/2021/07/13/americas-top-states-for-business.html")
      
      #cnbc_data = webpage_c %>%
      #  html_node("table") %>%
      #  html_table()
      
      #names(cnbc_data) = c("OVERALL", "State", "Cost_of_Doing_Business",  
      #                     "Infrastructure", "Life_Health_Inclusion", "Workforce",
      #                     "Economy",  "Business_Friendliness", "Access_to_Capital",       
      #                     "Technology_Innovation",  "Education", "Cost_of_Living"   )
      #write.csv(cnbc_data, "cnbc_data_2021.csv", row.names = FALSE )
      
      
      #============================================================= #
      
      ############################ QED ############################################# #
      ############################################################################## #
      
      cnbc = read.csv("cnbc_data_2021.csv", header = T)
      head(cnbc)
      
      cnbc = cnbc %>% column_to_rownames("State")
      Overall = cnbc$OVERALL
      
      cnbc$OVERALL = NULL
      
      NbClust(cnbc, method = "complete", index = 'hartigan')$Best.nc
      
      result = kcca(cnbc,k=4)
      barplot(result)
      image(result)
      points(cnbc)
      str(result)  
      
      FeatureImp_res <- FeatureImpCluster(result,as.data.table(cnbc))
      plot(FeatureImp_res)
      names(FeatureImp_res)
      
      barplot(FeatureImp_res$featureImp, horiz = FALSE, las = 2)
      
      
      FI = as.data.frame(FeatureImp_res$featureImp)
      names(FI) = "FIMP"
      
      FI = FI %>% rownames_to_column("Feature")
      
      ggplot(FI, aes(x = reorder(Feature, desc(FIMP)), y = FIMP, 
                     fill = as.factor(FIMP))) + 
        geom_col() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
        theme(legend.position = "none") +
        coord_flip()
      
      
      cnbc$cluster = result@cluster
      cnbc$Overall = Overall
      
      cnbc["Connecticut",]
      
      cnbc2 = cnbc %>% filter(cluster==2) 
      
      
      ###PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
      
      
      dat_m = cnbc %>% rownames_to_column("State") 
      head(dat_m)
      
      ggplot(data = dat_m, aes(x = cluster, y = reorder(Overall, desc(Overall)), 
                               label = State, col = as.factor(cluster))) +        
        geom_text(vjust = 1) +
        geom_point(aes(size = cluster), alpha = 0.2) +
        geom_point(aes(size = Overall), alpha = 0.2) +
        geom_jitter() +
        labs(y = "Overall Ranking on CNBC Top States for Doing Business",
             x = "Groupings",
             title = "Top State for Doing Business, by cluster groups",
             caption = "Arod") +
        theme(legend.position = "null")
      
      