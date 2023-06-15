
# Load the tidyverse
library(tidyverse, quietly = TRUE)

# Read in the dataset
argentina <- read_csv("datasets/argentina.csv")

# Inspect the first rows of the dataset
nrow(argentina)
head(argentina)

library(testthat) 
library(IRkernel.testthat)

soln_argentina <- readr::read_csv("datasets/argentina.csv")

run_tests({
    
    test_that("The correct package was loaded.", {
        expect_true("tidyverse" %in% .packages(), 
                    info = "Please make sure that the tidyverse package is loaded.")
    })
    
    test_that("Read in data correctly.", {
        
        expect_is(argentina, "tbl_df", info="Did you use read_csv for reading the data in?")
        
        expect_identical(colnames(argentina),
                         colnames(soln_argentina),
            info = "`argentina` has the wrong columns. Did you import datasets/argentina.csv?")
    })
    
    test_that("Read in data correctly.", {
        expect_equivalent(argentina, soln_argentina, 
            info = 'argentina should contain the data in "datasets/argentina.csv"')
    })
})

# Add gdp_per_capita column to argentina
argentina <- argentina %>% 
  mutate(gdp_per_cap = gdp / pop) 

# Find the four richest provinces
( rich_provinces  <- argentina %>% 
arrange(desc(gdp_per_cap)) %>% 
 select(province, gdp_per_cap) %>% 
 top_n(4, gdp_per_cap) )

# Find the provinces with populations over 1 million
( bigger_pops <- argentina %>% 
 arrange(desc(pop)) %>% 
 select(province, pop) %>% 
 filter(pop > 1000000) )

soln_argentina <- soln_argentina %>% 
  mutate(gdp_per_cap = gdp / pop) 

soln_rich_provinces  <- soln_argentina %>% 
  arrange(-gdp_per_cap) %>% 
  select(province, gdp_per_cap) %>% 
  top_n(4)

soln_bigger_pops <- soln_argentina %>% 
  arrange(-pop) %>%          
  select(province, pop) %>%  
  filter(pop > 1e6)


run_tests({
    
    test_that("bigger_pops has correct columns", {
      expect_identical(
          colnames(soln_bigger_pops),
          colnames(bigger_pops), 
        info = "bigger_pops has incorrect columns. Did you select `province` and `pop`?")
    })
    
    test_that("bigger_pops has correct number of rows", {
      expect_true(nrow(bigger_pops) == 9, 
        info = "bigger_pops should have 9 rows")
    })
    
    test_that("bigger_pops has correct values", {
      expect_equivalent(bigger_pops, 
                        soln_bigger_pops, 
        info = "bigger_pops should have the provinces with more than 1 million inhabitants")
    })
    
    test_that("bigger_pops has correct columns", {
      expect_identical(
          colnames(soln_rich_provinces),
          colnames(rich_provinces), 
        info = "rich_provinces has incorrect columns. Did you select `province` and `gdp_per_cap`?")
    })
    
    test_that("rich_provinces has correct number of rows", {
      expect_true(nrow(rich_provinces) == 4, 
        info = "rich_provinces should have 4 rows")
    })
    
    test_that("rich_provinces has be correct ", {
      expect_equivalent(rich_provinces, soln_rich_provinces, 
        info = "rich_provinces should have the top 4 provinces by gdp_per_cap")
    })
})

# Select numeric columns and cast to matrix
argentina_matrix  <- argentina  %>% 
  select_if(is.numeric) %>%  
  as.matrix()

# Print the first lines of the result
head(argentina_matrix)

soln_argentina_matrix <- soln_argentina %>% 
                           select_if(is.numeric) %>%  
                           as.matrix 

run_tests({
    
    test_that("argentina_matrix is a matrix", {
      expect_true(class(argentina_matrix) == "matrix", 
        info = "Did you use as.matrix to cast your data.frame to a matrix?")
    })

    test_that("argentina_matrix has the correct data", {
        expect_equal(argentina_matrix, soln_argentina_matrix,
                    info = "argentina_matrix does not have the correct data. ")
    })
})

# Load FactoMineR
library(FactoMineR)

# Apply PCA and print results
( argentina_pca  <- PCA(argentina_matrix, scale.unit = TRUE) )

soln_argentina_pca <- FactoMineR::PCA(soln_argentina_matrix, scale.unit = TRUE)

run_tests({
    
    test_that("The correct package was loaded.", {
        expect_true("FactoMineR" %in% .packages(), 
                    info = "Please make sure that the FactoMineR package is loaded.")
    })
    
    test_that("Variance is scaled", {

      expect_true("PCA" %in% class(argentina_pca),
        info = "Did you use PCA from FactoMineR?")
    })
    
    test_that("Variance is scaled", {
      skip("skip slow test")
      argentina_pca_wrong <- PCA(argentina_matrix, scale.unit = FALSE)

      expect_false(argentina_pca$eig[1,1]==argentina_pca_wrong$eig[1,1],
        info = "Did you set scale.units to TRUE?")
    })
    
    test_that("Results are correct", {
        
      expect_true(argentina_pca$eig[1,1]==soln_argentina_pca$eig[1,1],
        info = "Did you apply PCA to argentina_matrix?")
    })  
})

# Load factoextra
library(factoextra)

# Set the size of plots in this notebook
options(repr.plot.width=7, repr.plot.height=5)

# Plot the original variables and the first 2 components and print the plot object.
( pca_var_plot <- fviz_pca_var(argentina_pca) )

# Sum the variance preserved by the first two components. Print the result.
( variance_first_two_pca <- argentina_pca$eig[1,2] + argentina_pca$eig[2,2] )

student_plot <- pca_var_plot
soln_pca_var_plot <- fviz_pca_var(soln_argentina_pca)


run_tests({
    
    test_that("The correct package was loaded.", {
        expect_true("FactoMineR" %in% .packages(), 
                    info = "Please make sure that the FactoMineR package is loaded.")
    })
       
    test_that("pca_var_plot is correct", {
            
      expect_s3_class(student_plot, "ggplot")
         
      expect_identical(soln_pca_var_plot$data, 
                       student_plot$data,
        info = "Did you use fviz_pca_var with argentina_pca as argument?")
        
      expect_identical(
          
        deparse(student_plot$mapping$x),
        deparse(soln_pca_var_plot$mapping$x),
          info = 'The `x` aesthetic is incorrect. Did you change the default settings to fviz_pca_var?'
      )  
        
      expect_identical(
          
        deparse(student_plot$mapping$y),
        deparse(soln_pca_var_plot$mapping$y),
          info = 'The `y` aesthetic is incorrect. Did you change the default settings to fviz_pca_var?'
      )
    })
    
    
    test_that("variance_first_two_pca is the sum of the first 2 component's variance", {

      expect_equal(variance_first_two_pca, 63.5489736117608, tolerance=1e-4,
        info = "Did you add the variance of the first two components?")
    })
})

# Visualize Dim2 vs. Dim1
fviz_pca_ind(argentina_pca,
            title = "Provinces - PCA"
            )

student_plot <- last_plot()
soln_plot <- fviz_pca_ind(soln_argentina_pca, title = "Provinces - PCA") 

run_tests({
    
    test_that("Student plot is correct", {
        
      expect_equivalent(
        student_plot$data,
        soln_plot$data,
        info = "Did you use argentina_pca as input to fviz_pca_ind?"
      )
    })
    
    
    test_that("last plot has correct labels", {
        
      expect_equal(student_plot$labels$title, 
                   soln_plot$labels$title, 
        info = "Did you add the title 'Provinces - PCA?'")
        
      expect_equivalent(
        student_plot$labels,
        soln_plot$labels,
        info = "Did you use the default settings for fviz_pca_ind?")
    })
})

# Set seed to 1234 for reproducibility
set.seed(1234)

# Create an intermediate data frame with pca_1 and pca_2
argentina_comps <- tibble(pca_1 = argentina_pca$ind$coord[ ,"Dim.1"],  
                          pca_2 = argentina_pca$ind$coord[ ,"Dim.2"])

# Cluster the observations using the first 2 components and print its contents
( argentina_km <- kmeans(argentina_comps, centers = 4, nstart = 20, iter.max = 50) )

soln_argentina_comps <- tibble(pca_1 = soln_argentina_pca$ind$coord[,1],  
                               pca_2 = soln_argentina_pca$ind$coord[,2])

run_tests({
    
    test_that("intermediate data frame is correct", {    
      expect_equal(argentina_comps, 
                   soln_argentina_comps, 
        info = "argentina_comps must have one row per province and only 2 columns, pca_1 and pca_2")
    })
    
    test_that("kmeans finds correct number of clusters", {
        
      expect_equal(nrow(argentina_km$centers), 4, 
        info = "Did you create 4 clusters?")
    })
})

# Convert assigned clusters to factor
clusters_as_factor <- factor(argentina_km$cluster)

# Plot individulas colored by cluster
fviz_pca_ind(argentina_pca, 
             title = "Clustered Provinces - PCA", 
             habillage = clusters_as_factor) 

student_plot <- last_plot()
soln_plot <- fviz_pca_ind(soln_argentina_pca, 
             title = "Clustered Provinces - PCA", 
             habillage=factor(argentina_km$cluster))

run_tests({
      
    test_that("clusters_as_factors is a factor", {
                
      expect_is(clusters_as_factor, "factor",
               info = "Did you convert clusters to a factor?")
                
    })
    
   
    test_that("last plot has correct labels", {
                
      expect_equal(student_plot$labels$x, 
                   soln_plot$labels$x,
        info = "Did you use the default labels for the x axis?")
        
    })
    
    test_that("last plot has correct labels", {
        
      expect_equal(student_plot$labels$y, 
                   soln_plot$labels$y,
        info = "Did you use the default labels for the y axis?")
                
    })
    
    test_that("last plot has correct labels", {
        
      expect_equal(student_plot$labels$title, 
                    soln_plot$labels$title,
        info = "Did you add the title Clustered Provinces - PCA?")
    })
    
    test_that("last plot has Groups column in data ", {
        
      expect_equal(student_plot$data$Groups, 
                   soln_plot$data$Groups,
        info = "Did you set the habillage argument when calling fviz_pca_ind?")
    })

})

# Load ggrepel
library(ggrepel)

# Add cluster column to argentina
argentina <- argentina %>%
               mutate(cluster=clusters_as_factor)

# Make a scatterplot of gdp vs. cluster, colored by cluster
ggplot(argentina, aes(cluster, gdp, color = cluster)) +
  geom_point() +
  geom_text_repel(aes(label = province), show.legend = FALSE) +
  labs(x = "Cluster", y = "GDP")

student_plot <- last_plot()

soln_argentina_km <- kmeans(soln_argentina_comps, centers = 4, nstart = 20, iter.max = 50) 

soln_argentina <- mutate(soln_argentina, 
                           cluster=factor(argentina_km$cluster))

# Make a scatterplot of gdp vs. cluster, colored by cluster
soln_plot <- ggplot(soln_argentina, 
                    aes(cluster, gdp, color=cluster)) +
             geom_point() +
             ggrepel::geom_text_repel(aes(label=province), show.legend=FALSE) +
             labs(x="Cluster", y="GDP")


run_tests({
    
    test_that("The correct package was loaded.", {
        expect_true("ggrepel" %in% .packages(), 
                    info = "Please make sure that the ggrepel package is loaded.")
    })
    
    
    test_that("added cluster factor", {
        
      expect_s3_class(argentina$cluster, "factor")
        
      expect_identical(
        colnames(argentina),
        colnames(soln_argentina),
        info = "`argentina` does not have the correct columns")
        
      expect_true(
        "cluster" %in% colnames(argentina),
        info = "Did you add a cluster column to `argentina`?"
      )
        
    })
      
    test_that("The plot is correct", {
        
      expect_s3_class(student_plot, "ggplot")
        
      expect_identical(
        soln_plot$data,
        student_plot$data,
          info = "Did you use `argentina` for your plot?"
      )
        
      expect_identical(
        deparse(student_plot$mapping$x),
        deparse(soln_plot$mapping$x),
        info = 'The `x` aesthetic is incorrect. Did you map it to `cluster`?'
      )
        
      expect_identical(
        deparse(student_plot$mapping$y),
        deparse(soln_plot$mapping$y),
        info = 'The `y` aesthetic is incorrect. Did you map it to `gdp`?'
      )
        
      expect_identical(
        deparse(student_plot$mapping$group),
        deparse(soln_plot$mapping$group),
        info = 'The `group` aesthetic is incorrect. Did you map it to `cluster`?'
      )
                
      expect_identical(
        student_plot$labels$x,
        soln_plot$labels$x,
        info = "The label in the x-axis is incorrect. Did you set it to `Cluster`?"
      )
      expect_identical(
        student_plot$labels$y,
        soln_plot$labels$y,
        info = "The label in the y-axis is incorrect. Did you set it to `GDP`?"
      )
    })
    
    # Testing the geoms with %in% 
    test_that("last plot has geom_point and geom_text_repel layers", {
        
      student_plot_layers <- student_plot$layers
      student_plot_layers_geoms <- map(student_plot_layers, ~class(.$geom)[[1]])

      expect_true("GeomPoint" %in% student_plot_layers_geoms,
        info = "Did you add a geom_point() layer to your plot?")
        
      expect_true("GeomTextRepel" %in% student_plot_layers_geoms,
        info = "Did you add a geom_text_repel() layer to your plot?")
    })
    
})

# Make a scatterplot of GDP per capita vs. cluster, colored by cluster
ggplot(argentina, aes(cluster, gdp_per_cap, color = cluster)) +
  geom_point() +
  geom_text_repel(aes(label = province), show.legend = FALSE) +
  labs(x = "Cluster", y = "GDP per capita")

student_plot <- last_plot()

soln_plot <- ggplot(soln_argentina, aes(cluster, gdp_per_cap, color=cluster)) +
  geom_point() + 
  geom_text_repel(aes(label=province), show.legend=FALSE) +
  labs(x="Cluster", y="GDP per capita")


run_tests({
    
      
    test_that("The plot is correct", {
        
      expect_s3_class(student_plot, "ggplot")
        
      expect_identical(
        soln_plot$data,
        student_plot$data,
          info = "Did you use `argentina` for your plot?"
      )
        
      expect_identical(
        deparse(student_plot$mapping$x),
        deparse(soln_plot$mapping$x),
        info = 'The `x` aesthetic is incorrect. Did you map it to `cluster`?'
      )
        
      expect_identical(
        deparse(student_plot$mapping$y),
        deparse(soln_plot$mapping$y),
        info = 'The `y` aesthetic is incorrect. Did you map it to `gdp_per_cap`?'
      )
        
      expect_identical(
        deparse(student_plot$mapping$group),
        deparse(soln_plot$mapping$group),
        info = 'The `group` aesthetic is incorrect. Did you map it to `cluster`?'
      )
                        
      expect_identical(
        student_plot$labels$x,
        soln_plot$labels$x,
        info = "The label in the x-axis is incorrect. Did you set it to `Cluster`?"
      )
      expect_identical(
        student_plot$labels$y,
        soln_plot$labels$y,
        info = "The label in the y-axis is incorrect. Did you set it to `GDP per capita`?"
      )
    })
    
    # Testing the geoms with %in% 
    test_that("last plot has geom_point and geom_text_repel layers", {
        
      student_plot_layers <- student_plot$layers
      student_plot_layers_geoms <- map(student_plot_layers, ~class(.$geom)[[1]])

      expect_true("GeomPoint" %in% student_plot_layers_geoms,
        info = "Did you add a geom_point() layer to your plot?")
        
      expect_true("GeomTextRepel" %in% student_plot_layers_geoms,
        info = "Did you add a geom_text_repel() layer to your plot?")
    })
    
})

# Make scatterplot of poverty vs. cluster, colored by cluster
ggplot(argentina, aes(cluster, poverty, color = cluster)) +
  geom_point() +
  labs(x = "Cluster", y = "Poverty rate") +
  geom_text_repel(aes(label = province), show.legend = FALSE)

student_plot <- last_plot()

soln_plot <- ggplot(soln_argentina, aes(cluster, poverty, color = cluster)) +
  geom_point() +
  geom_text_repel(aes(label=province), show.legend = FALSE) +
  labs(x="Cluster", y="Poverty rate")


run_tests({
    
      
    test_that("The plot is correct", {
        
      expect_s3_class(student_plot, "ggplot")
        
      expect_identical(
        soln_plot$data,
        student_plot$data,
          info = "Did you use `argentina` for your plot?"
      )
        
      expect_identical(
        deparse(student_plot$mapping$x),
        deparse(soln_plot$mapping$x),
        info = 'The `x` aesthetic is incorrect. Did you map it to `cluster`?'
      )
        
      expect_identical(
            deparse(student_plot$mapping$y),
            deparse(soln_plot$mapping$y),
            info = 'The `y` aesthetic is incorrect. Did you map it to `poverty`?'
      )
        
      expect_identical(
            deparse(student_plot$mapping$group),
            deparse(soln_plot$mapping$group),
            info = 'The `group` aesthetic is incorrect. Did you map it to `cluster`?'
      )
        
        
      # Should we test specific labels?
                        
      expect_identical(
            student_plot$labels$x,
            soln_plot$labels$x,
            info = "The label in the x-axis is incorrect. Did you set it to `Cluster`?"
      )
      expect_identical(
            student_plot$labels$y,
            soln_plot$labels$y,
            info = "The label in the y-axis is incorrect. Did you set it to `Poverty`?"
      )
    })
    
    # Testing the geoms with %in% 
    test_that("last plot has geom_point and geom_text_repel layers", {
        
      student_plot_layers <- student_plot$layers
      student_plot_layers_geoms <- map(student_plot_layers, ~class(.$geom)[[1]])

      expect_true("GeomPoint" %in% student_plot_layers_geoms,
        info = "Did you add a geom_point() layer to your plot?")
        
      expect_true("GeomTextRepel" %in% student_plot_layers_geoms,
        info = "Did you add a geom_text_repel() layer to your plot?")
    })
    
})

# Assign pilot provinces to the most diverse group
pilot_provinces <- 3

run_tests({
    test_that("the answer is 1, 2 or 3", {
      expect_true(as.numeric(pilot_provinces) %in% c(1, 2, 3), 
        info = "You should choose one of groups 1, 2 or 3")
    })
    test_that("the answer is not 1", {
      expect_false(as.numeric(pilot_provinces) %in% c(1, 2), 
        info = "This set of provinces is on the same cluster, and are very similar")
    })
})
