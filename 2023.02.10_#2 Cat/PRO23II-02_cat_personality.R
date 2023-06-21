# library
library(tidyverse)
library(fmsb)
library(ggradar)
library(psych)
library(scales)

# data
blueprint <- read_csv("2023.02.10_#2 Cat/Data/3. Analysis Data/2. blueprint_feline_dataset.csv")
df <- read_csv("2023.02.10_#2 Cat/Data/3. Analysis Data/1. feline_dataset.csv")
breed_blueprint <- read.csv("2023.02.10_#2 Cat/Data/3. Analysis Data/3. blueprint_cat_breed.csv")
EFA_blueprint <- read.csv("2023.02.10_#2 Cat/Data/3. Analysis Data/blueprint_EFA.csv")

# cleaning
  # NA in blueprint
    blueprint$Factor <- gsub("#N/A", NA, blueprint$Factor) #ganti NA versi excel ke versi R
    blueprint <- na.omit(blueprint)
    

# descriptive
  # samples
  n <- count(df)
  unique(df$BREEDGROUP)
  # how many breeds
  n_breed <- length(unique(breed_blueprint$breed))
  # research title
  research_title <- "Reliability and Validity of Seven Feline Behavior and Personality Traits"
  # author
  research_author <- "Mikkola, Salla., et.al."
  # research summary
  research_summary <- "Kucing memiliki kepribadian dan karakteristik unik yang mempengaruhi kebutuhan lingkungan mereka untuk mendapatkan kualitas hidup yang baik. Sebuah studi terhadap lebih dari 4300 kucing menemukan bahwa beberapa karakteristik kepribadian lebih umum ditemukan pada jenis-jenis tertentu."
  # n by breed
  n_by_breed <- df %>%
  group_by(BREEDGROUP) %>%
  summarise(count = n())

# radar prep
items <- df[ ,c(1,which(colnames(df) %in% blueprint$Variables))]
colnames(items) <- c("CatID",blueprint$Code)

  # recode
  items[-1] <- reverse.code(blueprint$Key,items[-1], mini = 1, maxi = 5)
  
  #factor score
  f1 <- rowMeans(items[blueprint$Code[which(blueprint$Factor=="Factor 1")]], na.rm = TRUE)
  f2 <- rowMeans(items[blueprint$Code[which(blueprint$Factor=="Factor 2")]], na.rm = TRUE)
  f3 <- rowMeans(items[blueprint$Code[which(blueprint$Factor=="Factor 3")]], na.rm = TRUE)
  f4 <- rowMeans(items[blueprint$Code[which(blueprint$Factor=="Factor 4")]], na.rm = TRUE)
  f5 <- rowMeans(items[blueprint$Code[which(blueprint$Factor=="Factor 5")]], na.rm = TRUE)
  f6 <- rowMeans(items[blueprint$Code[which(blueprint$Factor=="Factor 6")]], na.rm = TRUE)
  f7 <- rowMeans(items[blueprint$Code[which(blueprint$Factor=="Factor 7")]], na.rm = TRUE)
  
  # new df. diganti jadi per faktor bukan per item lagi
  new_df <- data.frame(
    catID = df$CATID,
    breed = df$BREEDGROUP,
    age = df$AGE_BEHAVIOUR,
    other_cat = df$OTHER_CATS,
    problematic = df$problematic_behavior,
    f1 = f1,
    f2 = f2,
    f3 = f3,
    f4 = f4,
    f5 = f5,
    f6 = f6,
    f7 = f7
  )
  
  # ganti underscore jadi spasi
  new_df$breed <- gsub("_"," ",new_df$breed)
  
  # new df diganti per breed
  f_scores <- matrix(ncol=7, nrow=20)
  for (i in 1:n_breed) {
    f_scores[i,] <- round(colMeans(new_df[which(new_df$breed == breed_blueprint$breed[i]),6:12],na.rm = TRUE),1)
  }
  
  f_scores <- data.frame(f_scores) # ubah matrix ke dataframe
  
  # buat new_df2
  new_df2 <- mutate(
    f_scores, breed = breed_blueprint$breed
  )
  
  new_df2 <- new_df2[,c(8,1:7)] # pindah urutan kolom, breed paling depan
  colnames(new_df2) <- c("breed",EFA_blueprint$Name)
  
  # rescale data
  new_df3 <- new_df2
   new_df3[2] <- rescale(unlist(new_df2[2]), to = c(2.4,5)) # Petualang: min = 3.4, max = 4.0
   new_df3[3] <- rescale(unlist(new_df2[3]), to = c(1,4.3)) # Galak: min = 1.1, max = 2.3
   new_df3[4] <- rescale(unlist(new_df2[4]), to = c(2.3,5)) # Gaul: min = 3.3, max = 4.4
   new_df3[5] <- rescale(unlist(new_df2[5]), to = c(2.6,5)) # Petakilan: min = 3.6, max = 4.2
   new_df3[6] <- rescale(unlist(new_df2[6]), to = c(2.5,5)) # Setia: min = 3.5, max = 4.1
   new_df3[7] <- rescale(unlist(new_df2[7]), to = c(1,3.5)) # Garong: min = 1.9, max = 2.5
   new_df3[8] <- rescale(unlist(new_df2[8]), to = c(2,5)) # Suka Anjing: min = 3.0, max = 4.2
   
# radar chart viz
  # page1: 1-4
  rbind(rep(5,7) , rep(1,7) , new_df3[1:4,-1]) %>%
    radarchart(
      pch = 25,
      cglty = 1, cglcol = "#bdbdbd", cglwd = 2,
      caxislabels = c(1,2,3,4,5),
      plty = 1, plwd = 3,
      pcol = c("#C69759", "#9F8E81", "#D16B5E", "#5E7983"),
      axislabcol = "#4e4f4f", axistype = 4
    )
  
  # page2: 5-8
  rbind(rep(5,7) , rep(1,7) , new_df3[5:8,-1]) %>%
    radarchart(
      pch = 25,
      cglty = 1, cglcol = "#bdbdbd", cglwd = 1.5,
      caxislabels = c(1,2,3,4,5),
      plty = 1, plwd = 3,
      pcol = c("#E6BE2F", "#DF9A9C", "#755239", "#000000"),
      axislabcol = "#4e4f4f", axistype = 4
    )
  
  # page3: 9-12
  rbind(rep(5,7) , rep(1,7) , new_df3[9:12,-1]) %>%
    radarchart(
      pch = 25,
      cglty = 1, cglcol = "#bdbdbd", cglwd = 1.5,
      caxislabels = c(1,2,3,4,5),
      plty = 1, plwd = 3,
      pcol = c("#727271", "#634E42", "#3AAA35", "#000000"),
      axislabcol = "#4e4f4f", axistype = 4
    )
  
  # page4: 13-16
  rbind(rep(5,7) , rep(1,7) , new_df3[13:16,-1]) %>%
    radarchart(
      pch = 25,
      cglty = 1, cglcol = "#bdbdbd", cglwd = 1.5,
      caxislabels = c(1,2,3,4,5),
      plty = 1, plwd = 3,
      pcol = c("#DF8029", "#7CCBE0", "#5B5E63", "#A78F81"),
      axislabcol = "#4e4f4f", axistype = 4
    )
  
  # page5: 17-20
  rbind(rep(5,7) , rep(1,7) , new_df3[17:20,-1]) %>%
    radarchart(
      pch = 25,
      cglty = 1, cglcol = "#bdbdbd", cglwd = 1.5,
      caxislabels = c(1,2,3,4,5),
      plty = 1, plwd = 3,
      pcol = c("#DF8029", "#AC52BB", "#323232", "#E191C9"),
      axislabcol = "#4e4f4f", axistype = 4
    )
