<<<<<<< HEAD
# Earthquake viz
# libraries
library(lubridate)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(grid)
library(gridtext)
library(ggtext)

# data
df <- read.csv("2023.02.10_#1 Earthquake/export_EMSC.csv", sep = ";")
df$Date <- as_date(df$Date)
#df["label"] <- NA

# labels
  # identify events
  cianjur <- which(df$Date == "2022-11-21" & df$Magnitude > 5) #cianjur, nov, intensity IV
  maluku <- which(df$Magnitude == max(df$Magnitude)) #terbesar, maluku, hanya bertahan 7 jam, intensity V
  which(df$Magnitude == Rfast::nth(df$Magnitude,2,descending = T)) # second, bengkulu, agak jauh 
  which(month(df$Date) == 9 & df$Date < as_date("2023-01-01") & df$Magnitude == 6.2) #cianjur, nov, intensity IV
  
# plot
# dev.new(width = 8, height = 5, units = "in", noRStudioGD = T)
  #en----
  ggplot(df, aes(Date, Magnitude, color = Magnitude)) +
    annotate(geom = "rect", xmin = ymd("2022-12-20"), ymin = 2.5, xmax =ymd("2023-01-16"), ymax = Inf,
             fill = alpha("#b4c5e4",0.2), color = alpha("#264653",.5), linetype = "dashed") + #rectangle piala AFF
    annotate(geom = "rect", xmin = ymd("2022-11-20"), ymin = 2.5, xmax =ymd("2022-12-18"), ymax = Inf,
             fill = alpha("#b4c5e4",0.2), color = alpha("#264653",.5), linetype = "dashed") + #rectangle piala dunia
    annotate("segment",
             x = ymd(df$Date[cianjur]), y = df$Magnitude[cianjur],
             xend = ymd(df$Date[cianjur]), yend= df$Magnitude[cianjur]+.9,
             color = "grey80") +
    geom_jitter(data = df[-maluku,] %>% filter(Magnitude > 5.5), aes(size = Magnitude),
               alpha = 0.9, shape = 16, stroke = 1, color = "#82667f") +
    geom_jitter(data = df[-cianjur,] %>% filter(Magnitude <= 5.5), aes(alpha = Magnitude, color = Magnitude),
               size = 1.7, shape = 16, stroke = 1) + # <= 5.5, tanpa cianjur
    geom_point(data = df[cianjur,],
                size = 3, shape = 21, stroke = 1.5, color = "#a53860", fill = "#d68db8") + #gempa cianjur
    geom_point(data = df[maluku,],
               size = 3, shape = 21, stroke = 1.5, color = "#525775", fill = "#9ba0bc") + #gempa maluku
    ylim(2.5,8.5) +
    scale_alpha_continuous(range = c(0.1, .9)) +
    scale_color_gradient(low = "grey", high = "#82667f") +
    scale_size_continuous(range = c(2,3),limits = c(min(df$Magnitude[df$Magnitude > 5]), max(df$Magnitude)))+
    scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
    theme_swd() +
    guides(color = FALSE, size = FALSE, alpha = FALSE) +
    theme(plot.background = element_rect(fill = "white")) +
    annotate(geom = "richtext",
             label = "<b style='color:#bd4089'>Cianjur Earthquake | Intensity IV</b><br>
             <i>Even though not as big as in Maluku,<br>
             the exposure was quite high.<br>
             Probably because it is close to Jakarta.</i>",
             x = df$Date[cianjur], y = df$Magnitude[cianjur]+1,
             color = GRAY5, fill = alpha("white",0.5), label.color = NA, hjust = 1, vjust = 1, size = 3.5) +
    annotate(geom = "richtext",
             label = "<b style='color:#525775'>Maluku Earthquake | Intensity V</b><br>
             <i>The biggest earthquake since June, 2022<br>
             The internet search is not even 1/10 of Cianjur Earthquake.<br>
             Probably because it is outside Java and coincided with AFF Cup.</i>",
             x = df$Date[maluku]-2, y = df$Magnitude[maluku],
             color = GRAY5, fill = alpha("white",0.5), label.color = NA, hjust = 1, vjust = 0.5, size = 3.5) +
    annotate(geom = "richtext",
             label = "<i> Wolrd Cup</i>",
             x = mean(c(ymd("2022-11-20"), ymd("2022-12-18"))), y = 8.5,
             color = GRAY5, fill = NA, label.color = NA, hjust = 0.5, vjust = 0.5, size = 3) + #world cup label
    annotate(geom = "richtext",
             label = "<i> AFF Cup</i>",
             x = mean(c(ymd("2022-12-20"), ymd("2023-1-16"))), y = 8.5,
             color = GRAY5, fill = NA, label.color = NA, hjust = 0.5, vjust = 0.5, size = 3) + #AFF cup label
    labs(title = "<b style='font-size:15pt;color:#264653'> Latest Earthquake in Indonesia </b><br>
         <i style='font-family:Helvetica-Light;color:#8e9091'>In fact, there are a lot of earthquake happened. But most of them didn't<br> have really significant impact.<b style='color:#d1a5be'> While the other, even though it wasn't<br> the biggest, it grabbed a lot of social media attention</b></i>",
         caption = "*The data was taken on Friday, February 17<sup>th</sup>. 5000 records collected with the oldest is from June 24<sup>th</sup> 2022.<br>
         **Internet search analysis using Ubersuggest by Neil Patel<br>
         ***Source: EMSC") +
    theme(
      plot.title.position = "plot",
      plot.title = element_markdown(size = 13, lineheight = 1.2),
      plot.caption = element_markdown(size = 9, lineheight = 1))

  #id----
  ggplot(df, aes(Date, Magnitude, color = Magnitude)) +
    annotate(geom = "rect", xmin = ymd("2022-12-20"), ymin = 2.5, xmax =ymd("2023-01-16"), ymax = Inf,
             fill = alpha("#b4c5e4",0.2), color = alpha("#264653",.5), linetype = "dashed") + #rectangle piala AFF
    annotate(geom = "rect", xmin = ymd("2022-11-20"), ymin = 2.5, xmax =ymd("2022-12-18"), ymax = Inf,
             fill = alpha("#b4c5e4",0.2), color = alpha("#264653",.5), linetype = "dashed") + #rectangle piala dunia
    annotate("segment",
             x = ymd(df$Date[cianjur]), y = df$Magnitude[cianjur],
             xend = ymd(df$Date[cianjur]), yend= df$Magnitude[cianjur]+.9,
             color = "grey80") +
    geom_jitter(data = df[-maluku,] %>% filter(Magnitude > 5.5), aes(size = Magnitude),
                alpha = 0.9, shape = 16, stroke = 1, color = "#82667f") +
    geom_jitter(data = df[-cianjur,] %>% filter(Magnitude <= 5.5), aes(alpha = Magnitude, color = Magnitude),
                size = 1.7, shape = 16, stroke = 1) + # <= 5.5, tanpa cianjur
    geom_point(data = df[cianjur,],
               size = 3, shape = 21, stroke = 1.5, color = "#a53860", fill = "#d68db8") + #gempa cianjur
    geom_point(data = df[maluku,],
               size = 3, shape = 21, stroke = 1.5, color = "#525775", fill = "#9ba0bc") + #gempa maluku
    ylim(2.5,8.5) +
    scale_alpha_continuous(range = c(0.1, .9)) +
    scale_color_gradient(low = "grey", high = "#82667f") +
    scale_size_continuous(range = c(2,3),limits = c(min(df$Magnitude[df$Magnitude > 5]), max(df$Magnitude)))+
    scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
    theme_swd() +
    guides(color = FALSE, size = FALSE, alpha = FALSE) +
    theme(plot.background = element_rect(fill = "white")) +
    annotate(geom = "richtext",
             label = "<b style='color:#bd4089'>Gempa Cianjur | Intensitas IV</b><br>
             <i>Meskipun tidak sebesar gempa Maluku,<br>
             percakapan di internet relatif tinggi. Salah satu<br>
             faktornya karena gempa ini terasa di Jabodetabek.</i>",
             x = df$Date[cianjur], y = df$Magnitude[cianjur]+1,
             color = GRAY5, fill = alpha("white",0.5), label.color = NA, hjust = 1, vjust = 1, size = 3.5) +
    annotate(geom = "richtext",
             label = "<b style='color:#525775'>Gempa Maluku | Intensitas V</b><br>
             <i>Gempa terbesar sejak Juni 2022. Percakapan di internet tidak<br>
              sampai 1/10 Gempa Cianjur. Beberapa alasannya karena terjadi di luar Jawa<br>
             dan berbarengan dengan pertandingan Timnas di Piala AFF.</i>",
             x = df$Date[maluku]-2, y = df$Magnitude[maluku],
             color = GRAY5, fill = alpha("white",0.5), label.color = NA, hjust = 1, vjust = 0.5, size = 3.5) +
    annotate(geom = "richtext",
             label = "<i> Piala Dunia </i>",
             x = mean(c(ymd("2022-11-20"), ymd("2022-12-18"))), y = 8.5,
             color = GRAY5, fill = NA, label.color = NA, hjust = 0.5, vjust = 0.5, size = 3) + #world cup label
    annotate(geom = "richtext",
             label = "<i> Piala AFF </i>",
             x = mean(c(ymd("2022-12-20"), ymd("2023-1-16"))), y = 8.5,
             color = GRAY5, fill = NA, label.color = NA, hjust = 0.5, vjust = 0.5, size = 3) + #AFF cup label
    labs(title = "<b style='font-size:15pt;color:#264653'> Gempa Terkini di Indonesia </b><br>
         <i style='font-family:Helvetica-Light;color:#8e9091'>Faktanya, sangat banyak gempa terjadi di Indonesia. Namun sebagian besar<br>
         tidak begitu signifikan.<b style='color:#d1a5be'> Beberapa gempa lainnya, walaupun bukan yang paling besar,<br> menyedot banyak perhatian netizen Indonesia</b></i>",
         caption = "*Data diambil pada Jumat 17 Februari. Terdapat 5000 data dengan yang paling lama tanggal 24 Juni 2022.<br>
         **Analisis percakapan internet berdasarkan aplikasi Ubersuggest oleh Neil Patel.<br>
         ***Sumber catatan gempa: EMSC.") +
    theme(
      plot.title.position = "plot",
      plot.title = element_markdown(size = 13, lineheight = 1.2),
      plot.caption = element_markdown(size = 9, lineheight = 1))
  
=======
# Earthquake viz
# libraries
library(lubridate)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(grid)
library(gridtext)
library(ggtext)

# data
df <- read.csv("2023.02.10_#1 Earthquake/export_EMSC.csv", sep = ";")
df$Date <- as_date(df$Date)
#df["label"] <- NA

# labels
  # identify events
  cianjur <- which(df$Date == "2022-11-21" & df$Magnitude > 5) #cianjur, nov, intensity IV
  maluku <- which(df$Magnitude == max(df$Magnitude)) #terbesar, maluku, hanya bertahan 7 jam, intensity V
  which(df$Magnitude == Rfast::nth(df$Magnitude,2,descending = T)) # second, bengkulu, agak jauh 
  which(month(df$Date) == 9 & df$Date < as_date("2023-01-01") & df$Magnitude == 6.2) #cianjur, nov, intensity IV
  
# plot
# dev.new(width = 8, height = 5, units = "in", noRStudioGD = T)
  #en----
  ggplot(df, aes(Date, Magnitude, color = Magnitude)) +
    annotate(geom = "rect", xmin = ymd("2022-12-20"), ymin = 2.5, xmax =ymd("2023-01-16"), ymax = Inf,
             fill = alpha("#b4c5e4",0.2), color = alpha("#264653",.5), linetype = "dashed") + #rectangle piala AFF
    annotate(geom = "rect", xmin = ymd("2022-11-20"), ymin = 2.5, xmax =ymd("2022-12-18"), ymax = Inf,
             fill = alpha("#b4c5e4",0.2), color = alpha("#264653",.5), linetype = "dashed") + #rectangle piala dunia
    annotate("segment",
             x = ymd(df$Date[cianjur]), y = df$Magnitude[cianjur],
             xend = ymd(df$Date[cianjur]), yend= df$Magnitude[cianjur]+.9,
             color = "grey80") +
    geom_jitter(data = df[-maluku,] %>% filter(Magnitude > 5.5), aes(size = Magnitude),
               alpha = 0.9, shape = 16, stroke = 1, color = "#82667f") +
    geom_jitter(data = df[-cianjur,] %>% filter(Magnitude <= 5.5), aes(alpha = Magnitude, color = Magnitude),
               size = 1.7, shape = 16, stroke = 1) + # <= 5.5, tanpa cianjur
    geom_point(data = df[cianjur,],
                size = 3, shape = 21, stroke = 1.5, color = "#a53860", fill = "#d68db8") + #gempa cianjur
    geom_point(data = df[maluku,],
               size = 3, shape = 21, stroke = 1.5, color = "#525775", fill = "#9ba0bc") + #gempa maluku
    ylim(2.5,8.5) +
    scale_alpha_continuous(range = c(0.1, .9)) +
    scale_color_gradient(low = "grey", high = "#82667f") +
    scale_size_continuous(range = c(2,3),limits = c(min(df$Magnitude[df$Magnitude > 5]), max(df$Magnitude)))+
    scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
    theme_swd() +
    guides(color = FALSE, size = FALSE, alpha = FALSE) +
    theme(plot.background = element_rect(fill = "white")) +
    annotate(geom = "richtext",
             label = "<b style='color:#bd4089'>Cianjur Earthquake | Intensity IV</b><br>
             <i>Even though not as big as in Maluku,<br>
             the exposure was quite high.<br>
             Probably because it is close to Jakarta.</i>",
             x = df$Date[cianjur], y = df$Magnitude[cianjur]+1,
             color = GRAY5, fill = alpha("white",0.5), label.color = NA, hjust = 1, vjust = 1, size = 3.5) +
    annotate(geom = "richtext",
             label = "<b style='color:#525775'>Maluku Earthquake | Intensity V</b><br>
             <i>The biggest earthquake since June, 2022<br>
             The internet search is not even 1/10 of Cianjur Earthquake.<br>
             Probably because it is outside Java and coincided with AFF Cup.</i>",
             x = df$Date[maluku]-2, y = df$Magnitude[maluku],
             color = GRAY5, fill = alpha("white",0.5), label.color = NA, hjust = 1, vjust = 0.5, size = 3.5) +
    annotate(geom = "richtext",
             label = "<i> Wolrd Cup</i>",
             x = mean(c(ymd("2022-11-20"), ymd("2022-12-18"))), y = 8.5,
             color = GRAY5, fill = NA, label.color = NA, hjust = 0.5, vjust = 0.5, size = 3) + #world cup label
    annotate(geom = "richtext",
             label = "<i> AFF Cup</i>",
             x = mean(c(ymd("2022-12-20"), ymd("2023-1-16"))), y = 8.5,
             color = GRAY5, fill = NA, label.color = NA, hjust = 0.5, vjust = 0.5, size = 3) + #AFF cup label
    labs(title = "<b style='font-size:15pt;color:#264653'> Latest Earthquake in Indonesia </b><br>
         <i style='font-family:Helvetica-Light;color:#8e9091'>In fact, there are a lot of earthquake happened. But most of them didn't<br> have really significant impact.<b style='color:#d1a5be'> While the other, even though it wasn't<br> the biggest, it grabbed a lot of social media attention</b></i>",
         caption = "*The data was taken on Friday, February 17<sup>th</sup>. 5000 records collected with the oldest is from June 24<sup>th</sup> 2022.<br>
         **Internet search analysis using Ubersuggest by Neil Patel<br>
         ***Source: EMSC") +
    theme(
      plot.title.position = "plot",
      plot.title = element_markdown(size = 13, lineheight = 1.2),
      plot.caption = element_markdown(size = 9, lineheight = 1))

  #id----
  ggplot(df, aes(Date, Magnitude, color = Magnitude)) +
    annotate(geom = "rect", xmin = ymd("2022-12-20"), ymin = 2.5, xmax =ymd("2023-01-16"), ymax = Inf,
             fill = alpha("#b4c5e4",0.2), color = alpha("#264653",.5), linetype = "dashed") + #rectangle piala AFF
    annotate(geom = "rect", xmin = ymd("2022-11-20"), ymin = 2.5, xmax =ymd("2022-12-18"), ymax = Inf,
             fill = alpha("#b4c5e4",0.2), color = alpha("#264653",.5), linetype = "dashed") + #rectangle piala dunia
    annotate("segment",
             x = ymd(df$Date[cianjur]), y = df$Magnitude[cianjur],
             xend = ymd(df$Date[cianjur]), yend= df$Magnitude[cianjur]+.9,
             color = "grey80") +
    geom_jitter(data = df[-maluku,] %>% filter(Magnitude > 5.5), aes(size = Magnitude),
                alpha = 0.9, shape = 16, stroke = 1, color = "#82667f") +
    geom_jitter(data = df[-cianjur,] %>% filter(Magnitude <= 5.5), aes(alpha = Magnitude, color = Magnitude),
                size = 1.7, shape = 16, stroke = 1) + # <= 5.5, tanpa cianjur
    geom_point(data = df[cianjur,],
               size = 3, shape = 21, stroke = 1.5, color = "#a53860", fill = "#d68db8") + #gempa cianjur
    geom_point(data = df[maluku,],
               size = 3, shape = 21, stroke = 1.5, color = "#525775", fill = "#9ba0bc") + #gempa maluku
    ylim(2.5,8.5) +
    scale_alpha_continuous(range = c(0.1, .9)) +
    scale_color_gradient(low = "grey", high = "#82667f") +
    scale_size_continuous(range = c(2,3),limits = c(min(df$Magnitude[df$Magnitude > 5]), max(df$Magnitude)))+
    scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
    theme_swd() +
    guides(color = FALSE, size = FALSE, alpha = FALSE) +
    theme(plot.background = element_rect(fill = "white")) +
    annotate(geom = "richtext",
             label = "<b style='color:#bd4089'>Gempa Cianjur | Intensitas IV</b><br>
             <i>Meskipun tidak sebesar gempa Maluku,<br>
             percakapan di internet relatif tinggi. Salah satu<br>
             faktornya karena gempa ini terasa di Jabodetabek.</i>",
             x = df$Date[cianjur], y = df$Magnitude[cianjur]+1,
             color = GRAY5, fill = alpha("white",0.5), label.color = NA, hjust = 1, vjust = 1, size = 3.5) +
    annotate(geom = "richtext",
             label = "<b style='color:#525775'>Gempa Maluku | Intensitas V</b><br>
             <i>Gempa terbesar sejak Juni 2022. Percakapan di internet tidak<br>
              sampai 1/10 Gempa Cianjur. Beberapa alasannya karena terjadi di luar Jawa<br>
             dan berbarengan dengan pertandingan Timnas di Piala AFF.</i>",
             x = df$Date[maluku]-2, y = df$Magnitude[maluku],
             color = GRAY5, fill = alpha("white",0.5), label.color = NA, hjust = 1, vjust = 0.5, size = 3.5) +
    annotate(geom = "richtext",
             label = "<i> Piala Dunia </i>",
             x = mean(c(ymd("2022-11-20"), ymd("2022-12-18"))), y = 8.5,
             color = GRAY5, fill = NA, label.color = NA, hjust = 0.5, vjust = 0.5, size = 3) + #world cup label
    annotate(geom = "richtext",
             label = "<i> Piala AFF </i>",
             x = mean(c(ymd("2022-12-20"), ymd("2023-1-16"))), y = 8.5,
             color = GRAY5, fill = NA, label.color = NA, hjust = 0.5, vjust = 0.5, size = 3) + #AFF cup label
    labs(title = "<b style='font-size:15pt;color:#264653'> Gempa Terkini di Indonesia </b><br>
         <i style='font-family:Helvetica-Light;color:#8e9091'>Faktanya, sangat banyak gempa terjadi di Indonesia. Namun sebagian besar<br>
         tidak begitu signifikan.<b style='color:#d1a5be'> Beberapa gempa lainnya, walaupun bukan yang paling besar,<br> menyedot banyak perhatian netizen Indonesia</b></i>",
         caption = "*Data diambil pada Jumat 17 Februari. Terdapat 5000 data dengan yang paling lama tanggal 24 Juni 2022.<br>
         **Analisis percakapan internet berdasarkan aplikasi Ubersuggest oleh Neil Patel.<br>
         ***Sumber catatan gempa: EMSC.") +
    theme(
      plot.title.position = "plot",
      plot.title = element_markdown(size = 13, lineheight = 1.2),
      plot.caption = element_markdown(size = 9, lineheight = 1))
  
>>>>>>> 74cf6e1011cc9d7a7b3da059cff25ee0c61d8462
  