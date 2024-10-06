# Community Water Monitoring Network - Chloride Data Visualization 
# Last update Oct 2024
# Program website: 
# Program contacts: 

library(tidyverse)
library(lubridate)
library(cowplot)
library(ggpubr)

setwd("C:/Users/grace/OneDrive/Desktop")

allchem = read_csv("Ponds Chemistry Aug 2024.csv")

#Subset to surface water data
chem = allchem %>%
  #subset to volunteer-collected, storm sampling, water column, and side of pond sampling
  #excluding benthic (sediment) samples
  filter(sampletype == "V" | sampletype == "S" | 
           sampletype == "C" | sampletype == "P") %>%
  select(-x254:-x555) %>%
  mutate(month = month(date)) %>%
  #replace the pond letter code with an intelligible name
  mutate(pond = case_when(pond == "G" ~ "Graber",
                          pond == "O" ~ "Orchid North",
                          pond == "D" ~ "Orchid South",
                          pond == "L" ~ "Lakeview",
                          pond == "T" ~ "Tiedemans",
                          pond == "S" ~ "Strickers",
                          pond == "K" ~ "Kettle",
                          pond == "N" ~ "Owen West",
                          pond == "E" ~ "Elver",
                          pond == "W" ~ "Lakeview West",
                          pond == "F" ~ "Forebay",
                          TRUE ~ "McKee"))%>% 
  #Factor the pond name and place it in order - helps with color coding
  mutate(pond = factor(pond,levels = c("Forebay",
                                       "Strickers",
                                       "Tiedemans",
                                       "Lakeview West",
                                       "Orchid North",
                                       "Orchid South",
                                       "Owen West",
                                       "Lakeview",
                                       "Graber",
                                       "Kettle",
                                       "Elver",
                                       "McKee"))) 

cwmnTP = chem %>% 
  select(date, pond, tp_ugl) %>%
  filter(!is.na(tp_ugl)) %>%
  rename(tp = tp_ugl) %>%
  mutate(month = month(date),
         year = year(date)) %>%
  group_by(pond) %>%
  summarise(n = n(),
            meanTP = mean(tp, na.rm = TRUE),
            sdTP = sd(tp, na.rm = TRUE)) %>%
  ungroup() %>% 
  add_row(pond = "Graber") %>% 
  add_row(pond = "Orchid South") %>%
  mutate(pond = factor(pond,
                       levels = c( #Middleton Ponds
                         "Forebay","Strickers","Tiedemans","Lakeview West",
                         "Lakeview", "Graber",
                         "Orchid North", "Orchid South", 
                         "Owen West", "Kettle", "Elver", "McKee"))) 

#=======================================================================
# Average SRP - Comparison Among Dane County Ponds

ggplot(cwmnTP, aes(x = pond, y = meanTP, fill = pond)) + 
  # scale_y_log10() +
  geom_errorbar(aes(ymin = meanTP - 10,
                    ymax = meanTP + sdTP,
                    width = 0.2)) +
  geom_bar(stat = "identity") +
  geom_text(aes(x = pond, y = meanTP + sdTP + 25, label = n),
            color = "gray50", size = 3) +
  geom_hline(yintercept = 24, linetype = 'dashed', color = "gray30") +
  geom_hline(yintercept = 96, linetype = 'dotted', color = "gray30") +
  theme_bw() + xlab('') + ylab("Total P (ug/L)") +
  ggtitle('Total P Summary (all available data)') +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  scale_fill_manual(values = 
                      c("#238b45","#a1d99b",
                        "#2f6687","#92c5de",
                        "#73460d","#d19141",
                        "#7b5887", "#c99fd6",
                        "gray70", "gray70","gray70", "gray70"))


# TIME SERIES =================================================
# Make individual data frames for plotting highlighted time series
chemTP = chem %>% 
  select(date, pond, tp_ugl) %>%
  filter(!is.na(tp_ugl)) %>%
  rename(tp = tp_ugl) %>%
  mutate(month = month(date),
         year = year(date))

lakeview = chemTP %>% filter(pond == "Lakeview")
lakeviewWest = chemTP %>% filter(pond == "Lakeview West")
orchidNorth = chemTP %>% filter(pond == "Orchid North")
strickers = chemTP %>% filter(pond == "Strickers")
tiedemans = chemTP %>% filter(pond == "Tiedemans")
forebay = chemTP %>% filter(pond == "Forebay")


# CHLORIDE TIME SERIES -- 
# Lakeview Chloride Time Series
lakeviewPonds_timeSeries =
  ggplot(chemTP, aes(x = date, y = tp, by = pond)) +
  geom_point(size = 1.5, color = "gray80", shape = 18) +
  theme_bw() + scale_y_log10() +
  ylab("Total P (ug/L)") + ggtitle("Lakeview Ponds") +
  theme(axis.title.x = element_blank()) +
  geom_hline(yintercept = 24, linetype = "dashed", color = "gray30") +
  geom_hline(yintercept = 96, linetype = "dotdash", color = "gray30") +
  #Lakeview Park Constructed Pond
  geom_line(data = lakeview, aes(x = date, y = tp), size = 0.7, color = "#73460d") +
  geom_point(data = lakeview, aes(x = date, y = tp), size = 1.5, color = "#73460d") +
  #Lakeview Retention Pond in the western part of the park
  geom_line(data = lakeviewWest, aes(x = date, y = tp), size = 0.7, color = "#729aad") +
  geom_point(data = lakeviewWest, aes(x = date, y = tp), size = 1.5, color = "#729aad") +
  geom_point(aes(x = date('2022-07-01'), y = 2000), color = "#73460d", size = 2) +
  geom_point(aes(x = date('2022-07-01'), y = 1300), color = "#729aad", size = 2) +
  annotate(geom = 'text', x = date('2022-08-01'), y = 2000, 
           label = "Lakeview Park", hjust = 0, size = 3) +
  annotate(geom = 'text', x = date('2022-08-01'), y = 1300, 
           label = "Retention Pond", hjust = 0, size = 3) 


# Orchid N and S Chloride Time Series
orchidPonds_timeSeries =
  ggplot(chemTP, aes(x = date, y = tp, by = pond)) +
  geom_point(size = 1.5, color = "gray80", shape = 18) +
  theme_bw() + scale_y_log10() +
  ylab("Total P (mg/L)") + ggtitle("Orchid Park Ponds") +
  theme(axis.title.x = element_blank()) +
  geom_hline(yintercept = 24, linetype = "dashed", color = "gray50") +
  geom_hline(yintercept = 96, linetype = "dotdash", color = "gray30") +
  # Orchid North Time Series
  geom_line(data = orchidNorth, aes(x = date, y = tp), size = 0.7, color = "#7b5887") +
  geom_point(data = orchidNorth, aes(x = date, y = tp), size = 1.5, color = "#7b5887") +
  # Orchid South Time Series
  geom_point(aes(x = date('2022-07-01'), y = 2000), color = "#7b5887", size = 2) +
  geom_point(aes(x = date('2022-07-01'), y = 1300), color = "#c99fd6", size = 2) +
  annotate(geom = 'text', x = date('2022-08-01'), y = 2000, 
           label = "Orchid North", hjust = 0, size = 3) +
  annotate(geom = 'text', x = date('2022-08-01'), y = 1300, 
           label = "Orchid South", hjust = 0, size = 3) 

# Forebay and Strickers Time Series
forebayStrickers_timeSeries =
  ggplot(chemTP, aes(x = date, y = tp, by = pond)) +
  geom_point(size = 1.5, color = "gray80", shape = 18) +
  theme_bw() + scale_y_log10() +
  ylab("Total P (mg/L)") + ggtitle("Forebay & Stricker's Ponds") +
  theme(axis.title.x = element_blank()) +
  geom_hline(yintercept = 24, linetype = "dashed", color = "gray50") +
  geom_hline(yintercept = 96, linetype = "dotdash", color = "gray30") +
  # Forebay Time Series
  geom_line(data = forebay, aes(x = date, y = tp), size = 0.7, color = "#238b45") +
  geom_point(data = forebay, aes(x = date, y = tp), size = 1.5, color = "#238b45") +
  # Strickers Time Series
  geom_line(data = strickers, aes(x = date, y = tp), size = 0.7, color = "#8fc28a") + 
  geom_point(data = strickers, aes(x = date, y = tp), size = 1.5, color = "#8fc28a") +
  geom_point(aes(x = date('2022-07-01'), y = 17), color = "#238b45", size = 2) +
  geom_point(aes(x = date('2022-07-01'), y = 10), color = "#8fc28a", size = 2) +
  annotate(geom = 'text', x = date('2022-08-01'), y = 17, 
           label = "Stricker's Forebay", hjust = 0, size = 3) +
  annotate(geom = 'text', x = date('2022-08-01'), y = 10, 
           label = "Stricker's Pond", hjust = 0, size = 3) 


# Strickers and Tiedemans Time Series
strickersTiedemans_timeSeries =
  ggplot(chemTP, aes(x = date, y = tp, by = pond)) +
  geom_point(size = 1.5, color = "gray80", shape = 18) +
  theme_bw() + scale_y_log10() +
  ylab("Total P (mg/L)") + ggtitle("Stricker's & Tiedeman's Ponds") +
  theme(axis.title.x = element_blank()) +
  geom_hline(yintercept = 24, linetype = "dashed", color = "gray50") +
  geom_hline(yintercept = 96, linetype = "dotdash", color = "gray30") +
  # Strickers Time Series
  geom_line(data = strickers, aes(x = date, y = tp), size = 0.7, color = "#8fc28a") + 
  geom_point(data = strickers, aes(x = date, y = tp), size = 1.5, color = "#8fc28a") +
  # Tiedemans Time Series
  geom_line(data = tiedemans, aes(x = date, y = tp), size = 0.7, color = "#2f6687") +
  geom_point(data = tiedemans, aes(x = date, y = tp), size = 1.5, color = "#2f6687") +
  # Legend
  geom_point(aes(x = date('2022-07-01'), y = 17), color = "#2f6687", size = 2) +
  geom_point(aes(x = date('2022-07-01'), y = 10), color = "#8fc28a", size = 2) +
  annotate(geom = 'text', x = date('2022-08-01'), y = 17, 
           label = "Tiedeman's Pond", hjust = 0, size = 3) +
  annotate(geom = 'text', x = date('2022-08-01'), y = 10, 
           label = "Stricker's Pond", hjust = 0, size = 3) 

# Compile all the panels
ggarrange(lakeviewPonds_timeSeries, orchidPonds_timeSeries,
          forebayStrickers_timeSeries, strickersTiedemans_timeSeries,
          nrow = 2, ncol = 2)



## ===================================
# Fraction of TP that is SRP

fracTP = chem %>% 
  select(date, pond, tp_ugl, srp_ugl) %>%
  filter(!is.na(tp_ugl)| !is.na(srp_ugl)) %>%
  rename(tp = tp_ugl,
         srp = srp_ugl) %>%
  mutate(fracTP = (srp/tp)*100) %>%
  group_by(pond) %>%
  summarise(n = n(),
            meanFracTP = mean(fracTP, na.rm = TRUE),
            sdFracTP = sd(fracTP, na.rm = TRUE)) %>%
  ungroup() %>% 
  add_row(pond = "Graber") %>% 
  mutate(pond = factor(pond,
                       levels = c( #Middleton Ponds
                         "Forebay","Strickers","Tiedemans","Lakeview West",
                         "Lakeview", "Graber",
                         "Orchid North", "Orchid South", 
                         "Owen West", "Kettle", "Elver", "McKee"))) 

#=======================================================================
# Average SRP - Comparison Among Dane County Ponds

ggplot(fracTP, aes(x = pond, y = meanFracTP, fill = pond)) + 
  geom_errorbar(aes(ymin = meanFracTP - 1,
                    ymax = meanFracTP + sdFracTP,
                    width = 0.2)) +
  geom_bar(stat = "identity") +
  geom_text(aes(x = pond, y = meanFracTP + sdFracTP + 3, label = n),
            color = "gray50", size = 3.5) +
  theme_bw() + xlab('') + ylab("Percentage of Total P from SRP (%)") +
  ggtitle('% of Total P that is Soluble Reactive P') +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  scale_fill_manual(values = 
                      c("#238b45","#a1d99b",
                        "#2f6687","#92c5de",
                        "#73460d","#d19141",
                        "#7b5887", "#c99fd6",
                        "gray70", "gray70","gray70", "gray70"))

