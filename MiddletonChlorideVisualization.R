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

# POND MONITORING BY ADRIANNA GORSKY ======================
# Package ID: knb-lter-ntl.433.1 Cataloging System:https://pasta.edirepository.org.
# Data set title: Greenhouse gas and water chemistry data from urban ponds in Madison, Wisconsin during the summer and under-ice period of 2021-2022.
# Data set creators:  Adrianna Gorsky, Emily Stanley, Hilary Dugan - University of Wisconsin-Madison 
# Metadata Provider:  NTL Information Manager - University of Wisconsin-Madison 
# Contact:  Adrianna Gorsky - Graduate Student University of Wisconsin-Madison  - agorsky@wisc.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/433/1/79746d457c961dc974af49843e51dffa" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")

gorskyPonds <-read_csv(infile1) %>%
  filter(!is.na(cl_mgL)) %>%
  select(-pond, -avsnow_cm:-whiteice_cm, -openwater_perc:-algae_type) %>%
  rename(pond = name)
unlink(infile1)


# ***********Cite Data as: 
# Gorsky, A.L., E.H. Stanley, and H.A. Dugan. 2024. 
# Greenhouse gas and water chemistry data from urban ponds in Madison, Wisconsin during the summer and under-ice period of 2021-2022 ver 1. 
# Environmental Data Initiative. https://doi.org/10.6073/pasta/ee8b4cb3e5bd8669be0a215ccee466f6


# Combine CWMN data with Gorsky data
gorskyChloride = gorskyPonds %>% 
  select(sampledate, pond, cl_mgL) %>%
  rename(date = sampledate, 
         chloride_mgl = cl_mgL) %>%
  mutate(pond = case_when(pond == "Elver Pond" ~ "Elver",
                          pond == "Kettle Pond" ~ "Kettle",
                          pond == "Owen Park Pond East" ~ "Owen East",
                          pond == "Owen Park Pond West" ~ "Owen West",
                          pond == "Midtown Pond" ~ "Midtown",
                          pond == "Door Creek Stormwater 1" ~ "Door Creek 1",
                          pond == "Door Creek Stormwater 2" ~ "Door Creek 2",
                          pond == "Door Creek Stormwater 3" ~ "Door Creek 3",
                          pond == "Alliant Center Pond" ~ "Alliant Center",
                          pond == "Commercial Ave. Pond" ~ "Commercial Ave",
                          pond == "Dobson A4 Pond" ~ "Door Creek Church",
                          pond == "Dobson A5 Pond" ~ "Buckeye Road",
                          pond == "Dobson A7 Pond" ~ "West Towne",
                          pond == "Dobson A8 Pond" ~ "Waldorf Blvd",
                          pond == "Lower Mannitou" ~ "Lower Manitou",
                          TRUE ~ pond))

cwmnChloride = chem %>% 
  select(date, pond, chloride_mgl) %>%
  filter(!is.na(chloride_mgl))

combo = bind_rows(gorskyChloride, cwmnChloride) %>%
  mutate(month = month(date),
         year = year(date)) 

#=======================================================================
# Average Chloride by Season - Comparison Among Dane County Ponds

# Subset the data to winter months, calculate means
winter = combo %>%
  mutate(month = month(date)) %>%
  filter(month <=4 | month >= 11) %>%
  group_by(pond) %>%
  summarise(n = n(),
            meanCl = mean(chloride_mgl, na.rm = TRUE),
            sdCl = sd(chloride_mgl, na.rm = TRUE)) %>%
  ungroup() %>% 
  add_row(pond = "Graber") %>%
  mutate(pond = factor(pond,
                       levels = c( #Middleton Ponds
                         "Forebay","Strickers","Tiedemans","Lakeview West",
                         "Lakeview", "Graber",
                         "Orchid North", "Orchid South",
                         #Madison Ponds - CWMN and Gorsky Survey
                         "Alliant Center","Hospital Pond","West Towne","Elver",
                         "Lower Manitou", "Upper Manitou",
                         "Lot 60 Pond", "McKee", "Commercial Ave",
                         "Door Creek 1", "Door Creek 2", "Door Creek 3", "Door Creek Church",
                         "Kettle","Owen West","Owen East",
                         "Buckeye Road", "Waldorf Blvd", "Midtown")))
  
# WINTER Chloride Bar Graph
winter_chloride_barplot =
  ggplot(winter, aes(x = pond, y = meanCl, fill = pond)) + 
  geom_hline(yintercept = 230, linetype = 'dashed', color = 'gray50') +
  geom_errorbar(aes(ymin = meanCl - 3,
                    ymax = meanCl + sdCl,
                    width = 0.2)) +
  geom_bar(stat = "identity") +
  geom_text(aes(x = pond, y = meanCl + sdCl + 30, label = n),
            color = "gray50", size = 3) +
  theme_bw() + xlab('') + ylab("Chloride (mg/L)") +
  ggtitle('Winter Chloride Summary (November - April)') +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  ylim(0, 500) +
  scale_fill_manual(values = 
                      c("#238b45","#a1d99b",
                        "#2f6687","#92c5de",
                        "#73460d","#d19141",
                        "#7b5887", "#c99fd6",
                        "gray70", "gray70","gray70", "gray70","gray70", "gray70",
                        "gray70", "gray70","gray70", "gray70","gray70", "gray70",
                        "gray70", "gray70","gray70", "gray70","gray70", "gray70",
                        "gray70"))

#Subset the open water data, calcualte mean
summer = combo %>%
  mutate(month = month(date)) %>%
  filter(month >= 5) %>% filter(month <= 10) %>%
  group_by(pond) %>%
  summarise(n = n(),
            meanCl = mean(chloride_mgl, na.rm = TRUE),
            sdCl = sd(chloride_mgl, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(pond = factor(pond,
                       levels = c( #Middleton Ponds
                         "Forebay","Strickers","Tiedemans","Lakeview West",
                         "Lakeview", "Graber",
                         "Orchid North", "Orchid South",
                         #Madison Ponds - CWMN and Gorsky Survey
                         "Alliant Center","Hospital Pond","West Towne","Elver",
                         "Lower Manitou", "Upper Manitou",
                         "Lot 60 Pond", "McKee", "Commercial Ave",
                         "Door Creek 1", "Door Creek 2", "Door Creek 3", "Door Creek Church",
                         "Kettle","Owen West","Owen East",
                         "Buckeye Road", "Waldorf Blvd", "Midtown")))

# SUMMER Chloride Bar Graph
openwater_chloride_barplot =
  ggplot(summer, aes(x = pond, y = meanCl, fill = pond)) + 
  geom_hline(yintercept = 230, linetype = 'dashed', color = 'gray50') +
  geom_errorbar(aes(ymin = meanCl - 5,
                    ymax = meanCl + sdCl,
                    width = 0.2)) +
  geom_bar(stat = "identity") +
  geom_text(aes(x = pond, y = meanCl + sdCl + 30, label = n),
            color = "gray50", size = 3) +
  theme_bw() + xlab('') + ylab("Chloride (mg/L)") +
  ggtitle('Open Water Chloride Summary (May - October)') +
  theme(legend.position = 'none',
        # axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)
        axis.text.x = element_blank()
        ) + 
  ylim(0, 500) +
  scale_fill_manual(values = 
                      c("#238b45","#a1d99b",
                        "#2f6687","#92c5de",
                        "#73460d","#d19141",
                        "#7b5887", "#c99fd6",
                        "gray70", "gray70","gray70", "gray70","gray70", "gray70",
                        "gray70", "gray70","gray70", "gray70","gray70", "gray70",
                        "gray70", "gray70","gray70", "gray70","gray70", "gray70",
                        "gray70"))


ggdraw() +
  draw_plot(openwater_chloride_barplot, 
            x = 0, y = 0.5, 
            width = 1, height = 0.35) +
  draw_plot(winter_chloride_barplot, 
            x = 0, y = 0, 
            width = 1, height = 0.5)

# TIME SERIES =================================================
# Make individual data frames for plotting highlighted time series
lakeview = combo %>% filter(pond == "Lakeview")
lakeviewWest = combo %>% filter(pond == "Lakeview West")
orchidNorth = combo %>% filter(pond == "Orchid North")
orchidSouth = combo %>% filter(pond == "Orchid South") %>%
  add_row(pond = "Orchid South", date = date("2023-01-01"))
strickers = combo %>% filter(pond == "Strickers")
tiedemans = combo %>% filter(pond == "Tiedemans")
forebay = combo %>% filter(pond == "Forebay")


# CHLORIDE TIME SERIES -- 
# Lakeview Chloride Time Series
lakeviewPonds_timeSeries =
  ggplot(combo, aes(x = date, y = chloride_mgl, by = pond)) +
  geom_point(size = 1.5, color = "gray80", shape = 18) +
  theme_bw() + ylim(0,600) +
  ylab("Chloride (mg/L)") + ggtitle("Lakeview Ponds") +
  geom_hline(yintercept = 230, linetype = "dashed") +
  theme(axis.title.x = element_blank()) +
  #Lakeview Park Constructed Pond
  geom_line(data = lakeview, aes(x = date, y = chloride_mgl), size = 0.7, color = "#73460d") +
  geom_point(data = lakeview, aes(x = date, y = chloride_mgl), size = 1.5, color = "#73460d") +
  #Lakeview Retention Pond in the western part of the park
  geom_line(data = lakeviewWest, aes(x = date, y = chloride_mgl), size = 0.7, color = "#729aad") +
  geom_point(data = lakeviewWest, aes(x = date, y = chloride_mgl), size = 1.5, color = "#729aad") +
  geom_point(aes(x = date('2021-09-01'), y = 570), color = "#73460d", size = 2) +
  geom_point(aes(x = date('2021-09-01'), y = 530), color = "#729aad", size = 2) +
  annotate(geom = 'text', x = date('2021-10-01'), y = 570, 
           label = "Lakeview Park", hjust = 0, size = 3) +
  annotate(geom = 'text', x = date('2021-10-01'), y = 530, 
           label = "Retention Pond", hjust = 0, size = 3) 


# Orchid N and S Chloride Time Series
orchidPonds_timeSeries = 
  ggplot(combo, aes(x = date, y = chloride_mgl, by = pond)) +
  geom_point(size = 1.5, color = "gray80", shape = 18) +
  theme_bw() + ylim(0,600) +
  ylab("Chloride (mg/L)") + ggtitle("Orchid Park Ponds") +
  geom_hline(yintercept = 230, linetype = "dashed") +
  theme(axis.title.x = element_blank()) +
  # Orchid North Time Series
  geom_line(data = orchidNorth, aes(x = date, y = chloride_mgl), size = 0.7, color = "#7b5887") +
  geom_point(data = orchidNorth, aes(x = date, y = chloride_mgl), size = 1.5, color = "#7b5887") +
  # Orchid South Time Series
  geom_line(data = orchidSouth, aes(x = date, y = chloride_mgl), size = 0.7, color = "#c99fd6") + 
  geom_point(data = orchidSouth, aes(x = date, y = chloride_mgl), size = 1.5, color = "#c99fd6") +
  geom_point(aes(x = date('2021-09-01'), y = 570), color = "#7b5887", size = 2) +
  geom_point(aes(x = date('2021-09-01'), y = 530), color = "#c99fd6", size = 2) +
  annotate(geom = 'text', x = date('2021-10-01'), y = 570, 
           label = "Orchid North", hjust = 0, size = 3) +
  annotate(geom = 'text', x = date('2021-10-01'), y = 530, 
           label = "Orchid South", hjust = 0, size = 3) 

# Forebay and Strickers Time Series
forebayStrickers_timeSeries = 
  ggplot(combo, aes(x = date, y = chloride_mgl, by = pond)) +
  geom_point(size = 1.5, color = "gray80", shape = 18) +
  theme_bw() + ylim(0,600) +
  ylab("Chloride (mg/L)") + ggtitle("Forebay & Stricker's Ponds") +
  geom_hline(yintercept = 230, linetype = "dashed") +
  theme(axis.title.x = element_blank()) +
  # Forebay Time Series
  geom_line(data = forebay, aes(x = date, y = chloride_mgl), size = 0.7, color = "#238b45") +
  geom_point(data = forebay, aes(x = date, y = chloride_mgl), size = 1.5, color = "#238b45") +
  # Strickers Time Series
  geom_line(data = strickers, aes(x = date, y = chloride_mgl), size = 0.7, color = "#8fc28a") + 
  geom_point(data = strickers, aes(x = date, y = chloride_mgl), size = 1.5, color = "#8fc28a") +
  geom_point(aes(x = date('2021-09-01'), y = 570), color = "#238b45", size = 2) +
  geom_point(aes(x = date('2021-09-01'), y = 530), color = "#8fc28a", size = 2) +
  annotate(geom = 'text', x = date('2021-10-01'), y = 570, 
           label = "Stricker's Forebay", hjust = 0, size = 3) +
  annotate(geom = 'text', x = date('2021-10-01'), y = 530, 
           label = "Stricker's Pond", hjust = 0, size = 3) 


# Strickers and Tiedemans Time Series
strickersTiedemans_timeSeries = 
  ggplot(combo, aes(x = date, y = chloride_mgl, by = pond)) +
  geom_point(size = 1.5, color = "gray80", shape = 18) +
  theme_bw() + ylim(0,600) +
  ylab("Chloride (mg/L)") + ggtitle("Stricker's & Tiedeman's Ponds") +
  geom_hline(yintercept = 230, linetype = "dashed") +
  theme(axis.title.x = element_blank()) +
  # Tiedemans Time Series
  geom_line(data = tiedemans, aes(x = date, y = chloride_mgl), size = 0.7, color = "#2f6687") +
  geom_point(data = tiedemans, aes(x = date, y = chloride_mgl), size = 1.5, color = "#2f6687") +
  # Strickers Time Series
  geom_line(data = strickers, aes(x = date, y = chloride_mgl), size = 0.7, color = "#8fc28a") + 
  geom_point(data = strickers, aes(x = date, y = chloride_mgl), size = 1.5, color = "#8fc28a") +
  geom_point(aes(x = date('2021-09-01'), y = 570), color = "#2f6687", size = 2) +
  geom_point(aes(x = date('2021-09-01'), y = 530), color = "#8fc28a", size = 2) +
  annotate(geom = 'text', x = date('2021-10-01'), y = 570, 
           label = "Tiedeman's Pond", hjust = 0, size = 3) +
  annotate(geom = 'text', x = date('2021-10-01'), y = 530, 
           label = "Stricker's Pond", hjust = 0, size = 3) 

# Compile all the panels
ggarrange(lakeviewPonds_timeSeries, orchidPonds_timeSeries,
          forebayStrickers_timeSeries, strickersTiedemans_timeSeries,
          nrow = 2, ncol = 2)

