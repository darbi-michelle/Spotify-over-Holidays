#Load Packages

library(rvest) 
library(tidyverse) 
library(lubridate) 
library(readxl)
library(extrafont)

#Web pages to be scraped (without dates)

url_global <- "https://spotifycharts.com/regional/global/daily/"
url_uk <-"https://spotifycharts.com/regional/gb/daily/"

#Dates to be added to web pages

threeya_values <- seq(as.Date("2017/12/24"), as.Date("2017/12/26"), by = "day")
twoya_values <- seq(as.Date("2018/12/24"), as.Date("2018/12/26"), by = "day")
lastyear_values <- seq(as.Date("2019/12/24"), as.Date("2019/12/26"), by = "day")
thisyear_values <- seq(as.Date("2020/12/24"), as.Date("2020/12/26"), by = "day")

holiday_values <- c(threeya_values, twoya_values, lastyear_values, thisyear_values)
holiday_values

#Adding dates to urls

new.url <- function(url, x){
  full_url <- paste0(url, x)
  full_url}

holiday_uk_url <- new.url(url_uk, holiday_values)
holiday_global_url <- new.url(url_global, holiday_values)

#Scraping web pages

SpotifyScrape <- function(x){
  page <- x
  rank <- page %>%
    read_html() %>% 
    html_nodes('.chart-table-position') %>% 
    html_text() %>% 
    as.data.frame()
  track <- page %>% 
    read_html() %>% 
    html_nodes('strong') %>% 
    html_text() %>% 
    as.data.frame()
  artist <- page %>% 
    read_html() %>% 
    html_nodes('.chart-table-track span') %>% 
    html_text() %>% 
    as.data.frame()
  streams <- page %>% 
    read_html() %>% 
    html_nodes('td.chart-table-streams') %>% 
    html_text() %>% 
    as.data.frame()
  dates <- page %>% 
    read_html() %>% 
    html_nodes('.responsive-select~ .responsive-select+ .responsive-select .responsive-select-value') %>%
    html_text() %>% 
    as.data.frame()
  
  #Combining scraped data into a tibble
  
  chart <- cbind(rank, track, artist, streams, dates) 
  names(chart) <- c("Rank", "Track", "Artist", "Streams", "Date") 
  chart <- as_tibble(chart) 
  return(chart) 
}

global_data_raw <- map_df(holiday_global_url, SpotifyScrape) 
saveRDS(global_data_raw, "globaldataRAW.rds")


global_data_raw <- readRDS("globaldataRAW.rds")
dim(global_data_raw) 
head(global_data_raw)

#Clean the data

global_data_clean <- global_data_raw %>% 
  mutate( Artist = gsub("by ", "", Artist), 
          Streams = gsub(",", "", Streams), 
          Streams = as.numeric(Streams),
          Rank = as.numeric(Rank),
          Date = as.factor(Date),
          Date = strptime(Date, format="%m/%d/%Y"),
          Date = as.Date(Date,format="%Y/%m/%d"), 
          Day = paste0(day(Date),"-",  month(Date)),
          Day = as.factor(Day),
          Year = year(Date),
          Year = as.factor(Year))
head(global_data_clean)
summary(global_data_clean)

#Visualising the data
monthly_users <- read_xlsx("spotify_mau.xlsx", sheet = 2, range = "B13:C28", 
                           col_names = c("Quarter", "MAU"), 
                           col_types = c("text", "numeric")) %>%
  separate(Quarter, c("Quarter", "Year"), sep = " ") %>%
  mutate(Quarter = as.factor(Quarter), 
         Year = as.factor(Year)) %>%
  filter(Year != "2016") %>%
 filter(Year == 2020 & Quarter == "Q3" | Year != 2020 & Quarter == "Q4")

monthly_users

global_normalised <- global_data_clean %>%
  mutate(Stream_by_user = Streams/(monthly_users$MAU[match(global_data_clean$Year,monthly_users$Year)]))
global_normalised

#Prepare the top 10 data

top_10_xmas_eve_global_n <- global_normalised %>%
  filter(Day == "24-12") %>%
  arrange(Year, desc(Streams) ) %>%
  group_by(Track, Year) %>%
  filter(Rank %in% c(1:10)) %>%
  mutate(Track = gsub(" -.*", "", Track),
         Track = gsub("\\[.*", "", Track),
         Track = gsub("\\(.*", "", Track)) %>%
  arrange(Rank)
top_10_xmas_eve_global_n

top_10_xmas_global_n <- global_normalised %>%
  filter(Day == "25-12") %>%
  arrange(Year, desc(Streams) ) %>%
  group_by(Track, Year) %>%
  filter(Rank %in% c(1:10)) %>%
  mutate(Track = gsub(" -.*", "", Track),
         Track = gsub("\\[.*", "", Track),
         Track = gsub("\\(.*", "", Track)) %>%
  arrange(Rank)
top_10_xmas_global_n

top_10_boxing_day_global_n <- global_normalised %>%
  filter(Day == "26-12") %>%
  arrange(Year, desc(Streams) ) %>%
  group_by(Track, Year) %>%
  filter(Rank %in% c(1:5)) %>%
  mutate(Track = gsub(" -.*", "", Track),
         Track = gsub("\\[.*", "", Track),
         Track = gsub("\\(.*", "", Track)) %>%
  arrange(Rank)
top_10_boxing_day_global_n


#Visualise the Christmas Eve data

christmas_eve <- top_10_xmas_eve_global_n %>%
  mutate(Stream_by_user = round(Stream_by_user, digits = 0)) %>%
  mutate(Stream_by_users = prettyNum(Stream_by_user, big.mark = ",", preserve.width="none")) %>%
  ggplot(aes(reorder(Track, -Rank), y = Stream_by_user, fill = as.factor(Rank))) +
  geom_bar(stat = "identity", show.legend = FALSE)  +
  coord_flip() +
  theme_classic() +
  theme(axis.text.y  = element_text(size = 4, colour = "#b3b3b3", family = "Gotham"),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 6, colour = "#b3b3b3"),
        axis.title.y = element_blank(),
        axis.line.y =  element_line(size = 0.3, colour = "#212121"),
        axis.line.x = element_blank(),
        axis.ticks.y = element_line(size = 0.2),
        axis.ticks.x = element_blank(),
        plot.caption = element_text(size= 5, face = "italic", hjust = 1, colour = "#1db954", family = "Gotham"),
        plot.title = element_text(size = 11, colour = "#1db954", face = "bold", family = "Gotham"),
        plot.background = element_rect(fill = "#191414", colour = "#191414"),
        plot.subtitle = element_text(size = 8, colour = "#b3b3b3", family = "Gotham"),
        panel.background = element_rect(fill = "#191414"),
        strip.background = element_rect(size = 1, fill = "#191414", colour = "#191414"),
        strip.text = element_text(size = 8, margin = margin(.1, 0, .1, 0, "cm"), colour = "#b3b3b3")) +  facet_grid(cols = vars(Year)) +
  geom_text(aes(y=Stream_by_user, label = Stream_by_users, hjust = 1.2), 
            size = 1.5, colour = "white") +
  labs( y = "Streams per one million users", title = "Top 10 Songs Streamed on Christmas Eve", 
       subtitle = "From 2017 to 2020*", 
       caption = "Source: Spotify, Spotify Charts \n * Q3 data was used for 2020 as Q4 data is unavailable.")
christmas_eve 
ggsave(plot = christmas_eve, "Christmas_eve_top_10.png", height = 5.8, width = 11.3, units= "cm")

#Visualise the Christmas Day data

christmas <- top_10_xmas_global_n %>%
  mutate(Stream_by_user = round(Stream_by_user, digits = 0)) %>%
  mutate(Stream_by_users = prettyNum(Stream_by_user, big.mark = ",", preserve.width="none")) %>%
  ggplot(aes(reorder(Track, -Rank), y = Stream_by_user, fill = as.factor(Rank))) +
  geom_bar(stat = "identity", show.legend = FALSE)  +
  coord_flip() +
  theme_classic() +
  theme(axis.text.y  = element_text(size = 4, colour = "#b3b3b3", family = "Gotham"),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 6, colour = "#b3b3b3"),
        axis.title.y = element_blank(),
        axis.line.y =  element_line(size = 0.3, colour = "#212121"),
        axis.line.x = element_blank(),
        axis.ticks.y = element_line(size = 0.2),
        axis.ticks.x = element_blank(),
        plot.caption = element_text(size= 5, face = "italic", hjust = 1, colour = "#1db954", family = "Gotham"),
        plot.title = element_text(size = 10, colour = "#1db954", face = "bold", family = "Gotham"),
        plot.background = element_rect(fill = "#191414", colour = "#191414"),
        plot.subtitle = element_text(size = 8, colour = "#b3b3b3", family = "Gotham"),
        panel.background = element_rect(fill = "#191414"),
        strip.background = element_rect(size = 1, fill = "#191414", colour = "#191414"),
        strip.text = element_text(size = 8, margin = margin(.1, 0, .1, 0, "cm"), colour = "#b3b3b3")) +  facet_grid(cols = vars(Year)) +
  geom_text(aes(y=Stream_by_user, label = Stream_by_users, hjust = 1.4), 
            size = 1.5, colour = "white") +
  labs( y = "Streams per one million users", title = "Top 10 Songs Streamed on Christmas Day", 
       subtitle = "From 2017 to 2020*", 
       caption = "Source: Spotify, Spotify Charts \n * Q3 data was used for 2020 as Q4 data is unavailable.")
christmas
ggsave(plot = christmas, "Christmas_top_10.png", height = 5.8, width = 11.3, units= "cm")

#Visualise the Boxing Day data

boxing_day <- top_10_boxing_day_global_n %>%
  mutate(Stream_by_user = round(Stream_by_user, digits = 0)) %>%
  mutate(Stream_by_users = prettyNum(Stream_by_user, big.mark = ",", preserve.width="none")) %>%
  ggplot(aes(reorder(Track, -Rank), y = Stream_by_user, fill = as.factor(Rank))) +
  geom_bar(stat = "identity", show.legend = FALSE, width = 1.5)  +
  coord_flip() +
  theme_classic() +
  theme(axis.text.y  = element_text(size = 4, colour = "#b3b3b3", family = "Gotham"),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 6, colour = "#b3b3b3"),
        axis.title.y = element_blank(),
        axis.line.y =  element_line(size = 0.3, colour = "#212121"),
        axis.line.x = element_blank(),
        axis.ticks.y = element_line(size = 0.2),
        axis.ticks.x = element_blank(),
        plot.caption = element_text(size= 5, face = "italic", hjust = 1, colour = "#1db954", family = "Gotham"),
        plot.title = element_text(size = 11, colour = "#1db954", face = "bold", family = "Gotham"),
        plot.background = element_rect(fill = "#191414", colour = "#191414"),
        plot.subtitle = element_text(size = 8, colour = "#b3b3b3", family = "Gotham"),
        panel.background = element_rect(fill = "#191414"),
        strip.background = element_rect(size = 1, fill = "#191414", colour = "#191414"),
        strip.text = element_text(size = 8, margin = margin(.1, 0, .1, 0, "cm"), colour = "#b3b3b3")) +  facet_grid(cols = vars(Year)) +
  geom_text(aes(y=Stream_by_user, label = Stream_by_users, hjust = 1.5), 
            size = 1.5, colour = "white") +
  labs( y = "Streams per one million users", title = "Top 5 Songs Streamed on Boxing Day", 
       subtitle = "From 2017 to 2020*", 
       caption = "Source: Spotify, Spotify Charts \n * Q3 data was used for 2020 as Q4 data is unavailable.")
boxing_day
ggsave(plot = boxing_day, "Boxing_day_top_10.png", height = 5.8, width = 11.3, units= "cm")
