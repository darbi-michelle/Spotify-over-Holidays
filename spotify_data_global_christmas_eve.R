#Load Packages

library(rvest) 
library(tidyverse) 
library(lubridate) 
library(readxl)
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
  
  chart <- cbind(rank, track, artist, streams, dates) # Combine R Objects by Columns
  names(chart) <- c("Rank", "Track", "Artist", "Streams", "Date") # Functions to get or set the names of an object
  chart <- as_tibble(chart) #turns an existing object into a tibble (Tibble package)
  return(chart) # Final tibble 5 columns & (200 rows * 12 days) = 2400
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

top_10_xmas_eve_global <- global_data_clean %>%
  filter(Day == "24-12") %>%
  arrange(Year, desc(Streams) ) %>%
  group_by(Track, Year) %>%
  filter(Rank %in% c(1:10)) %>%
  mutate(Track = gsub(" -.*", "", Track)) %>%
  arrange(Rank)
top_10_xmas_eve_global

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

top_10_xmas_eve_global_n <- global_normalised %>%
  filter(Day == "24-12") %>%
  arrange(Year, desc(Streams) ) %>%
  group_by(Track, Year) %>%
  filter(Rank %in% c(1:10)) %>%
  mutate(Track = gsub(" -.*", "", Track)) %>%
  arrange(Rank)
top_10_xmas_eve_global_n

top_10_xmas_eve_global_n %>%
  mutate(Year = fct_reorder(Year, Stream_by_user)) %>%
  ggplot(aes(reorder(Track, Streams), y = Stream_by_user, fill = as.factor(Rank))) +
  geom_bar(stat = "identity")  +
  coord_flip() +
  labs(x = "Track Name", y = "Streams per one million users", title = "Most Streamed Songs on Christmas Eve", 
       subtitle = "From 2017 to 2020*", 
       caption = "Source: Spotify Charts \n * Q3 data was used for 2020 as Q4 data is unavailable.") +
  scale_fill_discrete(name = "Rank") +
  theme_classic() +
  scale_y_continuous(expand = c(0,0)) +
  facet_grid(cols = vars(Year))
  