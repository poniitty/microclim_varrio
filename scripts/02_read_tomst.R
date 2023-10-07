library(tidyverse)
library(lubridate)
library(data.table)
library(cowplot)
library(zoo)

# Set date limits to remove implausible dates
mind <- as.Date("2018-06-01", tz = "Etc/GMT-2")
maxd <- as.Date("2022-10-10", tz = "Etc/GMT-2")

raw_data_dir <- "/scratch/project_2007415/microclim/Varrio2022"
# raw_data_dir <- "C:/Users/OMISTAJA/OneDrive - University of Helsinki/Kesä2022/Varrio2022"


# List logger data files to read
f <- list.files(raw_data_dir, pattern = "data_", full.names = T, recursive = T)

fi <- data.frame(file = f)

fi$file2 <- gsub("_..csv", "", fi$file)

fi$site <- as.numeric(parse_number(unlist(lapply(fi$file, function(x) rev(strsplit(x, "/")[[1]])[2]))))

fi <- fi[order(fi$site),]

fi$tomst_id <- unlist(lapply(fi$file, function(x) as.numeric(strsplit(gsub("data_","",rev(strsplit(x, "/")[[1]])[1]), "_")[[1]][1])))

readdata <- function(i){
  nn <- sum(grepl(i, fi$file2))
  
  if(nn > 1){
    
    fi2 <- fi %>% filter(grepl(i, fi$file2))
    
    df2 <- data.frame()
    for(ii in fi2$file2){
      print(ii)
      d <- fread(ii)
      
      d %>% select(V2,V3,V4,V5,V6,V7) -> d
      
      d %>% filter(!duplicated(.$V2, fromLast = T)) -> d
      
      df2 <- bind_rows(df2, d)
    }
    
    df2 %>% filter(!duplicated(.$V2, fromLast = T)) -> df2
    
    df2$site <- fi[which(fi$file2 == ii),"id"]
    d$tomst_id <- fi[which(fi$file2 == i),"tomst_id"]
    
    df2 %>% mutate(across(V4:V6, ~as.numeric(gsub(",",".\\",.)))) -> df2
    
    df2 %>% mutate(V2 = ymd_hm(V2, tz = "UTC")) %>% 
      mutate(V2 = with_tz(V2, tzone = "Etc/GMT-2")) -> df2
    
    
    return(df2)
    
  } else {
    
    print(i)
    d <- fread(fi$file[fi$file2 == i])
    
    d %>% select(V2,V3,V4,V5,V6,V7) -> d
    
    d %>% filter(!duplicated(.$V2, fromLast = T)) -> d
    
    d %>% mutate(across(V4:V6, ~as.numeric(gsub(",",".\\",.)))) -> d
    
    d$site <- fi[which(fi$file2 == i),"site"]
    d$tomst_id <- fi[which(fi$file2 == i),"tomst_id"]
    
    d %>% mutate(V2 = ymd_hm(V2, tz = "UTC")) %>% 
      mutate(V2 = with_tz(V2, tzone = "Etc/GMT-2")) -> d
    
    return(d)
    
  }
  
}

mylist <- lapply(fi$file2, readdata)
df <- rbindlist( mylist )

# Rename columns
df %>% rename(datetime = V2,
              zone = V3,
              T1 = V4,
              T2 = V5,
              T3 = V6,
              moist = V7) -> df

df %>% arrange(site, datetime) -> df

df %>% group_by(site, tomst_id) %>% 
  summarise(maxdt = max(datetime)) -> maxdt
# maxdt <- full_join(maxdt, fi %>% select(site, tomst_id) %>% filter(!duplicated(.)))
fwrite(maxdt, "data/reading_times_2022.csv")
maxdt %>% arrange(maxdt)
maxdt %>% arrange(desc(maxdt))

# Remove implausible dates
df %>% filter(datetime > mind,
              datetime < maxd) -> df

sites <- unique(df$site)

# Calculate different daily values for diagnostics
df %>% mutate(date = as_date(datetime)) %>% 
  group_by(site,date,tomst_id) %>% 
  summarise(soil_mean = mean(T1),
            air_mean = mean(T3)) %>% 
  as.data.frame() -> df2

# create column for error codes
df2 %>% mutate(probl = 0) -> df2

############################################################################
# PLOTTINGS
############################################################################

# Months to plot
times <- seq(floor_date(as_date(min(df2$date)), "month"),
             ceiling_date(as_date(max(df2$date)), "month") + months(1) - days(1),
             by = "month")

# Plot each site month by month
for(siteid in sites){
  # siteid <- "SAA1195"
  print(siteid)
  pdf(paste0("visuals/monthly_", siteid, ".pdf"), 10, 6)
  temp <- df %>% filter(site == siteid)
  
  if(length(na.omit(unique(temp$tomst_id))) > 1){
    
    for(ii in na.omit(unique(temp$tomst_id))){
      
      for (tt in 1:(length(times) - 1)) {
        temp %>% filter(tomst_id == ii) %>%
          filter(datetime >= ymd(times[tt]),
                 datetime < ymd(times[tt + 1])) -> dft
        
        if(nrow(dft %>% filter(complete.cases(.)) > 0)){
          dft %>%
            ggplot(aes_string(x = "datetime")) +
            geom_line(aes_string(y = "T3"), col = "cornflowerblue") +
            geom_line(aes_string(y = "T2"), col = "brown1") +
            geom_line(aes_string(y = "T1"), col = "darkgoldenrod") +
            theme_minimal() +
            ylab("Temperature") + xlab("Date") +
            ggtitle(paste("Site: ", siteid, "; Tomst: ", ii, "; Time: ", times[tt])) +
            scale_x_datetime(date_minor_breaks = "1 day") -> GG1
          
          dft %>%
            ggplot(aes_string(x = "datetime")) +
            geom_line(aes_string(y = "moist"), col = "blue") +
            theme_minimal() +
            ylab("Moisture") + xlab("Date") +
            ggtitle(paste("Site: ", siteid, "; Time: ", times[tt])) +
            scale_x_datetime(date_minor_breaks = "1 day") -> GG2
          
          print(plot_grid(plotlist = list(GG1, GG2), nrow = 2))
        }
      }
    }
  } else {
    for (tt in 1:(length(times) - 1)) {
      
      temp %>% 
        filter(datetime >= ymd(times[tt]),
               datetime < ymd(times[tt + 1])) -> dft
      
      if(nrow(dft %>% filter(complete.cases(.)) > 0)){
        dft %>%
          ggplot(aes_string(x = "datetime")) +
          geom_line(aes_string(y = "T3"), col = "cornflowerblue") +
          geom_line(aes_string(y = "T2"), col = "brown1") +
          geom_line(aes_string(y = "T1"), col = "darkgoldenrod") +
          theme_minimal() +
          ylab("Temperature") + xlab("Date") +
          ggtitle(paste("Site: ", siteid, "; Time: ", times[tt])) +
          scale_x_datetime(date_minor_breaks = "1 day") -> GG1
        
        dft %>%
          ggplot(aes_string(x = "datetime")) +
          geom_line(aes_string(y = "moist"), col = "blue") +
          theme_minimal() +
          ylab("Moisture") + xlab("Date") +
          ggtitle(paste("Site: ", siteid, "; Time: ", times[tt])) +
          scale_x_datetime(date_minor_breaks = "1 day") -> GG2
        
        print(plot_grid(plotlist = list(GG1, GG2), nrow = 2))
      }
    }
  }
  dev.off()
}  

#################################################################################
# Screening each site for possible errors

# SITE = 1
siteid <- 1

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-07-09")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = 2
siteid <- 2

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-07-12")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = 3
siteid <- 3

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-07-11")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = 4
siteid <- 4

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-07-09")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = 5
siteid <- 5

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-07-10")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = 6
siteid <- 6

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-07-10")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = 7
siteid <- 7

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-07-12")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = 8
siteid <- 8

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-07-10")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = 10
siteid <- 10

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-07-09")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = 11
siteid <- 11

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-07-11")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = 12
siteid <- 12

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-07-10")))
probls <- c(as_date(as_date("2020-09-19"):as_date("2021-07-05")))
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = 13
siteid <- 13

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-07-09")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = 14
siteid <- 14

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-07-11")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = 15
siteid <- 15

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-07-09")))
probls <- c(as_date(as_date("2022-07-12"):as_date("2022-09-29")))
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = 16
siteid <- 16

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-07-12")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = 17
siteid <- 17

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-07-10")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = 18
siteid <- 18

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-07-10")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = 19
siteid <- 19

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-07-09")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = 20
siteid <- 20

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-07-09")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = 21
siteid <- 21

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-07-10")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = 22
siteid <- 22

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-07-09")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = 23
siteid <- 23

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-07-09")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = 25
siteid <- 25

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-07-12")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = 26
siteid <- 26

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-07-09")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = 27
siteid <- 27

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-07-11")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = 28
siteid <- 28

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-07-11")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = 29
siteid <- 29

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-07-11")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = 30
siteid <- 30

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-07-09")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = 31
siteid <- 31

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-07-12")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = 32
siteid <- 32

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-07-10")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = 33
siteid <- 33

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-07-09")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = 35
siteid <- 35

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-09-24")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = 36
siteid <- 36

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-07-10")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = 37
siteid <- 37

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-07-10")))
probls <- c(as_date(as_date("2020-06-16"):as_date("2020-07-17")))
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = 38
siteid <- 38

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-07-12")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = 39
siteid <- 39

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-07-10")),
            as_date(as_date("2021-04-01"):as_date("2021-07-07")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = 40
siteid <- 40

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-07-11")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = 41
siteid <- 41

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-09-24")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = 42
siteid <- 42

office <- c(as_date(as_date(min(df$datetime)):as_date("2021-07-03")))
probls <- c()
hattu <- c()
T2T3moisterror <- c(as_date(as_date("2021-07-09"):as_date("2022-10-02")))

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% T2T3moisterror,
                        8, probl)) -> df2

# SITE = 44
siteid <- 44

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-07-10")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = 45
siteid <- 45

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-07-10")))
probls <- c(as_date(as_date("2020-10-06"):as_date("2021-07-07")))
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = 46
siteid <- 46

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-07-11")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = 47
siteid <- 47

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-07-11")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = 48
siteid <- 48

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-07-10")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = 50
siteid <- 50

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-07-12")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = 51
siteid <- 51

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-07-11")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = 52
siteid <- 52

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-07-12")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = 53
siteid <- 53

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-07-11")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = 54
siteid <- 54

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-07-12")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = 55
siteid <- 55

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-07-09")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

########################################################################
# FILL MISSING TIMESTAMPS WITH NA

df2 <- df2 %>% 
  mutate(site_id = paste0(site, "_", tomst_id))
df <- df %>% 
  mutate(site_id = paste0(site, "_", tomst_id))

sites2 <- unique(df$site_id)
sites2 <- sites2[!grepl("_NA", sites2)]

df3 <- data.frame()
for(i in sites2){
  #i <- "35_94194421"
  
  df %>% filter(site_id == i) -> temp
  
  temp %>% mutate(timediff = as.numeric(datetime - lag(datetime))) -> temp
  temp[1,"timediff"] <- 15
  holes <- table(temp$timediff)
  
  if(max(temp$timediff, na.rm = T) > 15){
    
    print(i)
    
    missingt <- c()
    for(ii in which(temp %>% pull(timediff) > 15)){
      
      temp %>% slice((ii-1):(ii+1)) %>% pull(timediff) -> diffs
      
      if(diffs[1] %% 15 == 0L){
        seq(temp %>% slice(ii-1) %>% pull(datetime),
            temp %>% slice(ii) %>% pull(datetime), by = "15 mins") -> seqs
        
        missingt <- c(missingt, 
                      as.character(seqs[which(!seqs %in% (temp %>% slice((ii-1):(ii+1)) %>% pull(datetime)))]))
        
      } else {
        seq(temp %>% slice(ii-1) %>% pull(datetime),
            temp %>% slice(ii) %>% pull(datetime), by = "10 mins") -> seqs
        
        missingt <- c(missingt, 
                      as.character(seqs[which(!seqs %in% (temp %>% slice((ii-1):(ii+1)) %>% pull(datetime)))]))
        
      }
    }
    
    missingdf <- data.frame(datetime = ymd_hms(missingt),
                            site_id = i)
    
    print(NROW(missingdf))
    
    temp %>% full_join(., missingdf, by = c("datetime", "site_id")) %>% 
      arrange(datetime) %>% 
      select(-timediff) -> temp
    
    df3 <- bind_rows(df3, temp)
    
  } else {
    
    temp %>% select(-timediff) -> temp
    
    df3 <- bind_rows(df3, temp)
  }
}

#################################################################################
# CALCULATE BIASES BASED ON THE NOT-IN-FIELD DATA
#

diffs_all <- data.frame()
for(i in sites2){
  # i <- "2_94194338"
  office <- df2 %>% filter(site_id == i) %>% 
    filter(probl == 2) %>% pull(date)
  office <- office[-which(office == max(office))]
  
  df %>% filter(site_id == i) %>%
    mutate(date = as_date(datetime)) %>% 
    filter(date %in% office) %>% 
    mutate(change1a = abs(T3 - lag(T3,1)),
           change1b = abs(T3 - lag(T3,2)),
           change1c = abs(T3 - lag(T3,3)),
           change1d = abs(T3 - lag(T3,4)),
           change1e = abs(T3 - lag(T3,5)),
           change1f = abs(T3 - lag(T3,6)),
           change1g = abs(T3 - lead(T3,1)),
           change1h = abs(T3 - lead(T3,2)),
           change1i = abs(T3 - lead(T3,3))) %>% 
    rowwise() %>%
    mutate(change1 = max(change1a, change1b, change1c,
                         change1d, change1e, change1f,
                         change1g, change1g, change1i, na.rm = T)) %>%
    mutate(T3 = ifelse(change1 > 0.1250, NA, T3)) %>% 
    filter(!is.na(T3)) %>% 
    as.data.frame() %>% 
    filter(complete.cases(.)) -> temp
  
  means <- c(T1 = mean(temp$T1),
             T2 = mean(temp$T2),
             T3 = mean(temp$T3))
  
  diffs <- round(means - median(means),4)
  
  print(i)
  print(diffs)
  
  diffs_all <- bind_rows(diffs_all,
                         bind_cols(data.frame(site_id = i), 
                                   as.data.frame(t(as.data.frame(diffs)))))
}

fwrite(diffs_all, "output/Correction_temperatures.csv")

###################################################################################
# Write out a error log

elog <- df2 %>% 
  mutate(runid = rleid(probl)) %>% 
  group_by(site, tomst_id, runid) %>% 
  summarise(start_date = min(date),
            end_date = max(date),
            probl = max(probl)) %>% 
  filter(probl != 0)

elog %>% write_csv("output/error_log.csv")

###################################################################################
# Delete erroneous data
#

# Delete not in field data
df3 %>% mutate(date = as_date(datetime)) %>%
  left_join(., df2 %>% select(site, date, probl)) %>% 
  filter(probl != 2) -> df3

# Look for weird extra measurements between real ones
df3 <- df3 %>% 
  mutate(mins = minute(datetime))

for(i in unique(df3$site_id)){
  print(i)
  
  td <- df3 %>% 
    filter(site_id == i)
  
  tb <- table(td$mins)/nrow(td)
  tb <- tb[tb < 0.0001]
  if(sum(tb)*nrow(td) > 0){
    print(paste0("Removing ", sum(tb)*nrow(td), " rows..."))
  }
  
  df3 <- df3 %>% 
    filter(!(site_id == i & mins %in% as.numeric(names(tb))))
  
}

###############################################################################
# PLOT CORRECTED

pdf("visuals/Temperature_graphs_corrected.pdf", 12, 10)
for(i in sites){
  #i <- "L12
  print(i)
  df3 %>% filter(site == i) %>% 
    mutate(T1 = as.numeric(ifelse(probl %in% c(1,4,9), NA, T1))) %>% 
    mutate(T2 = as.numeric(ifelse(probl %in% c(1,7,8), NA, T2))) %>% 
    mutate(T3 = as.numeric(ifelse(probl %in% c(1,4,5,7,8), NA, T3))) %>% 
    #group_by(date) %>% 
    #summarise_at(vars(i, "soil"), funs(mean, min, max), na.rm = T) %>% 
    #lapply(function(x) replace(x, is.infinite(x),NA)) %>% as_tibble() %>% 
    ggplot(aes_string(x="datetime")) +
    geom_line(aes_string(y = "T3"), col = "cornflowerblue") +
    geom_line(aes_string(y = "T2"), col = "brown1") +
    geom_line(aes_string(y = "T1"), col = "darkgoldenrod") +
    theme_minimal() +
    ylab("Temperature") + xlab("Date")+
    scale_y_continuous(limits = c(-20, 35))+
    ggtitle(i) -> GG1
  
  df3 %>% filter(site == i) %>% 
    mutate(moist = as.numeric(ifelse(probl %in% c(1,6,8,9), NA, moist))) %>% 
    mutate(moist = as.numeric(ifelse(T1 <= 1, NA, moist))) %>% 
    #group_by(date) %>% 
    #summarise_at(vars(i, "soil"), funs(mean, min, max), na.rm = T) %>% 
    #lapply(function(x) replace(x, is.infinite(x),NA)) %>% as_tibble() %>% 
    ggplot(aes_string(x="datetime")) +
    geom_line(aes_string(y = "moist"), col = "black") +
    theme_minimal() +
    ylab("Soil moisture count") + xlab("Date")+
    scale_y_continuous(limits = c(500, 4000))+
    ggtitle(i) -> GG2
  
  print(plot_grid(plotlist = list(GG1, GG2), nrow = 2))
  
}
dev.off()

fwrite(df3 %>% select(-c(zone, site_id, date, mins)) %>% 
         relocate(site, tomst_id) %>% rename(error_tomst = probl), 
       "output/tomst_data_raw.csv")

####################################################################################