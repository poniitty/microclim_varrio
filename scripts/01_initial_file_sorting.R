##########################################################################
# THIS CODE MAKES SOME QUALITY CHECKS IF THE SITE AND TOMST IDs LOOKS FINE
#

library(tidyverse)

data_dir <- "C:/Users/OMISTAJA/OneDrive - University of Helsinki/Kesä2022/Varrio2022"
data_dir_old <- "C:/Users/OMISTAJA/Documents/repos/varrio_microclimate/data"

# List binary and command files to be removed from repository if also data file exists

f <- c(list.files(data_dir, pattern = "binary_", recursive = T, full.names = T),
       list.files(data_dir, pattern = "command_", recursive = T, full.names = T))

for(i in f){ 
  if(file.exists(gsub("binary_","data_",i)) | file.exists(gsub("command_","data_",i))){
    unlink(i)
  } else {
    print(paste0("DATA FILE MISSING!!! ", i))
  } 
}
# If no printed messages then no problems

# # Haxo
# 
# f <- list.files("data", pattern = "-2021.ltd$", recursive = T, full.names = T)
# 
# for(i in f){ if(file.exists(gsub("-2021.ltd","-2021.csv",i))){
#   unlink(i)
# } else {
#   print(paste0("DATA FILE MISSING!!! ", i))
# } 
# }

###########################################################################
# Check Tomst ID-numbers from last year data
maxdt <- read_csv("data/reading_times_2021.csv") %>% 
  mutate(site = site)

f <- list.files(data_dir, pattern = "data_", recursive = T, full.names = T)

fi <- data.frame(file = f)

fi$site <- as.numeric(parse_number(unlist(lapply(fi$file, function(x) rev(strsplit(x, "/")[[1]])[2]))))

fi <- fi[order(fi$site),]

fi$tomst_id <- unlist(lapply(fi$file, function(x) as.numeric(strsplit(gsub("data_","",rev(strsplit(x, "/")[[1]])[1]), "_")[[1]][1])))

fi %>% group_by(tomst_id) %>% summarise(n = n()) %>% filter(n > 1) %>% pull(tomst_id) -> doubled_ids
fi %>% filter(tomst_id %in% doubled_ids) # check for weird things!!! Good if none

# Check if more than one data file in a folder
fi %>% group_by(site) %>% summarise(n = n()) %>% filter(n > 1) %>% pull(site) -> doubled_sites
fi %>% filter(site %in% doubled_sites) # check for weird things!!! Good if none

# No problems

###########################################################################
# Update the file list

f <- list.files(data_dir, pattern = "data_", recursive = T, full.names = T)

fi <- data.frame(file = f)

fi$site <- as.numeric(parse_number(unlist(lapply(fi$file, function(x) rev(strsplit(x, "/")[[1]])[2]))))

fi <- fi[order(fi$site),]

fi$tomst_id <- unlist(lapply(fi$file, function(x) as.numeric(strsplit(gsub("data_","",rev(strsplit(x, "/")[[1]])[1]), "_")[[1]][1])))

fi %>% group_by(tomst_id) %>% summarise(n = n()) %>% filter(n > 1) %>% pull(tomst_id) -> doubled_ids
fi %>% filter(tomst_id %in% doubled_ids) # check for weird things!!! Good if none

# Check if more than one data file in a folder
fi %>% group_by(site) %>% summarise(n = n()) %>% filter(n > 1) %>% pull(site) -> doubled_sites
fi %>% filter(site %in% doubled_sites) # check for weird things!!! Good if none

#######################################################################
# Check if missing sites in 2021 data
all <- full_join(fi, maxdt)

# Check for duplicate sites
all %>% group_by(site) %>% summarise(n = n()) %>% filter(n > 1) %>% pull(site) -> doubled_sites
all %>% filter(site %in% doubled_sites) # No, Good!

# Non-matching sites
all %>% filter(!complete.cases(.))

# No sites that occur only in 2021 data

all %>% filter(tomst_id == 94194299) # No such tomst_id in 2021 data, so it is fine
all %>% filter(tomst_id == 94212876) # No such tomst_id in 2021 data, so it is fine
all %>% filter(tomst_id == 94212877) # No such tomst_id in 2021 data, so it is fine

# For site 39 find 2021 data and copy to repository
f2 <- list.files(data_dir_old,
                 pattern = "data_", recursive = T, full.names = T)

# Copy site 42 data from last year data
f2[grepl("94194299", f2)]
file.copy(f2[grepl("94194299", f2)],
          paste0(data_dir, "/VAR039/data_94194299_0.csv"))

########################################################################################
# Update file list

f <- list.files(data_dir, pattern = "data_", recursive = T, full.names = T)

fi <- data.frame(file = f)

fi$site <- as.numeric(parse_number(unlist(lapply(fi$file, function(x) rev(strsplit(x, "/")[[1]])[2]))))

fi <- fi[order(fi$site),]

fi$tomst_id <- unlist(lapply(fi$file, function(x) as.numeric(strsplit(gsub("data_","",rev(strsplit(x, "/")[[1]])[1]), "_")[[1]][1])))

fi %>% group_by(tomst_id) %>% summarise(n = n()) %>% filter(n > 1) %>% pull(tomst_id) -> doubled_ids
fi %>% filter(tomst_id %in% doubled_ids) # check for weird things!!! Good if none

fi %>% group_by(site) %>% summarise(n = n()) %>% filter(n > 1) %>% pull(site) -> doubled_sites
fi %>% filter(site %in% doubled_sites) # check for weird things!!! Good if none
# This site 39 is like it should be

#######################################################################
# Check if Tomst ids match between years
all <- full_join(fi, maxdt %>% rename(tomst_id_21 = tomst_id))

# Check for duplicate sites
all %>% group_by(site) %>% summarise(n = n()) %>% filter(n > 1) %>% pull(site) -> doubled_sites
all %>% filter(site %in% doubled_sites) # These are fine

all %>% filter(tomst_id == tomst_id_21)
all %>% filter(tomst_id != tomst_id_21)
# All seems to match nicely!!!!!!!!!!


# Good to go and read the data


