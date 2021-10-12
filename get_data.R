library("tidyverse")
library("rvest")
library("rlist")

##---- Get the links for each team ----
link <- "https://fbref.com/en/comps/24/Serie-A-Stats"
camp_br <- read_html(link)

links <- camp_br %>% 
  html_nodes("#stats_squads_standard_for a") %>% 
  html_attr("href")

links_teams <- list(1:20)

for (i in 1:20){
  links_teams[i] <- paste("https://fbref.com",links[i], sep = '')
  rm(i)
}


## Complete the web link
links_data <- list()
for (n in 1:20){
  new_string <- paste(substring(links_teams[[n]], c(1,37), c(36,70)), 
                      collapse = '/2021/all_comps')
  new_string <- paste(new_string, '-All-Competitions', sep = '')
  links_data[[n]] <- new_string
  rm(new_string, n)
}

##---- Get the list of all players ----

c <- 1 # counter
players_list <- list()
for (i in 1:length(links_data)){
  temp_list <- links_data[[i]] %>% 
    read_html() %>% 
    html_nodes('#all_stats_standard th a') %>% 
    html_attr('href')
  for (n in 1:length(temp_list)){
    players_list[[c]] <- temp_list[[n]]
    c <- c + 1
  }
  if (i == length(links_data)){
    rm(i, temp_list, c, n)
  }
}

# Remove duplicates
players_list <- unique(players_list)

# Create the final list of links
players_links <- data.frame("links" = matrix(unlist(players_list), 
                                   nrow=length(players_list), 
                                   byrow=TRUE))

# Update the links and add players names
for (i in 1:nrow(players_links)){
  players_links$name[i] <- str_replace_all(substring(players_links$links[[i]], 22, 50), "-", " ")
  players_links$links[i] <-  paste("https://fbref.com",players_links$links[i], sep = '')
  new_data <- paste(substring(players_links$links[i], c(1,39), c(38,70)),
             collapse = 'all_comps/')
  new_data <- paste(new_data, '---All-Competitions', sep = '')
  players_links$links[i] <- new_data
  rm(i)
}
rm(new_data)

##---- Get the std data of all players ----
for (i in 1:nrow(players_links)){
  if (i == 1){
    players_data_std <- list()
    players_names <- list()
    pb <- winProgressBar(title="Progress Bar", 
                         label="0% done", 
                         min=0, max=100, 
                         initial=0)
  }
  try({data <- as.data.frame(players_links$links[i] %>% 
                               read_html() %>% 
                               html_node('#div_stats_standard_dom_lg') %>% 
                               html_table())
  players_data_std <- list.append(players_data_std, data)
  players_names <- list.append(players_names, players_links$name[i])}, silent = T)
  
  label <- sprintf("%d%% done", round((i/nrow(players_links))*100))
  setWinProgressBar(pb, i/(nrow(players_links))*100, label=label)
  
  if (i == nrow(players_links)){
    close(pb)
    rm(pb, i, label, data)
    }
}

##---- Save the std players' data in csv to create my data base ----
dir.create("players_data/")
for (i in 1:length(players_data_std)) {
  file_name <- str_replace_all(players_names[i], fixed(" "), "_")
  dir.create(paste("players_data/", 
                   file_name,
                   sep = ''))
  path = paste("players_data/", 
        file_name,"/", file_name, 
        "_std.csv", sep = '')
  write.csv(players_data_std[i], 
            file = path)
}
