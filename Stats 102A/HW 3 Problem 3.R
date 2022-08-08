# Starting page
teampage <- read_html("http://www.baseball-reference.com/teams/")


# TEST:
teampage %>% html_nodes("#teams_active .left a") %>% html_text()
# It works well


# Time to start our session! :
teampage_session <- html_session("http://www.baseball-reference.com/teams/")
teams <- teampage_session %>% html_nodes("#teams_active .left a") %>% html_text()


# Form an empty list called baseball to contain all this information in the loop
baseball_list <- list()

for(i in teams[1:30]) {
  
  baseball_list[[i]] <- teampage_session %>% 
    follow_link(i) %>% 
    read_html() %>% 
    html_nodes("#franchise_years") %>% 
    html_table()
  
  baseball_list[[i]]$current_team_names <- teampage_session %>% 
    follow_link(i) %>% 
    read_html() %>% 
    html_nodes("tr:nth-child(1) td:nth-child(2)") %>% 
    html_text()
}