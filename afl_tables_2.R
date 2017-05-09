library(xml2)
library(dplyr)
library(tidyr)
library(rvest)

# Get team, venue and state data ----
library(googlesheets)
# Run gs_auth() to set this up

gs <- gs_key("17041tChNHzRNYmi1nCCJvacOqbk19MJUzW8UVX91b_A")

teams <-  gs_read(gs, sheet = "AFL_data", ws = "Teams",
                  locale = readr::locale(encoding = "UTF-8")) %>%
    tbl_df() %>%
    mutate(is_vic = team_state=="Vic") %>%
    arrange(desc(is_vic), team)

teams$team %>% factor() -> teams$team

venues <- gs_read(gs, sheet = "AFL_data", ws = "Venues",
                  locale = readr::locale(encoding = "UTF-8")) %>%
    tbl_df()

states <- gs_read(gs, sheet = "AFL_data", ws = "States",
                  locale = readr::locale(encoding = "UTF-8")) %>%
    tbl_df()

seasons <- c(seq(2012,2017, by=1))

history_df <- data.frame()

match_or_ladder <- function(node_table) {
      node_table %>% nrow() %>% as.character() %>% switch(., "1"="Bye","2"="Match","19"="Ladder") %>% return()
}

for (season in seasons) {
    ## create variable for URL of season 2016 on afltables
    afltables <- read_html(paste0("http://afltables.com/afl/seas/",as.character(season),".html"))

    ## Count number of nodes of type td_table
    node_type <- "center table"
    no_of_tables <-  afltables %>%html_nodes(node_type)  %>% length()

    for (table_count in c(seq(1,no_of_tables,by=1))) {
        node_count <<-table_count
        node_table <<- afltables %>%
            html_nodes(node_type)  %>%
            .[[table_count]] %>%
            html_table(fill = TRUE, head = FALSE) %>%
            data.frame()

    ## Adelaide versus Geelong in 2014 cancelled. Table is one row by one column.
    if(node_table %>% ncol()==4) {
         node_table <- node_table %>%
                       mutate(table_type=match_or_ladder(.))
         history_df <- rbind(history_df, node_table)
        }
    }
}

matches_df <- history_df %>% filter(table_type=="Match")
ladders_df <- history_df %>% filter(table_type=="Ladder")
