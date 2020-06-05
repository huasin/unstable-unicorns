rm(list = ls())
options(stringsAsFactors = FALSE)
library(rvest)
library(dplyr)
library(purrr)


# Leer la data ------------------------------------------------------------

df <- readRDS("files/cartas.rds")

page <- read_html("http://unstablegameswiki.com//index.php?title=UU_-_Baby_Unicorn_of_Incest")

a <- page %>% 
  html_node("div.mw-parser-output") %>% 
  html_children() %>% 
  .[[3]] %>% 
  html_node("ul") %>% 
  html_text()
