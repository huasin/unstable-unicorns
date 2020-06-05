rm(list = ls())
options(stringsAsFactors = FALSE)
library(rvest)
library(dplyr)
library(purrr)


# Leer la pagina web ------------------------------------------------------

web <- "http://unstablegameswiki.com/"
web_page <- "http://unstablegameswiki.com/index.php?title=Unstable_Unicorns_Base_Deck_(2nd_Edition)_-_Cards_In_This_Deck"
page <- read_html(web_page)


# Div en el body donde está el listado ------------------------------------

base_2 <- page %>% 
  html_node("div.mw-parser-output") %>% 
  html_node("div") %>% 
  html_nodes("ul") %>% 
  html_nodes("li") %>%
  html_nodes("a")

base_2_title <- base_2 %>% html_attr("title")
base_2_href <- base_2 %>% html_attr("href")
base_2_text <- base_2 %>% html_text()

# Crear dataframe con los datos scrapeados --------------------------------

df_base_2 <- data.frame(text = base_2_text,
                         title = base_2_title,
                         href = paste0(web,base_2_href))


# Encontrar el tipo de carta ----------------------------------------------
# Primero creamos cum_flag que identifica con un numero diferente, tipos de
# carta diferentes
df_base_2 <- df_base_2 %>% 
  mutate(title_lag = lag(title, 1, default = "")) %>% 
  mutate(cum_flag = cumsum(ifelse(is.na(title_lag),1,0))) 

# Se crea un diccionario para identificar el tipo y su respectivo cum_flag
dict_base_2 <- df_base_2 %>% 
  filter(is.na(title_lag)) %>% 
  select(type = text,cum_flag)

# Se realiza un join para agregar el tipo y se filtra los registros no desea2
df_base_2 <- df_base_2 %>% 
  left_join(dict_base_2, by = "cum_flag") %>% 
  filter(!is.na(title),!is.na(title_lag))

# Caso especial: rule card
df_base_2 <- df_base_2 %>% 
  mutate(type = ifelse(text == "Rule Card", "Rule Card", type))

# Seleccionamos las variables que interesan -------------------------------
df_base_2 <- df_base_2 %>% 
  transmute(deck = "Base Deck 2nd Edition", type = type, card_name = text, link = href)


