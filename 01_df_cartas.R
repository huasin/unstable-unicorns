rm(list = ls())
options(stringsAsFactors = FALSE)
library(rvest)
library(dplyr)
library(purrr)

decks <- list(
  "NSFW Base Deck" = "http://unstablegameswiki.com/index.php?title=Unstable_Unicorns_NSFW_Base_Deck_-_Cards_In_This_Deck",
  "Chaos Deck" = "http://unstablegameswiki.com/index.php?title=Unstable_Unicorns_Chaos_Deck_-_Cards_In_This_Deck",
  "Control Deck" = "http://unstablegameswiki.com/index.php?title=Unstable_Unicorns_Control_Deck_-_Cards_In_This_Deck",
  "Base Deck" = "http://unstablegameswiki.com/index.php?title=Unstable_Unicorns_Base_Deck_-_Cards_In_This_Deck",
  "Unicorns Dragons" = "http://unstablegameswiki.com/index.php?title=Unstable_Unicorns_Dragons_(Retail)_-_Cards_In_This_Deck",
  "Rainbow Apocalypse" = "http://unstablegameswiki.com/index.php?title=Unstable_Unicorns_Rainbow_Apocalypse_-_Cards_In_This_Deck",
  "NSFW Expansion" = "http://unstablegameswiki.com/index.php?title=Unstable_Unicorns_NSFW_Expansion_-_Cards_In_This_Deck",
  "Unicorns of Legend" = "http://unstablegameswiki.com/index.php?title=Unstable_Unicorns_Unicorns_of_Legend_-_Cards_In_This_Deck",
  "Base Deck - 2nd Edition" = "http://unstablegameswiki.com/index.php?title=Unstable_Unicorns_Base_Deck_-_2nd_Edition"
)

# Leer la pagina web ------------------------------------------------------

web <- "http://unstablegameswiki.com/"
page <- map(decks, read_html)

# Div en el body donde está el listado ------------------------------------

cards <- map(page, ~.x %>% 
  html_node("div.mw-parser-output") %>% 
  html_node("div") %>% 
  html_nodes("ul") %>% 
  html_nodes("li") %>%
  html_nodes("a"))

cards_title <- map(cards, ~.x %>% html_attr("title"))
cards_href <- map(cards, ~.x %>% html_attr("href"))
cards_text <- map(cards, ~.x %>% html_text())

# Crear dataframe con los datos scrapeados --------------------------------

df_card <- pmap(.l = list(cards_text, cards_title, cards_href),
                .f = function(text,title,href) data.frame(text = text, title = title, href = href))

# Encontrar el tipo de carta ----------------------------------------------
# Primero creamos cum_flag que identifica con un numero diferente, tipos de
# carta diferentes
df_card <- map(df_card, ~.x %>% 
  mutate(title_lag = lag(title, 1, default = "")) %>% 
  mutate(cum_flag = cumsum(ifelse(is.na(title_lag),1,0)))) 

# Se crea un diccionario para identificar el tipo y su respectivo cum_flag
dict_card <- map(df_card, ~.x %>% 
  filter(is.na(title_lag)) %>% 
  select(type = text,cum_flag))

# Se realiza un join para agregar el tipo y se filtra los registros no desea2
df_card <- map2(.x = df_card, .y = dict_card, .f = ~.x %>% 
  left_join(.y, by = "cum_flag") %>% 
  filter(!is.na(title),!is.na(title_lag)))

# Caso especial: rule card
df_card <- map(df_card, ~.x %>% 
  mutate(type = ifelse(text == "Rule Card", "Rule Card", type)))

# Seleccionamos las variables que interesan -------------------------------
df_card <- map(df_card, ~.x %>% 
  transmute(card_name = text, type = type, link = href))


# Agregamos el nombre del deck --------------------------------------------

for(i in 1:length(df_card)) {
  df_card[[i]] <- df_card[[i]] %>% 
    mutate(deck = !!names(df_card)[i],
           link = paste0(!!web,link))
}


# Generamos un solo dataframe ---------------------------------------------

df <- df_card %>% bind_rows()

# Guardamos la data -------------------------------------------------------

saveRDS(df, "files/cartas.rds")
openxlsx::write.xlsx(df, "files/cartas.xlsx")


