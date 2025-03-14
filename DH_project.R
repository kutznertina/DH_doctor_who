# Skripte scrapen

library(rvest)
library(stringr)

base_url <- "http://www.chakoteya.net/DoctorWho/"

main_page <- read_html(base_url)

# Übersichtsseiten Doktoren
overview_links <- html_nodes(main_page, "a")
overview_links <- html_attr(overview_links, "href")

# Seiten filtern, htm oder html
doctor_overview_links <- overview_links[str_detect(overview_links, "episodes[0-9]*\\.htm[l]?$")]

# URLs für einzelne Doktor-Seiten bauen
doctor_urls <- paste0(base_url, doctor_overview_links)

cat("Gefundene Übersichtsseiten:\n")
print(doctor_urls)

# Skript Links aus Übersichtsseiten filtern
episode_script_urls <- c()

for (doctor_url in doctor_urls) {
  cat("Scrape Übersicht:", doctor_url, "\n")
  

  doctor_page <- read_html(doctor_url)
  
  # Links extrahieren
  episode_links <- html_nodes(doctor_page, "a")
  episode_links <- html_attr(episode_links, "href")
  
  # richtige Skript Links, Format: X-Y.htm(l)
  episode_script_links <- episode_links[str_detect(episode_links, "^\\d{1,2}-\\d{1,2}\\.htm[l]?$")]
  
  # URLs bauen, zu Liste hinzufügen
  full_episode_urls <- paste0(base_url, episode_script_links)
  episode_script_urls <- c(episode_script_urls, full_episode_urls)
}

cat("Gefundene Episoden Skripte:\n")
print(episode_script_urls)


# Skripte in Classic Who (bis 26-4), New Who (ab 27-1 bis 41-0) teilen
classic_urls <- episode_script_urls[str_detect(episode_script_urls, "/(0?[1-9]|1[0-9]|2[0-6])-[0-9]+\\.htm[l]?$")]
new_urls <- episode_script_urls[str_detect(episode_script_urls, "/(2?[7-9]|3[0-9]|4[0-9])-[0-9]*\\.htm[l]?$")]


cat("Classic Who Episoden gefunden:", length(classic_urls), "\n")
cat("New Who Episoden gefunden:", length(new_urls), "\n")


# neuer Ordner für Skripte (geteilt)
dir.create("DoctorWho_Classic", showWarnings = FALSE)
dir.create("DoctorWho_New", showWarnings = FALSE)


save_script <- function(url, folder) {
  cat("Lade Skript von:", url, "\n")
  

  script_page <- read_html(url)
  
  # Skripttext extrahieren
  script_text <- html_nodes(script_page, "body")
  script_text <-html_text(script_text)
  
  # Name Datei
  filename <- paste0(folder, "/script_", basename(url), ".txt")
  
  
  writeLines(script_text, filename)
  cat("Gespeichert:", filename, "\n")
  
  
}

# Classic Who Skripte speichern
for (url in classic_urls) {
  save_script(url, "DoctorWho_Classic")
}

# New Who Skripte speichern
for (url in new_urls) {
  save_script(url, "DoctorWho_New")
}

cat("Alle Skripte gespeichert\n")

print(new_urls)
print(episode_script_urls)

#############################################################################################
# Dateien umbennenen für PCA corpus


classic_path <- "DoctorWho_Classic"
new_path <- "DoctorWho_New"

# Classic Who 
classic_files <- list.files(classic_path, full.names = TRUE)

for (file in classic_files) {
  new_name <- file.path(classic_path, paste0("Classic_", basename(file)))
  file.rename(file, new_name)
}

# New Who
new_files <- list.files(new_path, full.names = TRUE)

for (file in new_files) {
  new_name <- file.path(new_path, paste0("New_", basename(file)))
  file.rename(file, new_name)
}

cat("Alle Dateien umbenannt")

#############################################################################################

install.packages(c("tm", "textstem", "tidyverse", "magrittr", "tidytext", "udpipe"))
library(tm)
library(textstem)
library(tidyverse)
library(tidytext)
library(udpipe)


classic_path <- "DoctorWho_Classic"
new_path <- "DoctorWho_New"

# Korpora erstellen
classic_corpus <- VCorpus(DirSource(classic_path, encoding = "UTF-8"),
                          readerControl = list(language = "en"))

new_corpus <- VCorpus(DirSource(new_path, encoding = "UTF-8"),
                      readerControl = list(language = "en"))


classic_corpus_clean <- gsub("\\(.*?\\)|\\[.*?\\]", "", classic_corpus)
new_corpus_clean <- gsub("\\(.*?\\)|\\[.*?\\]", "", new_corpus)

# Sprecher-Rollen entfrnen
classic_corpus_clean <- gsub("[A-Z]+[\\s]*[A-Z]*:", "", classic_corpus_clean)
new_corpus_clean <- gsub("[A-Z]+[\\s]*[A-Z]*:", "", new_corpus_clean)


ud_model <- udpipe_load_model("english-ewt-ud-2.5-191206.udpipe")


get_noun_counts <- function(corpus, ud_model) {
  df <- tibble(text = unlist(sapply(corpus, as.character)))
  df <- unnest_tokens(df, word, text) 
  
  # Wortarten analysieren
  tagged_words <- udpipe_annotate(ud_model, x = df$word)
  tagged_words <- as.data.frame(tagged_words)
  
  #Substantive filtern
  nouns <- tagged_words
  nouns <- filter(nouns, upos == "NOUN")
  nouns <- count(nouns, token, sort = TRUE)
  
  return(nouns)
}


classic_nouns <- get_noun_counts(classic_corpus_clean, ud_model)
new_nouns <- get_noun_counts(new_corpus_clean, ud_model)

# Ergebnisse
head(classic_nouns, 20)
head(new_nouns, 20)

# häufigsten Substantive plotten
ggplot(classic_nouns[1:20,], aes(x = reorder(token, n), y = n)) +
  geom_col(fill = "blue") +
  coord_flip() +
  labs(title = "Häufigste Substantive in Classic Who",
       x = "Substantiv", y = "Häufigkeit")

ggplot(new_nouns[1:20,], aes(x = reorder(token, n), y = n)) +
  geom_col(fill = "red") +
  coord_flip() +
  labs(title = "Häufigste Substantive in New Who",
       x = "Substantiv", y = "Häufigkeit")
#######################################################
# PCA 

library("stylo")
stylo()

