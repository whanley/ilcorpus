library(xml2)
library(tidyverse)
library(tokenizers)
library(stringr)

# load files
bslc_files <- dir("~/GitHub/ilcorpus/journals/bslc/bslc-issues", full.names = TRUE)
clunet_files <- dir("~/GitHub/ilcorpus/journals/clunet/clunet-issues", full.names = TRUE)
rdilc_files <- dir("~/GitHub/ilcorpus/journals/rdilc/rdilc-issues", full.names = TRUE)

# function to extract notes and metadata for all divs
extract_note_data <- function(x){
  # read file
  file <- read_xml(x)
  # extract notes
  notes <- file %>% xml_find_all(".//d1:note[@place='bottom']")
  # remove spaces and line breaks
  text <- notes %>% xml_text() %>% gsub("\\s+", " ", .) %>% gsub("\n", " ", .)
  # get metadata 
  head <- notes %>% xml_find_first("./parent::d1:div/d1:head") %>% xml_text()
  author <- notes %>% xml_find_first("./parent::d1:div/d1:byline") %>% xml_text()
  type <- notes %>% xml_find_first("./parent::d1:div") %>% xml_attr("type")
  feature <- notes %>% xml_find_first("./parent::d1:div") %>% xml_attr("feature")
  filename <- x %>% gsub(".+/GitHub/ilcorpus/journals/[a-z]+/[a-z,-]+/", "", .) %>% gsub(".xml", "", .)
  year <- filename %>% gsub("[a-z]+-([0-9][0-9]?-)?", "", .) %>% gsub("[[:punct:]]\\d$", "", .)

  notes_tibble <- tibble(text, head, author, type, feature, filename, year = as.numeric(year))
}

# run function and make tibble
clunet_notes <- lapply(clunet_files, extract_note_data)
clunet_list <- do.call(rbind, clunet_notes)
clunet_notes_tibble <- as_tibble(clunet_list, .name_repair = "minimal")
# add word count
clunet_notes_table <- clunet_notes_tibble %>% 
  mutate(counter = str_count(clunet_notes_tibble$text, pattern = "\\w+"))

# run function and make tibble
rdilc_notes <- lapply(rdilc_files, extract_note_data)
rdilc_list <- do.call(rbind, rdilc_notes)
rdilc_notes_tibble <- as_tibble(rdilc_list, .name_repair = "minimal")
# add word count
rdilc_notes_table <- rdilc_notes_tibble %>% 
  mutate(counter = str_count(rdilc_notes_tibble$text, pattern = "\\w+"))

# run function and make tibble
bslc_notes <- lapply(bslc_files, extract_note_data)
bslc_list <- do.call(rbind, bslc_notes)
bslc_notes_tibble <- as_tibble(bslc_list, .name_repair = "minimal")
# add word count
bslc_notes_table <- bslc_notes_tibble %>% 
  mutate(counter = str_count(bslc_notes_tibble$text, pattern = "\\w+"))

# export as csv
write_csv(clunet_notes_table, file = "clunet_notes.csv")
write_csv(rdilc_notes_table, file = "rdilc_notes.csv")
write_csv(bslc_notes_table, file = "bslc_notes.csv")

