## AMPHIBIANS

library(rvest)
library(stringr)
library(xml2)
library(tibble)
library(dplyr)
library(readxl)

# point to the saved HTML file
page <- read_html("ngAmphibiansInventory.htm")

binomial_tbl <- page %>%
  html_elements("div[class~='Species'] a") %>%
  html_text2() %>%
  str_squish() %>%
  tibble(full_name = .) %>%
  mutate(
    binomial = str_extract(
      full_name,
      "^[A-Z][a-z]+\\s+[a-z][a-z-]*"
    ),
    year = str_extract(
      full_name,
      "\\b(17|18|19|20)\\d{2}\\b"
    ) %>% as.integer()
  ) %>%
  filter(!is.na(binomial)) %>%
  distinct(binomial, .keep_all = TRUE) %>%
  select(binomial, year)

binomial_tbl

## BIRDS -- the below code is very dodgy for creating checklist. Review to do properly at future date.

# Read the webpage
url <- "https://avibase.bsc-eoc.org/checklist.jsp?region=IDij"
page <- read_html("indonesianPapuaBirdsInventory.htm")

# Extract all italic text within the table (these are the binomials)
binomials <- page %>%
  html_nodes("table.table i") %>%
  html_text()

# View the results
print(binomials)

# Optional: create a data frame with common names too
bird_data <- page %>%
  html_nodes("table.table tr.highlight1") %>%
  lapply(function(row) {
    common_name <- row %>% html_node("td:nth-child(1)") %>% html_text()
    binomial <- row %>% html_node("i") %>% html_text()
    data.frame(common_name = common_name, binomial = binomial)
  }) %>%
  bind_rows()

# Combine PNG and IP birds
pngBirds = read.csv("Papua-New-Guinea-Species.csv")
pngBirds = pngBirds[,1:2]
bird_data = bird_data %>% rename(CommonName = common_name)
bird_data = bird_data %>% rename(ScientificName = binomial)
ngBirds = rbind(bird_data,pngBirds)
ngBirds = ngBirds %>%
  distinct(ScientificName, .keep_all = TRUE)

# Check names against HBW

hbwList = read_excel("birdsTaxonomicChecklist.xlsx")
ngBirdsFilt <- ngBirds %>%
  filter(ngBirds[[2]] %in% hbwList[[8]])

# Create a lookup from hbwList
lookup <- hbwList[, c(8, 9)]
names(lookup) <- c("ScientificName", "authority")

# Join to ngBirdsFilt
ngBirdsFilt <- ngBirdsFilt %>%
  left_join(lookup, by = "ScientificName") %>%
  mutate(year = str_extract(authority, "\\d{4}"))

# Remove duplicates keeping the first occurrence
ngBirdsFilt <- ngBirdsFilt %>%
  distinct(ScientificName, .keep_all = TRUE)

write.csv(ngBirdsFilt,"ngBirdsInventory.csv")
