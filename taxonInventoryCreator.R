## AMPHIBIANS

library(rvest)
library(stringr)
library(xml2)
library(tibble)
library(dplyr)
library(readxl)

# point to the saved HTML file
pngFrogsPage <- read_html("checklists/png_frogs_checklist.htm")
ipFrogsPage <- read_html("checklists/ip_frogs_checklist.htm")
endemicFrogsPage <- read_html("checklists/ng_frogs_endemic_checklist.htm")

pngFrogsBinomials <- pngFrogsPage %>%
  html_elements("div[class~='Species'] a") %>%
  html_text2() %>%
  str_squish() %>%
  tibble(full_name = .) %>%
  mutate(
    binomial = str_extract(
      full_name,
      "^[A-Z][a-z]+\\s+[a-z][a-z-]*"
    ),
    basionym_year = str_extract(
      full_name,
      "\\b(17|18|19|20)\\d{2}\\b"
    ) %>% as.integer()
  ) %>%
  filter(!is.na(binomial)) %>%
  distinct(binomial, .keep_all = TRUE) %>%
  select(binomial, basionym_year)

ipFrogsBinomials <- ipFrogsPage %>%
  html_elements("div[class~='Species'] a") %>%
  html_text2() %>%
  str_squish() %>%
  tibble(full_name = .) %>%
  mutate(
    binomial = str_extract(
      full_name,
      "^[A-Z][a-z]+\\s+[a-z][a-z-]*"
    ),
    basionym_year = str_extract(
      full_name,
      "\\b(17|18|19|20)\\d{2}\\b"
    ) %>% as.integer()
  ) %>%
  filter(!is.na(binomial)) %>%
  distinct(binomial, .keep_all = TRUE) %>%
  select(binomial, basionym_year)

endemicFrogsBinomials <- endemicFrogsPage %>%
  html_elements("div[class~='Species'] a") %>%
  html_text2() %>%
  str_squish() %>%
  tibble(full_name = .) %>%
  mutate(
    binomial = str_extract(
      full_name,
      "^[A-Z][a-z]+\\s+[a-z][a-z-]*"
    )
  ) %>%
  filter(!is.na(binomial)) %>%
  distinct(binomial, .keep_all = TRUE) %>%
  select(binomial)

pngFrogs <- pngFrogsBinomials %>%
  mutate(
    endemic = NA,
    idn = NA,
    png = 1
  )

# Relocate the new columns to their desired positions (e.g., after column 'A')
pngFrogs <- pngFrogs %>%
  relocate(endemic, idn, png, .after = binomial)

ipFrogs <- ipFrogsBinomials %>%
  mutate(
    endemic = NA,
    idn = 1,
    png = NA
  )

# Relocate the new columns to their desired positions (e.g., after column 'A')
ipFrogs <- ipFrogs %>%
  relocate(endemic, idn, png, .after = binomial)

# outer join of idpBirds and pngBirds
ngFrogs <- full_join(ipFrogs, pngFrogs, by = "binomial") %>%
  mutate(
    idn = coalesce(idn.x, idn.y),
    png = coalesce(png.x, png.y),
    endemic = coalesce(endemic.x, endemic.y),
    basionym_year = coalesce(basionym_year.x, basionym_year.y)
  ) %>%
  select(binomial, endemic, idn, png, basionym_year)

ngFrogs$idn[is.na(ngFrogs$idn)] <- 0
ngFrogs$png[is.na(ngFrogs$png)] <- 0
ngFrogs$endemic = 0
ngFrogs <- ngFrogs %>%
  mutate(endemic = ifelse(binomial %in% endemicFrogsBinomials$binomial, 1, endemic))

write.csv(ngFrogs,"checklists/ngFrogsChecklist.csv")



## BIRDS 

# Read webpages

ngBirdsPage <- read_html("ng_birds_checklist_avibase.htm")
pngBirdsPage <- read_html("png_birds_checklist_avibase.htm")
ppBirdsPage <- read_html("papua_birds_checklist_avibase.htm")
wpBirdsPage <- read_html("west_papua_birds_checklist_avibase.htm")

rows <- html_elements(ngBirdsPage, "tr.highlight1")

binomial <- rows %>%
  html_element("td:nth-child(2) i") %>%
  html_text2() %>%
  str_squish()

endemic <- rows %>%
  html_element("td:nth-child(3)") %>%
  html_text2() %>%
  str_squish() %>%
  str_detect(regex("\\bendemic\\b", ignore_case = TRUE)) %>%
  as.integer()

ngBirds <- tibble(
  binomial = binomial,
  endemic  = endemic
) %>%
  filter(!is.na(binomial), binomial != "")

# Extract all italic text within the table (these are the binomials)
pngBinomials <- pngBirdsPage %>%
  html_nodes("table.table i") %>%
  html_text()
ppBinomials <- ppBirdsPage %>%
  html_nodes("table.table i") %>%
  html_text()
wpBinomials <- wpBirdsPage %>%
  html_nodes("table.table i") %>%
  html_text()

ngBirds = data.frame(ngBinomials)
ngBirds <- ngBirds %>%
  rename(binomial = ngBinomials)
pngBirds = data.frame(pngBinomials)
pngBirds <- pngBirds %>%
  rename(binomial = pngBinomials)
ppBirds = data.frame(ppBinomials)
ppBirds <- ppBirds %>%
  rename(binomial = ppBinomials)
wpBirds = data.frame(wpBinomials)
wpBirds <- wpBirds %>%
  rename(binomial = wpBinomials)


ngBirds <- ngBirds %>%
  mutate(
    idn = NA,
    png = NA,
    basionym_year = NA
  )

pngBirds <- pngBirds %>%
  mutate(
    endemic = NA,
    idn = NA,
    png = 1,
    basionym_year = NA
  )

ppBirds <- ppBirds %>%
  mutate(
    endemic = NA,
    idn = 1,
    png = NA,
    basionym_year = NA
  )

wpBirds <- wpBirds %>%
  mutate(
    endemic = NA,
    idn = 1,
    png = NA,
    basionym_year = NA
  )

ipBirds <- bind_rows(wpBirds, ppBirds) %>%
  distinct(binomial, .keep_all = TRUE)



# outer join of idpBirds and pngBirds
ippngBirds <- full_join(ipBirds, pngBirds, by = "binomial") %>%
  mutate(
    idn = coalesce(idn.x, idn.y),
    png = coalesce(png.x, png.y),
    endemic = coalesce(endemic.x, endemic.y),
    basionym_year = coalesce(basionym_year.x, basionym_year.y)
  ) %>%
  select(binomial, endemic, idn, png, basionym_year)

ngChecklist <- ippngBirds %>%
  filter(binomial %in% ngBirds$binomial) %>%
  select(-endemic) %>%
  left_join(ngBirds %>% select(binomial, endemic), by = "binomial") %>%
  select(binomial, endemic, idn, png, basionym_year)

ngChecklist$idn[is.na(ngChecklist$idn)] <- 0
ngChecklist$png[is.na(ngChecklist$png)] <- 0

hbwChecklist = read_excel("checklists/birdsTaxonomicChecklist.xlsx")

ngChecklist <- ngChecklist %>%
  inner_join(hbwChecklist %>% 
               select(binomial = colnames(hbwChecklist)[8], 
                      basionym_year_hbw = colnames(hbwChecklist)[9]), 
             by = "binomial") %>%
  mutate(basionym_year = coalesce(basionym_year_hbw, basionym_year)) %>%
  select(binomial, endemic, idn, png, basionym_year)

ngChecklist <- ngChecklist %>%
  distinct(binomial, .keep_all = TRUE)

ngChecklist <- ngChecklist %>%
  mutate(basionym_year = as.numeric(gsub("[^0-9]", "", basionym_year)))

write.csv(ngChecklist,"checklists/ngBirdsChecklist.csv")
