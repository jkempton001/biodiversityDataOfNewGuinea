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
pngEndemicFrogsPage <- read_html("checklists/png_frogs_endemic_checklist.htm")
ipEndemicFrogsPage <- read_html("checklists/ip_frogs_endemic_checklist.htm")

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

pngEndemicFrogsBinomials <- pngEndemicFrogsPage %>%
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

ipEndemicFrogsBinomials <- ipEndemicFrogsPage %>%
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
    ng_endemic = NA,
    ip_endemic = NA,
    png_endemic = NA,
    idn = NA,
    png = 1
  )

pngFrogs <- pngFrogs %>%
  relocate(ng_endemic, ip_endemic, png_endemic, idn, png, .after = binomial)

ipFrogs <- ipFrogsBinomials %>%
  mutate(
    ng_endemic = NA,
    ip_endemic = NA,
    png_endemic = NA,
    idn = 1,
    png = NA
  )

ipFrogs <- ipFrogs %>%
  relocate(ng_endemic, ip_endemic, png_endemic, idn, png, .after = binomial)

# outer join of idpBirds and pngBirds
ngFrogs <- full_join(ipFrogs, pngFrogs, by = "binomial") %>%
  mutate(
    idn = coalesce(idn.x, idn.y),
    png = coalesce(png.x, png.y),
    ng_endemic = coalesce(ng_endemic.x, ng_endemic.y),
    ip_endemic = coalesce(ip_endemic.x, ip_endemic.y),
    png_endemic = coalesce(png_endemic.x, png_endemic.y),
    basionym_year = coalesce(basionym_year.x, basionym_year.y)
  ) %>%
  select(binomial, ng_endemic, ip_endemic, png_endemic, idn, png, basionym_year)

ngFrogs$idn[is.na(ngFrogs$idn)] <- 0
ngFrogs$png[is.na(ngFrogs$png)] <- 0
ngFrogs$ng_endemic = 0
ngFrogs <- ngFrogs %>%
  mutate(ng_endemic = ifelse(binomial %in% endemicFrogsBinomials$binomial, 1, ng_endemic))
ngFrogs$ip_endemic = 0
ngFrogs <- ngFrogs %>%
  mutate(ip_endemic = ifelse(binomial %in% ipEndemicFrogsBinomials$binomial, 1, ip_endemic))
ngFrogs$png_endemic = 0
ngFrogs <- ngFrogs %>%
  mutate(png_endemic = ifelse(binomial %in% pngEndemicFrogsBinomials$binomial, 1, png_endemic))




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

ng_endemic <- rows %>%
  html_element("td:nth-child(3)") %>%
  html_text2() %>%
  str_squish() %>%
  str_detect(regex("\\bendemic\\b", ignore_case = TRUE)) %>%
  as.integer()

ngBirds <- tibble(
  binomial = binomial,
  ng_endemic  = ng_endemic
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
    ng_endemic = NA,
    idn = NA,
    png = 1,
    basionym_year = NA
  )

ppBirds <- ppBirds %>%
  mutate(
    ng_endemic = NA,
    idn = 1,
    png = NA,
    basionym_year = NA
  )

wpBirds <- wpBirds %>%
  mutate(
    ng_endemic = NA,
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
    ng_endemic = coalesce(ng_endemic.x, ng_endemic.y),
    basionym_year = coalesce(basionym_year.x, basionym_year.y)
  ) %>%
  select(binomial, ng_endemic, idn, png, basionym_year)

ngChecklist <- ippngBirds %>%
  filter(binomial %in% ngBirds$binomial) %>%
  select(-ng_endemic) %>%
  left_join(ngBirds %>% select(binomial, ng_endemic), by = "binomial") %>%
  select(binomial, ng_endemic, idn, png, basionym_year)

ngChecklist$idn[is.na(ngChecklist$idn)] <- 0
ngChecklist$png[is.na(ngChecklist$png)] <- 0

hbwChecklist = read_excel("checklists/birdsTaxonomicChecklist.xlsx")

ngChecklist <- ngChecklist %>%
  inner_join(hbwChecklist %>% 
               select(binomial = colnames(hbwChecklist)[8], 
                      basionym_year_hbw = colnames(hbwChecklist)[9]), 
             by = "binomial") %>%
  mutate(basionym_year = coalesce(basionym_year_hbw, basionym_year)) %>%
  select(binomial, ng_endemic, idn, png, basionym_year)

ngChecklist <- ngChecklist %>%
  distinct(binomial, .keep_all = TRUE)

ngChecklist <- ngChecklist %>%
  mutate(basionym_year = as.numeric(gsub("[^0-9]", "", basionym_year)))

ngChecklist <- ngChecklist %>%
  mutate(
    ip_endemic = NA,
    png_endemic = NA
  )

ngChecklist <- ngChecklist %>%
  relocate(ip_endemic, png_endemic, .after = ng_endemic)


ngChecklist <- ngChecklist %>%
  mutate(
    ip_endemic = case_when(
      ng_endemic == 1 & idn == 1 & png == 0 ~ 1,
      ng_endemic == 1 & idn == 0 & png == 1 ~ 0,
      TRUE ~ ip_endemic
    ),
    png_endemic = case_when(
      ng_endemic == 1 & idn == 1 & png == 0 ~ 0,
      ng_endemic == 1 & idn == 0 & png == 1 ~ 1,
      TRUE ~ png_endemic
    )
  )

ngChecklist$ip_endemic[is.na(ngChecklist$ip_endemic)] <- 0
ngChecklist$png_endemic[is.na(ngChecklist$png_endemic)] <- 0


write.csv(ngChecklist,"checklists/ngBirdsChecklist.csv")
