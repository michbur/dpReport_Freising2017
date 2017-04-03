library(rentrez)
my_search <- entrez_search(db="pubmed", term=)
pubs <- entrez_fetch(db = "pubmed", id = my_search, rettype = "uilist")

search_year <- function(year){
  query <- paste("(digital PCR[TIAB] OR dPCR[TIAB])", "AND (", year, "[PDAT])")
  entrez_search(db="pubmed", term=query, retmax=0)$count
}

year <- 1990:2017
pubs <- sapply(year, search_year)
dpcr_pubs <- data.frame(year = year, n_pub = pubs)

mod <- nls(n_pub ~ exp(a + b * year), data = dpcr_pubs[-nrow(dpcr_pubs), ], start = list(a = 0, b = 0))

final_pubs <- cbind(dpcr_pubs, pred = predict(mod, list(year = dpcr_pubs[["year"]]))) %>% 
  melt(id.vars = "year") %>% 
  mutate(value = ifelse(variable == "pred" & year != 2017, 0, value)) %>% 
  mutate(variable = factor(variable, labels = c("Observed", "Predicted")))

write.csv(final_pubs, file = "./data/dpcr_final_data.csv", row.names = FALSE)
