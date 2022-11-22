library(tm)
library(tidyverse)
library(tidytext)
library(ggsci)
library(pdftools)
library(forcats)
library(ggdendro)

all_pdfs <- list.files(path = "pdfs/",
                       pattern = ".pdf$")

pdfCorpus <- map_df(all_pdfs, ~ tibble(txt = pdf_text(paste("pdfs/", .x, sep = ""))) %>%
                      mutate(paper = .x) %>%
                      unnest_tokens(word, txt))

pdfCorpus %>% 
  anti_join(stop_words) %>% 
  group_by(paper) %>% 
  summarise(n()) %>% 
  print(n = Inf)

data(stop_words)

paper_words <- mutate(pdfCorpus, text = gsub(x = word, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = "")) %>%
  filter(paper != "Part B1_ok.pdf") %>% 
  unnest_tokens(input = text, output = word) %>%
  filter(!word %in% c(stop_words$word)) %>%
  filter(!word %in% c("et", "al", "doi", "de", "fig", "figure", "data", "table", "uk", "jordi", "es")) %>%
  filter(!word %in% c("https", "pp", "dfss", "doiorg",
                      "creative", "commons", "license","https", "gb",
                      "csic","online", "library", "wiley",
                      "agupubsonlinelibrarywileycom","downloaded",
                      "governed", "conditions", "terms", "urici", "central", "oa", "oficialia", "onlinelibrarywileycom","organización")) %>%
  count(paper, word, sort = TRUE)

total_words <- paper_words %>% 
  group_by(paper) %>% 
  summarize(total = sum(n))

paper_words <- left_join(paper_words, total_words)

paper_tf_idf <- paper_words %>%
  bind_tf_idf(word, paper, n)

paper_tf_idf

# Converting from tidy_Text to Document-Term-Matrix
# https://www.tidytextmining.com/dtm.html#dtm
DTM <- paper_tf_idf %>%
  cast_dtm(paper, word, tf_idf)

# https://rpubs.com/saqib/DocumentClustering
# Document Clustering using tm package
inspect(DTM)
DTMmatrix <- as.matrix(DTM)
distMatrix <- dist(DTMmatrix, method="euclidean")
groups <- hclust(distMatrix,method="ward.D")
 # plot(groups, cex=0.9, hang=-1) 
 # rect.hclust(groups, k=15)

ggdendrogram(groups, rotate = T)
# ggsave("Figs/tf_idf/DocumentClustering_Nov2022_StopwordsEtAlDoi_othersFinal.pdf")
