library(tidyverse)
library(tidytext)
library(ggsci)
library(pdftools)
library(forcats)


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

# data(stop_words)

paper_words <- mutate(pdfCorpus, text = gsub(x = word, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = "")) %>%
  unnest_tokens(input = text, output = word) %>%
  # filter(!word %in% c(stop_words$word, "et", "al", "doi", "de", "fig", "figure", "data", "table", "cm", "uk", "jordi", "es")) %>% 
  count(paper, word, sort = TRUE)

total_words <- paper_words %>% 
  group_by(paper) %>% 
  summarize(total = sum(n))

paper_words <- left_join(paper_words, total_words)

paper_words %>% 
  filter(paper %in% c("2012_Pagès et al. FuncEcol.pdf",
                      "2010_Pagès et al. JEMBE.pdf",
                      "2013_Gera et al. JEcology.pdf",
                      "2013_Pagès et al. MEPS.pdf",
                      "2013_Pagès et al. PlosOne.pdf")) %>% 
  ggplot(aes(n/total, fill = paper)) +
  geom_histogram(show.legend = FALSE) +
  # xlim(NA, 0.0009) +
  facet_wrap(~paper, ncol = 2, scales = "free_y")


# The idea of tf-idf is to find the important words for the content of each document by decreasing the weight for commonly 
# used words and increasing the weight for words that are not used very much in a collection or corpus of documents, 
# in this case, the group of Jane Austen’s novels as a whole. Calculating tf-idf attempts to find the words that are
# important (i.e., common) in a text, but not too common.
paper_tf_idf <- paper_words %>%
  bind_tf_idf(word, paper, n)

paper_tf_idf

paper_tf_idf %>% 
  select(-total) %>% 
  arrange(desc(tf_idf))

papers <- unique(paper_tf_idf$paper)
for(i in 1:length(papers)){
  plot <- paper_tf_idf %>%
  filter(paper == papers[i]) %>% 
  slice_max(tf_idf, n = 15) %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = paper)) +
    geom_col(show.legend = FALSE) +
    labs(x = "tf-idf", y = NULL) +
    scale_fill_manual(values = "#3B4992FF") +
    theme_bw() +
    theme(legend.position = "none",
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          text = element_text(size = 11))
  ggsave(plot, filename = paste("Figs/tf_idf/", papers[i], sep = ""), width = 1000, height = 800, units = "px")
}


# Dendrogram tf-df
paper_tf_idf_to_clust <- paper_tf_idf %>% 
  filter(paper != "Part B1_ok.pdf") %>%
  group_by(paper) %>% 
  slice_max(tf_idf, n = 15) %>% 
  select(paper, word)

  
  
  
  