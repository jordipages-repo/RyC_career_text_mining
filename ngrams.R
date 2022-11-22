library(tidyverse)
library(tidytext)
library(ggsci)
library(pdftools)
library(forcats)


all_pdfs <- list.files(path = "pdfs/",
                       pattern = ".pdf$")

papers_bigrams <- map_df(all_pdfs[-33], ~ data_frame(txt = pdf_text(paste("pdfs/", .x, sep = ""))) %>%
                      mutate(paper = .x) %>%
                      unnest_tokens(bigram, txt, token = "ngrams", n = 2))

papers_bigrams %>%
  count(bigram, sort = TRUE)
# Here we get the usuals suspects. Of the, et al, in the...

# let's filter by stopwords
data("stop_words")
bigrams_separated <- papers_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  filter(!str_detect(word1, "[0-9]+|[[:punct:]]|\\(.*\\)")) %>% # to remove numbers etc.
  filter(!str_detect(word2, "[0-9]+|[[:punct:]]|\\(.*\\)")) # to remove numbers etc.


# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_counts %>% 
  unite(bigram, word1, word2, sep = " ") %>% 
  slice_max(n, n = 15) %>% 
  ggplot(aes(y = n, x = fct_reorder(bigram, n))) +
  geom_bar(stat = "identity", fill = "#1F77B4FF") +
  xlab("") +
  ylab("Frecuencia absoluta") +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text = element_text(size = 20))
# ggsave(filename = "Figs/bigramsTODO.png", width = 1920, height = 1080, units = "px")


bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigrams_united

bigram_tf_idf <- bigrams_united %>%
  count(paper, bigram) %>%
  bind_tf_idf(bigram, paper, n) %>%
  arrange(desc(tf_idf))
bigram_tf_idf

bigram_tf_idf %>%
  slice_max(tf_idf, n = 15) %>%
  ggplot(aes(tf_idf, fct_reorder(bigram, tf_idf))) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = "#008B45FF") +
  labs(x = "tf-idf", y = NULL) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text = element_text(size = 11))



papers <- unique(bigram_tf_idf$paper)
for(i in 1:length(papers)){
plot <- bigram_tf_idf %>%
  filter(paper == papers[i]) %>% 
  # group_by(paper) %>%
  slice_max(tf_idf, n = 15) %>%
  # ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(bigram, tf_idf), fill = paper)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = "#3B4992FF") +
  # facet_wrap(~paper, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text = element_text(size = 11))
ggsave(plot, filename = paste("Figs/ngrams/", papers[i], sep = ""), width = 1000, height = 800, units = "px")
}




####################

# DOCENCIA
# create a data frame listing all files to be analyzed
all_txts <- list.files(path = "txts/",
                       pattern = ".txt$",  # this pattern only selects files ending with .txt
                       full.names = TRUE)  # gives the file path as well as name

# create a data frame with one word per line
my_corpus <- map_dfr(all_txts, ~ tibble(txt = read_file(.x)) %>%   # read in each file in list
                       mutate(filename = basename(.x)) %>%   # add the file name as a new column
                       unnest_tokens(bigram, txt, token = "ngrams", n = 2))   # split each word out as a separate row

my_corpus %>%
  count(bigram, sort = TRUE)
# Here we get the usuals suspects. Of the, et al, in the...

# let's filter by stopwords
data("stop_words")
bigrams_separated <- my_corpus %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  filter(!str_detect(word1, "[0-9]+|[[:punct:]]|\\(.*\\)")) %>% # to remove numbers etc.
  filter(!str_detect(word2, "[0-9]+|[[:punct:]]|\\(.*\\)")) # to remove numbers etc.


# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_counts

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigrams_united

bigram_tf_idf <- bigrams_united %>%
  count(filename, bigram) %>%
  bind_tf_idf(bigram, filename, n) %>%
  arrange(desc(tf_idf))
bigram_tf_idf

bigram_tf_idf %>%
  slice_max(tf, n = 10) %>%
  ggplot(aes(tf, fct_reorder(bigram, tf))) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = "#008B45FF") +
  # facet_wrap(~paper, ncol = 2, scales = "free") +
  labs(x = "tf", y = NULL) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text = element_text(size = 11))
ggsave(filename = paste("Figs/ngrams/", "docencia", ".pdf", sep = ""), width = 1000, height = 800, units = "px")

