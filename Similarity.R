library(tidyverse)
library(tidytext)
library(ggsci)
library(pdftools)
library(forcats)

# Load RESEARCH corpus
all_pdfs <- list.files(path = "pdfs/",
                       pattern = ".pdf$")

pdfCorpus <- map_df(all_pdfs, ~ tibble(txt = pdf_text(paste("pdfs/", .x, sep = ""))) %>%
                      mutate(paper = .x) %>%
                      unnest_tokens(word, txt))

wordcount_reseach <- pdfCorpus %>% 
  anti_join(stop_words) %>% 
  filter(!str_detect(word, "[0-9]+|[[:punct:]]|\\(.*\\)")) %>% # to remove numbers etc.
  count(word)


# Load DOCENCIA corpus
# create a data frame listing all files to be analyzed
all_txts <- list.files(path = "txts/",
                       pattern = ".txt$",  # this pattern only selects files ending with .txt
                       full.names = TRUE)  # gives the file path as well as name

# create a data frame with one word per line
docencia_corpus <- map_dfr(all_txts, ~ tibble(txt = read_file(.x)) %>%   # read in each file in list
                       mutate(filename = basename(.x)) %>%   # add the file name as a new column
                       unnest_tokens(word, txt))   # split each word out as a separate row

wordcount_docencia <- docencia_corpus %>%
  anti_join(stop_words) %>%
  filter(!str_detect(word, "[0-9]+|[[:punct:]]|\\(.*\\)")) %>% 
  count(word)


a <- intersect(wordcount_docencia$word, wordcount_reseach$word)
round(length(a)/length(wordcount_docencia$word)*100) # 73 %

b <- setdiff(wordcount_docencia$word, wordcount_reseach$word)
round(length(b)/length(wordcount_docencia$word)*100) # 27 %


# prova
bind_rows(docencia_corpus, pdfCorpus)



## piechart
# Load ggplot2
library(ggplot2)

# Create Data
data <- data.frame(
  group=c("comunes", "no comunes"),
  value=c(73,27)
)

data <- data %>% 
  arrange(desc(group)) %>%
  mutate(prop = value / sum(data$value) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

# Basic piechart
ggplot(data, aes(x="", y=prop, fill=group)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none") +
  geom_text(aes(y = ypos, label = group), color = "white", size=6) +
  geom_text(aes(y = ypos-6, label = paste(value, "%", sep = "")), color = "white", size=6) +
  scale_fill_brewer(palette="Set1")
ggsave("PieChart.pdf")
