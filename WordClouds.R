library(tidyverse)
library(tidytext)
library(wordcloud2)
library(wordcloud)
library(ggwordcloud)
library(ggsci)
library(webshot)
library(pdftools)

# # # # # # # # # 
# # DOCÈNCIA ----
# # # # # # # # #
# 
# # create a data frame listing all files to be analyzed
# all_txts <- list.files(path = "txts/",
#                        pattern = ".txt$",  # this pattern only selects files ending with .txt
#                        full.names = TRUE)  # gives the file path as well as name
# 
# # create a data frame with one word per line
# my_corpus <- map_dfr(all_txts, ~ tibble(txt = read_file(.x)) %>%   # read in each file in list
#                        mutate(filename = basename(.x)) %>%   # add the file name as a new column
#                        unnest_tokens(word, txt))   # split each word out as a separate row
# 
# wordCount <- my_corpus %>%
#   anti_join(stop_words) %>%
#   filter(!str_detect(word, "[0-9]+|[[:punct:]]|\\(.*\\)")) %>% 
#   count(word)
# 
# # wordcloud(wordCount$word,wordCount$n, max.words = 70, color = pal_aaas(palette = "default")(5))
# 
# subwordCount <- wordCount %>% 
#   arrange(desc(n)) %>% 
#   filter(n>=16)
# 
# # wordcloud(subwordCount$word,subwordCount$n, color = pal_aaas(palette = "default")(5))
# 
# wordcloud2(subwordCount[,1:2], fontFamily = "arial", 
#            fontWeight ="normal", color = pal_aaas(palette = "default")(10), 
#            minRotation = 0, maxRotation = 0,
#            size = 0.8)
# 
# # ggplot(subwordCount, aes(label = word, size = n)) +
# #   geom_text_wordcloud() +
# #   scale_radius(range = c(0, 20), limits = c(0, NA)) +
# #   # scale_size_area(max_size = 12) +
# #   theme_minimal()

# # # # # #
# Articles ----
# # # # # #

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

cleancorpus <- mutate(pdfCorpus, text = gsub(x = word, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = "")) %>%
  unnest_tokens(input = text, output = word) %>%
  filter(!word %in% c(stop_words$word, "et", "al", "doi", "de", "fig", "figure", "data", "table", "cm", "uk", "jordi", "es"))


# 2010 JEMBE salinity
PagesJEMBE2010 <- cleancorpus %>%
  filter(paper == "2010_Pagès et al. JEMBE.pdf") %>% 
  filter(!word %in% c("psu", "lizaso", "torquemada", "romero", "pérez", "found", "term", "rates")) %>% 
  anti_join(stop_words) %>%
  count(word) %>% 
  arrange(desc(n)) %>% 
  filter(n>=5)
  
wordcloud2(PagesJEMBE2010[,1:2], fontFamily = "arial", 
           fontWeight ="normal", color = pal_aaas(palette = "default")(10), 
           minRotation = 0, maxRotation = 0,
           size = 1.2)


# 2012 FunEcol indirect interactions
PagesFunEcol2012 <- cleancorpus %>%
  filter(paper == "2012_Pagès et al. FuncEcol.pdf") %>% 
  # filter(paper %in% c("2012_Pagès et al. FuncEcol.pdf",
                      # "2010_Pagès et al. JEMBE.pdf")) %>% 
  filter(!word %in% c("ns", "sala", "british", "wootton", "ing", "romero", "farina", "authors", "tion", "np", "found")) %>%
  filter(!word %in% c("psu", "lizaso", "torquemada", "romero", "pérez", "term", "rates")) %>% 
  anti_join(stop_words) %>%
  count(word) %>% 
  arrange(desc(n)) %>% 
  # print(n = 100)
  filter(n>=7)
  # filter(n>=10)

wordcloud2(PagesFunEcol2012[,1:2], fontFamily = "arial", 
           fontWeight ="normal", color = pal_aaas(palette = "default")(10), 
           minRotation = 0, maxRotation = 0,
           size = 0.7)


# 2013_Gera et al. JEcology.pdf
GeraJEcol2013 <- cleancorpus %>%
  filter(paper == "2013_Gera et al. JEcology.pdf") %>% 
  # filter(paper %in% c("2012_Pagès et al. FuncEcol.pdf",
                      # "2010_Pagès et al. JEMBE.pdf",
                      # "2013_Gera et al. JEcology.pdf")) %>% 
  filter(!word %in% c("ns", "sala", "british", "wootton", "ing", "romero", "farina", "authors", "tion", "np")) %>%
  filter(!word %in% c("psu", "lizaso", "torquemada", "romero", "pérez", "term", "rates")) %>%
  filter(!word %in% c("st", "journal", "prado", "nsc", "alcoverro", "authors", "planes", "british", "df", "found")) %>%
  anti_join(stop_words) %>%
  count(word) %>% 
  arrange(desc(n)) %>% 
  # print(n = 100)
  filter(n>=7)

wordcloud2(GeraJEcol2013[,1:2], fontFamily = "arial", 
           fontWeight ="normal", color = pal_aaas(palette = "default")(10), 
           minRotation = 0, maxRotation = 0,
           size = 0.7)


# 2013_Pagès et al. MEPS.pdf
PagesMEPS2013 <- cleancorpus %>%
  filter(paper == "2013_Pagès et al. MEPS.pdf") %>% 
  # filter(paper %in% c("2012_Pagès et al. FuncEcol.pdf",
  #                     "2010_Pagès et al. JEMBE.pdf",
  #                     "2013_Gera et al. JEcology.pdf",
  #                     "2013_Pagès et al. MEPS.pdf")) %>% 
  filter(!word %in% c("ns", "sala", "british", "wootton", "ing", "romero", "farina", "authors", "tion", "np")) %>%
  filter(!word %in% c("psu", "lizaso", "torquemada", "romero", "pérez", "term", "rates")) %>%
  filter(!word %in% c("st", "journal", "prado", "nsc", "alcoverro", "authors", "planes", "british", "df", "ecol", "mar", "found")) %>%
  anti_join(stop_words) %>%
  count(word) %>% 
  arrange(desc(n)) %>% 
  # print(n = 100)
  filter(n>=9) 

wordcloud2(PagesMEPS2013[,1:2], fontFamily = "arial", 
           fontWeight ="normal", color = pal_aaas(palette = "default")(10), 
           minRotation = 0, maxRotation = 0,
           size = 0.7)


#2013_Pagès et al. PlosOne.pdf
PagesPlos2013 <- cleancorpus %>%
  filter(paper == "2013_Pagès et al. PlosOne.pdf") %>% 
  # filter(paper %in% c("2012_Pagès et al. FuncEcol.pdf",
  #                     "2010_Pagès et al. JEMBE.pdf",
  #                     "2013_Gera et al. JEcology.pdf",
  #                     "2013_Pagès et al. MEPS.pdf",
  #                     "2013_Pagès et al. PlosOne.pdf")) %>% 
  filter(!word %in% c("ns", "sala", "british", "wootton", "ing", "romero", "farina", "authors", "tion", "np")) %>%
  filter(!word %in% c("psu", "lizaso", "torquemada", "romero", "pérez", "term", "rates")) %>%
  filter(!word %in% c("st", "journal", "prado", "nsc", "alcoverro", "authors", "planes", "british", "df", "ecol", "mar", "found")) %>%
  anti_join(stop_words) %>%
  count(word) %>% 
  arrange(desc(n)) %>% 
  # print(n = 100)
  filter(n>=8)  

wordcloud2(PagesPlos2013[,1:2], fontFamily = "arial", 
           fontWeight ="normal", color = pal_aaas(palette = "default")(10), 
           minRotation = 0, maxRotation = 0,
           size = 0.7)


#2014_Gera et al. LimnolOcean.pdf
GeraLimno2014 <- cleancorpus %>%
  filter(paper == "2014_Gera et al. LimnolOcean.pdf") %>% 
  # filter(paper %in% c("2012_Pagès et al. FuncEcol.pdf",
  #                     "2010_Pagès et al. JEMBE.pdf",
  #                     "2013_Gera et al. JEcology.pdf",
  #                     "2013_Pagès et al. MEPS.pdf",
  #                     "2013_Pagès et al. PlosOne.pdf",
  #                     "2014_Gera et al. LimnolOcean.pdf")) %>% 
  filter(!word %in% c("ns", "sala", "british", "wootton", "ing", "romero", "farina", "authors", "tion", "np")) %>%
  filter(!word %in% c("psu", "lizaso", "torquemada", "romero", "pérez", "term", "rates")) %>%
  filter(!word %in% c("st", "journal", "prado", "nsc", "alcoverro", "authors", "planes", "british", "df", "ecol", "mar", "found")) %>%
  anti_join(stop_words) %>%
  count(word) %>% 
  arrange(desc(n)) %>% 
  # print(n = 100)
  filter(n>=6)  

wordcloud2(GeraLimno2014[,1:2], fontFamily = "arial", 
           fontWeight ="normal", color = pal_aaas(palette = "default")(10), 
           minRotation = 0, maxRotation = 0,
           size = 0.7)


# 2014_Pagès et al. FuncEcol.pdf
PagesFunEcol2014 <- cleancorpus %>%
  filter(paper == "2014_Pagès et al. FuncEcol.pdf") %>% 
  # filter(paper %in% c("2012_Pagès et al. FuncEcol.pdf",
  #                     "2010_Pagès et al. JEMBE.pdf",
  #                     "2013_Gera et al. JEcology.pdf",
  #                     "2013_Pagès et al. MEPS.pdf",
  #                     "2013_Pagès et al. PlosOne.pdf",
  #                     "2014_Gera et al. LimnolOcean.pdf",
  #                     "2014_Pagès et al. FuncEcol.pdf")) %>% 
  filter(!word %in% c("ns", "sala", "british", "wootton", "ing", "romero", "farina", "authors", "tion", "np")) %>%
  filter(!word %in% c("psu", "lizaso", "torquemada", "romero", "pérez", "term", "rates")) %>%
  filter(!word %in% c("st", "journal", "prado", "nsc", "alcoverro", "authors", "planes", "british", "df", "ecol", "mar", "found")) %>%
  anti_join(stop_words) %>%
  count(word) %>% 
  arrange(desc(n)) %>% 
  # print(n = 100)
  filter(n>=10)  

wordcloud2(PagesFunEcol2014[,1:2], fontFamily = "arial", 
           fontWeight ="normal", color = pal_aaas(palette = "default")(10), 
           minRotation = 0, maxRotation = 0,
           size = 0.9)


# 2015_Jahnke et al. JEcology.pdf
JahnkeJEcol2015 <- cleancorpus %>%
  filter(paper == "2015_Jahnke et al. JEcology.pdf") %>% 
  # filter(paper %in% c("2012_Pagès et al. FuncEcol.pdf",
  #                     "2010_Pagès et al. JEMBE.pdf",
  #                     "2013_Gera et al. JEcology.pdf",
  #                     "2013_Pagès et al. MEPS.pdf",
  #                     "2013_Pagès et al. PlosOne.pdf",
  #                     "2014_Gera et al. LimnolOcean.pdf",
  #                     "2014_Pagès et al. FuncEcol.pdf",
  #                     "2015_Jahnke et al. JEcology.pdf")) %>% 
  filter(!word %in% c("ns", "sala", "british", "wootton", "ing", "romero", "farina", "authors", "tion", "np")) %>%
  filter(!word %in% c("psu", "lizaso", "torquemada", "romero", "pérez", "term", "rates")) %>%
  filter(!word %in% c("st", "journal", "prado", "nsc", "alcoverro", "authors", "planes", "british", "df", "ecol", "mar", "found")) %>%
  anti_join(stop_words) %>%
  count(word) %>% 
  arrange(desc(n)) %>% 
  # print(n = 100)
  filter(n>=8)  

wordcloud2(JahnkeJEcol2015[,1:2], fontFamily = "arial", 
           fontWeight ="normal", color = pal_aaas(palette = "default")(10), 
           minRotation = 0, maxRotation = 0,
           size = 0.9)


# 2016_Bakker et al. Ecography.pdf
BakkerEcogr2016 <- cleancorpus %>%
  filter(paper == "2016_Bakker et al. Ecography.pdf") %>% 
  # filter(paper %in% c("2012_Pagès et al. FuncEcol.pdf",
  #                     "2010_Pagès et al. JEMBE.pdf",
  #                     "2013_Gera et al. JEcology.pdf",
  #                     "2013_Pagès et al. MEPS.pdf",
  #                     "2013_Pagès et al. PlosOne.pdf",
  #                     "2014_Gera et al. LimnolOcean.pdf",
  #                     "2014_Pagès et al. FuncEcol.pdf",
  #                     "2015_Jahnke et al. JEcology.pdf",
  #                     "2016_Bakker et al. Ecography.pdf")) %>% 
  filter(!word %in% c("ns", "sala", "british", "wootton", "ing", "romero", "farina", "authors", "tion", "np")) %>%
  filter(!word %in% c("psu", "lizaso", "torquemada", "romero", "pérez", "term", "rates")) %>%
  filter(!word %in% c("st", "journal", "prado", "nsc", "alcoverro", "authors", "planes", "british", "df", "ecol", "mar", "found")) %>%
  anti_join(stop_words) %>%
  count(word) %>% 
  arrange(desc(n)) %>% 
  # print(n = 100)
  filter(n>=11)  

wordcloud2(BakkerEcogr2016[,1:2], fontFamily = "arial", 
           fontWeight ="normal", color = pal_aaas(palette = "default")(10), 
           minRotation = 0, maxRotation = 0,
           size = 1)



# 2018_Pagès et al. MarinePollution.pdf
PagesMarPol2018 <- cleancorpus %>%
  filter(paper == "2018_Pagès et al. MarinePollution.pdf") %>% 
  # filter(paper %in% c("2012_Pagès et al. FuncEcol.pdf",
  #                     "2010_Pagès et al. JEMBE.pdf",
  #                     "2013_Gera et al. JEcology.pdf",
  #                     "2013_Pagès et al. MEPS.pdf",
  #                     "2013_Pagès et al. PlosOne.pdf",
  #                     "2014_Gera et al. LimnolOcean.pdf",
  #                     "2014_Pagès et al. FuncEcol.pdf",
  #                     "2015_Jahnke et al. JEcology.pdf",
  #                     "2016_Bakker et al. Ecography.pdf",
  #                     "2018_Pagès et al. MarinePollution.pdf")) %>% 
  filter(!word %in% c("ns", "sala", "british", "wootton", "ing", "romero", "farina", "authors", "tion", "np")) %>%
  filter(!word %in% c("psu", "lizaso", "torquemada", "romero", "pérez", "term", "rates")) %>%
  filter(!word %in% c("st", "journal", "prado", "nsc", "alcoverro", "authors", "planes", "british", "df", "ecol", "mar", "found")) %>%
  anti_join(stop_words) %>%
  count(word) %>% 
  arrange(desc(n)) %>% 
  # print(n = 100)
  filter(n>=8)  

wordcloud2(PagesMarPol2018[,1:2], fontFamily = "arial", 
           fontWeight ="normal", color = pal_aaas(palette = "default")(10), 
           minRotation = 0, maxRotation = 0,
           size = 0.7)


# 2019_Ladd et al. GRL.pdff
LaddGRL2019 <- cleancorpus %>%
  filter(paper == "2019_Ladd et al. GRL.pdf") %>% 
  # filter(paper %in% c("2012_Pagès et al. FuncEcol.pdf",
  #                     "2010_Pagès et al. JEMBE.pdf",
  #                     "2013_Gera et al. JEcology.pdf",
  #                     "2013_Pagès et al. MEPS.pdf",
  #                     "2013_Pagès et al. PlosOne.pdf",
  #                     "2014_Gera et al. LimnolOcean.pdf",
  #                     "2014_Pagès et al. FuncEcol.pdf",
  #                     "2015_Jahnke et al. JEcology.pdf",
  #                     "2016_Bakker et al. Ecography.pdf",
  #                     "2018_Pagès et al. MarinePollution.pdf",
  #                     "2019_Ladd et al. GRL.pdf")) %>% 
  filter(!word %in% c("ns", "sala", "british", "wootton", "ing", "romero", "farina", "authors", "tion", "np")) %>%
  filter(!word %in% c("psu", "lizaso", "torquemada", "romero", "pérez", "term", "rates")) %>%
  filter(!word %in% c("st", "journal", "prado", "nsc", "alcoverro", "authors", "planes", "british", "df", "ecol", "mar", "found")) %>%
  anti_join(stop_words) %>%
  count(word) %>% 
  arrange(desc(n)) %>% 
  # print(n = 100)
  filter(n>=6)  

wordcloud2(LaddGRL2019[,1:2], fontFamily = "arial", 
           fontWeight ="normal", color = pal_aaas(palette = "default")(10), 
           minRotation = 0, maxRotation = 0,
           size = 1.3)


# 2019_McKinley et al. EcoServices.pdf
McKinleyEcoserv2019 <- cleancorpus %>%
  filter(paper == "2019_McKinley et al. EcoServices.pdf") %>% 
  # filter(paper %in% c("2012_Pagès et al. FuncEcol.pdf",
  #                     "2010_Pagès et al. JEMBE.pdf",
  #                     "2013_Gera et al. JEcology.pdf",
  #                     "2013_Pagès et al. MEPS.pdf",
  #                     "2013_Pagès et al. PlosOne.pdf",
  #                     "2014_Gera et al. LimnolOcean.pdf",
  #                     "2014_Pagès et al. FuncEcol.pdf",
  #                     "2015_Jahnke et al. JEcology.pdf",
  #                     "2016_Bakker et al. Ecography.pdf",
  #                     "2018_Pagès et al. MarinePollution.pdf",
  #                     "2019_Ladd et al. GRL.pdf",
  #                     "2019_McKinley et al. EcoServices.pdf")) %>% 
  filter(!word %in% c("ns", "sala", "british", "wootton", "ing", "romero", "farina", "authors", "tion", "np")) %>%
  filter(!word %in% c("psu", "lizaso", "torquemada", "romero", "pérez", "rates")) %>%
  filter(!word %in% c("st", "journal", "prado", "nsc", "alcoverro", "authors", "planes", "british", "df", "ecol", "mar", "found", "term")) %>%
  anti_join(stop_words) %>%
  count(word) %>% 
  arrange(desc(n)) %>% 
  # print(n = 100)
  filter(n>=10)  

wordcloud2(McKinleyEcoserv2019[,1:2], fontFamily = "arial", 
           fontWeight ="normal", color = pal_aaas(palette = "default")(10), 
           minRotation = 0, maxRotation = 0,
           size = 0.9)


# 2019_Pagès et al. Ecosystems.pdf
PagesEcosyst2019 <- cleancorpus %>%
  filter(paper == "2019_Pagès et al. Ecosystems.pdf") %>% 
  # filter(paper %in% c("2012_Pagès et al. FuncEcol.pdf",
  #                     "2010_Pagès et al. JEMBE.pdf",
  #                     "2013_Gera et al. JEcology.pdf",
  #                     "2013_Pagès et al. MEPS.pdf",
  #                     "2013_Pagès et al. PlosOne.pdf",
  #                     "2014_Gera et al. LimnolOcean.pdf",
  #                     "2014_Pagès et al. FuncEcol.pdf",
  #                     "2015_Jahnke et al. JEcology.pdf",
  #                     "2016_Bakker et al. Ecography.pdf",
  #                     "2018_Pagès et al. MarinePollution.pdf",
  #                     "2019_Ladd et al. GRL.pdf",
  #                     "2019_McKinley et al. EcoServices.pdf",
  #                     "2019_Pagès et al. Ecosystems.pdf")) %>% 
  filter(!word %in% c("ns", "sala", "british", "wootton", "ing", "romero", "farina", "authors", "tion", "np")) %>%
  filter(!word %in% c("psu", "lizaso", "torquemada", "romero", "pérez", "rates")) %>%
  filter(!word %in% c("st", "journal", "prado", "nsc", "alcoverro", "authors", "planes", "british", "df", "ecol", "mar", "found", "term")) %>%
  anti_join(stop_words) %>%
  count(word) %>% 
  arrange(desc(n)) %>% 
  # print(n = 100)
  filter(n>=8)  

wordcloud2(PagesEcosyst2019[,1:2], fontFamily = "arial", 
           fontWeight ="normal", color = pal_aaas(palette = "default")(10), 
           minRotation = 0, maxRotation = 0,
           size = 1.3)


# 2020_Duggan-Edwards_et_al_JAppEcol.pdf
DugganJAppEco2020 <- cleancorpus %>%
  filter(paper == "2020_Duggan-Edwards_et_al_JAppEcol.pdf") %>% 
  # filter(paper %in% c("2012_Pagès et al. FuncEcol.pdf",
  #                     "2010_Pagès et al. JEMBE.pdf",
  #                     "2013_Gera et al. JEcology.pdf",
  #                     "2013_Pagès et al. MEPS.pdf",
  #                     "2013_Pagès et al. PlosOne.pdf",
  #                     "2014_Gera et al. LimnolOcean.pdf",
  #                     "2014_Pagès et al. FuncEcol.pdf",
  #                     "2015_Jahnke et al. JEcology.pdf",
  #                     "2016_Bakker et al. Ecography.pdf",
  #                     "2018_Pagès et al. MarinePollution.pdf",
  #                     "2019_Ladd et al. GRL.pdf",
  #                     "2019_McKinley et al. EcoServices.pdf",
  #                     "2019_Pagès et al. Ecosystems.pdf",
  #                     "2020_Duggan-Edwards_et_al_JAppEcol.pdf")) %>% 
  filter(!word %in% c("ns", "sala", "british", "wootton", "ing", "romero", "farina", "authors", "tion", "np")) %>%
  filter(!word %in% c("van", "bouma", "herman", "silliman", "bertness", "wesenbeeck")) %>%
  filter(!word %in% c("st", "journal", "prado", "nsc", "alcoverro", "authors", "planes", "british", "df", "ecol", "mar", "found", "term")) %>%
  anti_join(stop_words) %>%
  count(word) %>% 
  arrange(desc(n)) %>% 
  # print(n = 100)
  filter(n>=9)  

wordcloud2(DugganJAppEco2020[,1:2], fontFamily = "arial", 
           fontWeight ="normal", color = pal_aaas(palette = "default")(10), 
           minRotation = 0, maxRotation = 0,
           size = 0.9)


# 2020_McKinley_et_al_ECSS.pdf
McKinleyECSS2020 <- cleancorpus %>%
  filter(paper == "2020_McKinley_et_al_ECSS.pdf") %>% 
  # filter(paper %in% c("2012_Pagès et al. FuncEcol.pdf",
  #                     "2010_Pagès et al. JEMBE.pdf",
  #                     "2013_Gera et al. JEcology.pdf",
  #                     "2013_Pagès et al. MEPS.pdf",
  #                     "2013_Pagès et al. PlosOne.pdf",
  #                     "2014_Gera et al. LimnolOcean.pdf",
  #                     "2014_Pagès et al. FuncEcol.pdf",
  #                     "2015_Jahnke et al. JEcology.pdf",
  #                     "2016_Bakker et al. Ecography.pdf",
  #                     "2018_Pagès et al. MarinePollution.pdf",
  #                     "2019_Ladd et al. GRL.pdf",
  #                     "2019_McKinley et al. EcoServices.pdf",
  #                     "2019_Pagès et al. Ecosystems.pdf",
  #                     "2020_Duggan-Edwards_et_al_JAppEcol.pdf",
  #                     "2020_McKinley_et_al_ECSS.pdf")) %>% 
  filter(!word %in% c("ns", "sala", "british", "wootton", "ing", "romero", "farina", "authors", "tion", "np")) %>%
  filter(!word %in% c("psu", "lizaso", "torquemada", "romero", "pérez", "rates")) %>%
  filter(!word %in% c("st", "journal", "prado", "nsc", "alcoverro", "authors", "planes", "british", "df", "ecol", "mar", "found", "term")) %>%
  anti_join(stop_words) %>%
  count(word) %>% 
  arrange(desc(n)) %>% 
  # print(n = 100)
  filter(n>=12)  

wordcloud2(McKinleyECSS2020[,1:2], fontFamily = "arial", 
           fontWeight ="normal", color = pal_aaas(palette = "default")(10), 
           minRotation = 0, maxRotation = 0,
           size = 0.9)


# 2020_McKinley_et_al_OceanCoastMgment.pdf
McKinleyOCM2020 <- cleancorpus %>%
  filter(paper == "2020_McKinley_et_al_OceanCoastMgment.pdf") %>% 
  # filter(paper %in% c("2012_Pagès et al. FuncEcol.pdf",
  #                     "2010_Pagès et al. JEMBE.pdf",
  #                     "2013_Gera et al. JEcology.pdf",
  #                     "2013_Pagès et al. MEPS.pdf",
  #                     "2013_Pagès et al. PlosOne.pdf",
  #                     "2014_Gera et al. LimnolOcean.pdf",
  #                     "2014_Pagès et al. FuncEcol.pdf",
  #                     "2015_Jahnke et al. JEcology.pdf",
  #                     "2016_Bakker et al. Ecography.pdf",
  #                     "2018_Pagès et al. MarinePollution.pdf",
  #                     "2019_Ladd et al. GRL.pdf",
  #                     "2019_McKinley et al. EcoServices.pdf",
  #                     "2019_Pagès et al. Ecosystems.pdf",
  #                     "2020_Duggan-Edwards_et_al_JAppEcol.pdf",
  #                     "2020_McKinley_et_al_ECSS.pdf",
  #                     "2020_McKinley_et_al_OceanCoastMgment.pdf")) %>% 
  filter(!word %in% c("ns", "sala", "british", "wootton", "ing", "romero", "farina", "authors", "tion", "np")) %>%
  filter(!word %in% c("psu", "lizaso", "torquemada", "romero", "pérez", "rates")) %>%
  filter(!word %in% c("st", "journal", "prado", "nsc", "alcoverro", "authors", "planes", "british", "df", "ecol", "mar", "found", "term")) %>%
  anti_join(stop_words) %>%
  count(word) %>% 
  arrange(desc(n)) %>% 
  # print(n = 100)
  filter(n>=9)  

wordcloud2(McKinleyOCM2020[,1:2], fontFamily = "arial", 
           fontWeight ="normal", color = pal_aaas(palette = "default")(10), 
           minRotation = 0, maxRotation = 0,
           size = 0.9)


# 2021_Ladd_FrontMarSci.pdf
LaddFrontMarSci2021 <- cleancorpus %>%
  filter(paper == "2021_Ladd_FrontMarSci.pdf") %>% 
  # filter(paper %in% c("2012_Pagès et al. FuncEcol.pdf",
  #                     "2010_Pagès et al. JEMBE.pdf",
  #                     "2013_Gera et al. JEcology.pdf",
  #                     "2013_Pagès et al. MEPS.pdf",
  #                     "2013_Pagès et al. PlosOne.pdf",
  #                     "2014_Gera et al. LimnolOcean.pdf",
  #                     "2014_Pagès et al. FuncEcol.pdf",
  #                     "2015_Jahnke et al. JEcology.pdf",
  #                     "2016_Bakker et al. Ecography.pdf",
  #                     "2018_Pagès et al. MarinePollution.pdf",
  #                     "2019_Ladd et al. GRL.pdf",
  #                     "2019_McKinley et al. EcoServices.pdf",
  #                     "2019_Pagès et al. Ecosystems.pdf",
  #                     "2020_Duggan-Edwards_et_al_JAppEcol.pdf",
  #                     "2020_McKinley_et_al_ECSS.pdf",
  #                     "2020_McKinley_et_al_OceanCoastMgment.pdf",
  #                     "2021_Ladd_FrontMarSci.pdf")) %>% 
  filter(!word %in% c("ns", "sala", "british", "wootton", "ing", "romero", "farina", "authors", "tion", "np")) %>%
  filter(!word %in% c("psu", "lizaso", "torquemada", "romero", "pérez", "rates")) %>%
  filter(!word %in% c("st", "journal", "prado", "nsc", "alcoverro", "authors", "planes", "british", "df", "ecol", "mar", "found", "term")) %>%
  anti_join(stop_words) %>%
  count(word) %>% 
  arrange(desc(n)) %>% 
  # print(n = 100)
  filter(n>=10)  

wordcloud2(LaddFrontMarSci2021[,1:2], fontFamily = "arial", 
           fontWeight ="normal", color = pal_aaas(palette = "default")(10), 
           minRotation = 0, maxRotation = 0,
           size = 1.2)


# 2021_Pagès_et_al_MoveEcol.pdf
PagesMoveEcol2021 <- cleancorpus %>%
  filter(paper == "2021_Pagès_et_al_MoveEcol.pdf") %>% 
  # filter(paper %in% c("2012_Pagès et al. FuncEcol.pdf",
  #                     "2010_Pagès et al. JEMBE.pdf",
  #                     "2013_Gera et al. JEcology.pdf",
  #                     "2013_Pagès et al. MEPS.pdf",
  #                     "2013_Pagès et al. PlosOne.pdf",
  #                     "2014_Gera et al. LimnolOcean.pdf",
  #                     "2014_Pagès et al. FuncEcol.pdf",
  #                     "2015_Jahnke et al. JEcology.pdf",
  #                     "2016_Bakker et al. Ecography.pdf",
  #                     "2018_Pagès et al. MarinePollution.pdf",
  #                     "2019_Ladd et al. GRL.pdf",
  #                     "2019_McKinley et al. EcoServices.pdf",
  #                     "2019_Pagès et al. Ecosystems.pdf",
  #                     "2020_Duggan-Edwards_et_al_JAppEcol.pdf",
  #                     "2020_McKinley_et_al_ECSS.pdf",
  #                     "2020_McKinley_et_al_OceanCoastMgment.pdf",
  #                     "2021_Ladd_FrontMarSci.pdf",
  #                     "2021_Pagès_et_al_MoveEcol.pdf")) %>% 
  filter(!word %in% c("ns", "sala", "british", "wootton", "ing", "romero", "farina", "authors", "tion", "np")) %>%
  filter(!word %in% c("psu", "lizaso", "torquemada", "romero", "pérez", "rates")) %>%
  filter(!word %in% c("st", "journal", "prado", "nsc", "alcoverro", "authors", "planes", "british", "df", "ecol", "mar", "found", "term")) %>%
  anti_join(stop_words) %>%
  count(word) %>% 
  arrange(desc(n)) %>% 
  # print(n = 100)
  filter(n>=10)  

wordcloud2(PagesMoveEcol2021[,1:2], fontFamily = "arial", 
           fontWeight ="normal", color = pal_aaas(palette = "default")(10), 
           minRotation = 0, maxRotation = 0,
           size = 1)


# MSCA
PagesMSCA2021 <- cleancorpus %>%
  filter(paper == "Part B1_ok.pdf") %>% 
  # filter(paper %in% c("2012_Pagès et al. FuncEcol.pdf",
  #                     "2010_Pagès et al. JEMBE.pdf",
  #                     "2013_Gera et al. JEcology.pdf",
  #                     "2013_Pagès et al. MEPS.pdf",
  #                     "2013_Pagès et al. PlosOne.pdf",
  #                     "2014_Gera et al. LimnolOcean.pdf",
  #                     "2014_Pagès et al. FuncEcol.pdf",
  #                     "2015_Jahnke et al. JEcology.pdf",
  #                     "2016_Bakker et al. Ecography.pdf",
  #                     "2018_Pagès et al. MarinePollution.pdf",
  #                     "2019_Ladd et al. GRL.pdf",
  #                     "2019_McKinley et al. EcoServices.pdf",
  #                     "2019_Pagès et al. Ecosystems.pdf",
  #                     "2020_Duggan-Edwards_et_al_JAppEcol.pdf",
  #                     "2020_McKinley_et_al_ECSS.pdf",
  #                     "2020_McKinley_et_al_OceanCoastMgment.pdf",
  #                     "2021_Ladd_FrontMarSci.pdf",
  #                     "2021_Pagès_et_al_MoveEcol.pdf",
  #                     "Part B1_ok.pdf")) %>% 
  filter(!word %in% c("eu", "sala", "british", "wootton", "ing", "romero", "ef", "authors", "tion", "sec")) %>%
  filter(!word %in% c("wp", "ub", "forepast", "csic", "ii", "dr")) %>%
  filter(!word %in% c("st", "journal", "prado", "nsc", "irta", "authors", "planes", "british", "df", "ecol", "mar", "found", "term")) %>%
  anti_join(stop_words) %>%
  count(word) %>% 
  arrange(desc(n)) %>% 
  # print(n = 100)
  filter(n>=12)  

wordcloud2(PagesMSCA2021[,1:2], fontFamily = "arial", 
           fontWeight ="normal", color = pal_aaas(palette = "default")(10), 
           minRotation = 0, maxRotation = 0,
           size = 1)


# Final
final <- cleancorpus %>%
  filter(!word %in% c("ns", "sala", "british", "wootton", "ing", "romero", "farina", "authors", "tion", "np")) %>%
  filter(!word %in% c("psu", "lizaso", "torquemada", "romero", "pérez", "rates")) %>%
  filter(!word %in% c("st", "journal", "prado", "nsc", "alcoverro", "authors", "planes", "british", "df", "ecol", "mar", "found", "term")) %>%
  anti_join(stop_words) %>%
  count(word) %>% 
  arrange(desc(n)) %>% 
  # print(n = 100)
  filter(n>=160) #%>% 
  # mutate(colour = c(pal_aaas(palette = "default")(9), rep("#000000", 61)))

wordcloud2(final[,1:2], fontFamily = "arial", 
           fontWeight ="normal", color = pal_aaas(palette = "default")(10), 
           minRotation = 0, maxRotation = 0,
           size = 0.7)
# 
# ggplot(final, aes(label = word, size = n)) +
#   geom_text_wordcloud() +
#   scale_color_aaas(palette = "default")+
#   scale_radius(range = c(0, 10), limits = c(0, NA)) +
#   theme_minimal()
# 
# ggplot(data = final, aes(label = word, size = n, colour = word)) + 
#   geom_text_wordcloud(rm_outside = TRUE, max_steps = 1,
#                       grid_size = 1, eccentricity = .9)+
#   # scale_size_area(max_size = 14)+
#   # scale_color_aaas(palette = "default")+
#   scale_radius(range = c(0, 11), limits = c(0, NA)) +
#   scale_color_manual(values = c("sea" = "#3B4992FF", 
#                                 "species" = "#EE0000FF",
#                                 "plant" = "#008B45FF",
#                                 "ecosystem" = "#631879FF",
#                                 "seagrass" = "#008280FF",
#                                 "herbivores" = "#BB0021FF", 
#                                 "size" = "#5F559BFF",
#                                 "marsh" = "#A20056FF",
#                                 "urchin" = "#808180FF",
#                                 "herbivory" = "#1B1919FF")) +
#   # scale_colour_manual(values = rev(c(pal_aaas(palette = "default")(10), rep("#000000", 150)))) +
#   theme_void()
# # ggsave("prova.jpg")
