library(stm)
library(jiebaR)
library(dplyr)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textmodels)
library(sysfonts)
library(showtext)
library(furrr)
library(ggplot2)
library(tidyverse)
library(tidytext)
library(stminsights)
library(igraph)
library(ggraph)
showtext_auto(enable = TRUE)

#1. Document-feature Matrix Building
#> Read csv file
df <- read.csv(file = 'stm_leavehomesafe.csv')
df$index<-seq(1:nrow(df))

#> Build the corpus
jieba_tokens <- corpus(df, docid_field = "index", 
                       text_field = "tokenz") %>%
  tokenizers::tokenize_regex(pattern = " ") %>%
  tokens()

docvars(jieba_tokens, "day") <- as.numeric(df$day)
docvars(jieba_tokens, "stance") <- as.character(df$stance)
docvars(jieba_tokens, "sentiment") <- as.character(df$sentiment)
docvars(jieba_tokens, "thread_title") <- as.character(df$thread_title)

#> Build document-feature matrix
jieba_dfm <- dfm(jieba_tokens)
jieba_dfm

#> Data trimming
dfm_trim <- dfm_trim (jieba_dfm, min_docfreq = 0.003, max_docfreq = 0.99, 
                      docfreq_type = "prop", verbose = TRUE)
dfm_trim

topfeatures(dfm_trim, n = 20, scheme = "docfreq")

stmdfm <- convert(dfm_trim, to = "stm", docvars = docvars(jieba_tokens))

out <- list(documents = stmdfm$documents,
            vocab = stmdfm$vocab,
            meta = stmdfm$meta)

# 2. Model Selection
#> Try several different models and compare performance
many_models <- tibble(K = ((1:10)*5)) %>%
  mutate(topic_model = future_map(K, ~stm(out$documents, out$vocab, K = .,
                                          data = out$meta, init.type = "Spectral",
                                          seed = 2022, prevalence = ~stance + s(day) + sentiment)))
many_models

heldout <- make.heldout(dfm_trim)

k_result <- many_models %>%
  mutate(exclusivity = map(topic_model, exclusivity),
         semantic_coherence = map(topic_model, semanticCoherence, dfm_trim),
         eval_heldout = map(topic_model, eval.heldout, heldout$missing),
         residual = map(topic_model, checkResiduals, dfm_trim),
         bound = map_dbl(topic_model, function(x) max(x$convergence$bound)),
         lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
         lbound = bound + lfact,
         iterations = map_dbl(topic_model, function(x) length(x$convergence$bound)))
k_result

#> Model diagnostics by number of topics
k_result %>%
  transmute(K,
            `Lower bound` = lbound,
            Residuals = map_dbl(residual, "dispersion"),
            `Semantic coherence` = map_dbl(semantic_coherence, mean),
            `Held-out likelihood` = map_dbl(eval_heldout, "expected.heldout")) %>%
  gather(Metric, Value, -K) %>%
  ggplot(aes(K, Value, color = Metric)) +
  geom_line(size = 1.5, alpha = 0.7, show.legend = FALSE) +
  facet_wrap(~Metric, scales = "free_y") +
  labs(x = "K (number of topics)",
       y = NULL,
       title = "Model diagnostics by number of topics",
       subtitle = "")

#>  Comparing exclusivity and semantic coherence
k_result %>%
  unnest(c(exclusivity, semantic_coherence)) %>% 
  filter(K %in%  ((1:10)*5)) %>%
  group_by(K) %>% 
  summarize(exclusivity = mean(exclusivity),
            semantic_coherence = mean(semantic_coherence)) %>% 
  mutate(K = as.factor(K)) %>%
  ggplot(aes(x = semantic_coherence, y = exclusivity, color = as.factor(K))) +
  geom_point(size = 1, alpha = 0.7, show.legend = FALSE) +
  ggrepel::geom_text_repel(
    aes(label = as.factor(K)),
    size = 5,
    hjust = 0, 
    show.legend = FALSE
  )+
  labs(x = "Semantic Coherence (mean)",
       y = "Exclusivity (mean)",
       title = "Comparing exclusivity and semantic coherence",
       subtitle = "")

matrix <-
  k_result %>%
  unnest(c(exclusivity, semantic_coherence)) %>% 
  group_by(K) %>% 
  summarize(exclusivity = mean(exclusivity),
            semantic_coherence = mean(semantic_coherence))
write.table(matrix , file = "model_matrix.csv", sep=",", row.names=FALSE)
